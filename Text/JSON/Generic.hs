{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}

-- | JSON serializer and deserializer using Data.Generics.
-- The functions here handle algebraic data types and primitive types.
-- It uses the same representation as "Text.JSON" for "Prelude" types.
module Text.JSON.Generic
  ( module Text.JSON,
    Data,
    Typeable,
    toJSON,
    fromJSON,
    encodeJSON,
    decodeJSON,
  )
where

import Control.Monad.State
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Generics
import Data.Int
import qualified Data.IntSet as I
import Data.Time.Clock (UTCTime)
import Data.Word
import Text.JSON
import Text.JSON.String (parseJSValue)
import Text.ParserCombinators.Parsec (parse)

type T a = a -> JSValue

-- | Convert anything to a JSON value.
toJSON :: (Data a) => a -> JSValue
toJSON =
  toJSONGeneric
    `ext1Q` jList
    `extQ` (showJSON :: T Integer)
    `extQ` (showJSON :: T Int)
    `extQ` (showJSON :: T Word8)
    `extQ` (showJSON :: T Word16)
    `extQ` (showJSON :: T Word32)
    `extQ` (showJSON :: T Word64)
    `extQ` (showJSON :: T Int8)
    `extQ` (showJSON :: T Int16)
    `extQ` (showJSON :: T Int32)
    `extQ` (showJSON :: T Int64)
    `extQ` (showJSON :: T Double)
    `extQ` (showJSON :: T Float)
    `extQ` (showJSON :: T Char)
    `extQ` (showJSON :: T String)
    -- Bool has a special encoding.
    `extQ` (showJSON :: T Bool)
    `extQ` (showJSON :: T ())
    `extQ` (showJSON :: T Ordering)
    -- More special cases.
    `extQ` (showJSON :: T I.IntSet)
    `extQ` (showJSON :: T UTCTime)
    `extQ` (showJSON :: T S.ByteString)
    `extQ` (showJSON :: T L.ByteString)
  where
    -- Lists are simply coded as arrays.
    jList vs = JSArray $ map toJSON vs

toJSONGeneric :: (Data a) => a -> JSValue
toJSONGeneric = generic
  where
    -- Generic encoding of an algebraic data type.
    --   No constructor, so it must be an error value.  Code it anyway as JSNull.
    --   Elide a single constructor and just code the arguments.
    --   For multiple constructors, make an object with a field name that is the
    --   constructor (except lower case) and the data is the arguments encoded.
    generic a =
      case dataTypeRep (dataTypeOf a) of
        AlgRep [] -> JSNull
        AlgRep [c] -> encodeArgs c (toJSON `gmapQ` a)
        AlgRep _ -> encodeConstr (toConstr a) (toJSON `gmapQ` a)
        rep -> error $ "toJSON: not AlgRep " ++ show rep ++ "(" ++ show (dataTypeOf a) ++ ")"

    -- Encode nullary constructor as a string.
    -- Encode non-nullary constructors as an object with the constructor
    -- name as the single field and the arguments as the value.
    -- Use an array if the are no field names, but elide singleton arrays,
    -- and use an object if there are field names.
    encodeConstr constr [] = JSString $ toJSString $ showConstr constr
    encodeConstr constr argList = jsObject [(showConstr constr, encodeArgs constr argList)]

    encodeArgs constr argList = encodeArgs' (constrFields constr) argList
      where
        encodeArgs' [] [j] = j
        encodeArgs' [] js = JSArray js
        encodeArgs' ns js = jsObject $ zip (map mungeField ns) js

    -- Skip leading '_' in field name so we can use keywords etc. as field names.
    mungeField ('_' : cs) = cs
    mungeField cs = cs

    jsObject :: [(String, JSValue)] -> JSValue
    jsObject = JSObject . toJSObject

type F a = Result a

-- | Convert a JSON value to anything (fails if the types do not match).
fromJSON :: (Data a) => JSValue -> Result a
fromJSON j =
  fromJSONGeneric j
    `ext1R` jList
    `extR` (value :: F Integer)
    `extR` (value :: F Int)
    `extR` (value :: F Word8)
    `extR` (value :: F Word16)
    `extR` (value :: F Word32)
    `extR` (value :: F Word64)
    `extR` (value :: F Int8)
    `extR` (value :: F Int16)
    `extR` (value :: F Int32)
    `extR` (value :: F Int64)
    `extR` (value :: F Double)
    `extR` (value :: F Float)
    `extR` (value :: F Char)
    `extR` (value :: F String)
    `extR` (value :: F Bool)
    `extR` (value :: F ())
    `extR` (value :: F Ordering)
    `extR` (value :: F I.IntSet)
    `extR` (value :: F S.ByteString)
    `extR` (value :: F L.ByteString)
    `extR` (value :: F UTCTime)
  where
    value :: (JSON a) => Result a
    value = readJSON j

    jList :: (Data e) => Result [e]
    jList = case j of
      JSArray js -> mapM fromJSON js
      _ -> Error $ "fromJSON: Prelude.[] bad data: " ++ show j

fromJSONGeneric :: (Data a) => JSValue -> Result a
fromJSONGeneric j = generic
  where
    typ = dataTypeOf $ resType generic
    generic = case dataTypeRep typ of
      AlgRep [] -> case j of JSNull -> return (error "Empty type"); _ -> Error "fromJSON: no-constr bad data"
      AlgRep [_] -> decodeArgs (indexConstr typ 1) j
      AlgRep _ -> do (c, j') <- getConstr typ j; decodeArgs c j'
      rep -> Error $ "fromJSON: " ++ show rep ++ "(" ++ show typ ++ ")"
    getConstr t (JSObject o) | [(s, j')] <- fromJSObject o = do c <- readConstr' t s; return (c, j')
    getConstr t (JSString js) = do c <- readConstr' t (fromJSString js); return (c, JSNull) -- handle nullare constructor
    getConstr _ _ = Error "fromJSON: bad constructor encoding"
    readConstr' t s =
      maybe
        (Error $ "fromJSON: unknown constructor: " ++ s ++ " " ++ show t)
        return
        $ readConstr t s

    decodeArgs c = decodeArgs' (numConstrArgs (resType generic) c) c (constrFields c)
    decodeArgs' 0 c _ JSNull = construct c [] -- nullary constructor
    decodeArgs' 1 c [] jd = construct c [jd] -- unary constructor
    decodeArgs' n c [] (JSArray js) | n > 1 = construct c js -- no field names
    decodeArgs' _ c fs@(_ : _) (JSObject o) = selectFields (fromJSObject o) fs >>= construct c -- field names
    decodeArgs' _ c _ jd = Error $ "fromJSON: bad decodeArgs data " ++ show (c, jd)

    -- Build the value by stepping through the list of subparts.
    construct c = evalStateT $ fromConstrM f c
      where
        f :: (Data a) => StateT [JSValue] Result a
        f = do js <- get; case js of { [] -> lift $ Error "construct: empty list"; j' : js' -> do { put js'; lift $ fromJSON j' } }

    -- Select the named fields from a JSON object.  FIXME? Should this use a map?
    selectFields fjs = mapM sel
      where
        sel f = maybe (Error $ "fromJSON: field does not exist " ++ f) Ok $ lookup f fjs

    -- Count how many arguments a constructor has.  The value x is used to determine what type the constructor returns.
    numConstrArgs :: (Data a) => a -> Constr -> Int
    numConstrArgs x c = execState (fromConstrM f c `asTypeOf` return x) 0
      where
        f = do modify (+ 1); return undefined

    resType :: Result a -> a
    resType _ = error "resType"

encodeJSON :: (Data a) => a -> String
encodeJSON x = showJSValue (toJSON x) ""

decodeJSON :: (Data a) => String -> a
decodeJSON s =
  case parse parseJSValue "" s of
    Left msg -> error $ show msg
    Right j ->
      case fromJSON j of
        Error msg -> error msg
        Ok x -> x
