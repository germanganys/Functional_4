{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}

module Text.SimpleJSON.Generic(
    module Text.SimpleJSON,
    Data,
    Typeable,
    toJSON,
    fromJSON,
    encodeJSON,
    decodeJSON,
) where

import Control.Monad.State
    ( MonadTrans(lift),
      MonadState(put, get),
      modify,
      evalStateT,
      execState,
      StateT )
import Data.Generics
    ( Data(toConstr, gmapQ, dataTypeOf),
      Typeable,
      DataRep(AlgRep),
      Constr,
      constrFields,
      dataTypeRep,
      fromConstrM,
      indexConstr,
      readConstr,
      showConstr,
      ext1Q,
      ext1R,
      extQ,
      extR )
import Text.SimpleJSON
import Text.SimpleJSON.String (parseJSValue)
import Text.ParserCombinators.Parsec (parse)

type T a = a -> JSValue

toJSON :: (Data a) => a -> JSValue
toJSON =
        toJSONGeneric
        `ext1Q` jList
        `extQ` (showJSON :: T Integer)
        `extQ` (showJSON :: T Int)
        `extQ` (showJSON :: T Double)
        `extQ` (showJSON :: T Float)
        `extQ` (showJSON :: T Char)
        `extQ` (showJSON :: T String)
        `extQ` (showJSON :: T Bool)
    where
        jList vs = JSArray $ map toJSON vs

toJSONGeneric :: (Data a) => a -> JSValue
toJSONGeneric = generic
    where
        generic a =
            case dataTypeRep (dataTypeOf a) of
                AlgRep [] -> JSNull
                AlgRep [c] -> encodeArgs c (toJSON `gmapQ` a)
                AlgRep _ -> encodeConstr (toConstr a) (toJSON `gmapQ` a)
                rep -> error $ "toJSON: not AlgRep " ++ show rep ++ "(" ++ show (dataTypeOf a) ++ ")"

        encodeConstr constr [] = JSString $ toJSString $ showConstr constr
        encodeConstr constr argList = makeObj [(showConstr constr, encodeArgs constr argList)]

        encodeArgs constr = encodeArgs' (constrFields constr)
            where
                encodeArgs' [] [j] = j
                encodeArgs' [] js = JSArray js
                encodeArgs' ns js = makeObj $ zip ns js

type F a = Result a

fromJSON :: (Data a) => JSValue -> Result a
fromJSON j =
    fromJSONGeneric j
        `ext1R` jList
        `extR` (value :: F Integer)
        `extR` (value :: F Int)
        `extR` (value :: F Double)
        `extR` (value :: F Float)
        `extR` (value :: F Char)
        `extR` (value :: F String)
        `extR` (value :: F Bool)
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
            return $ readConstr t s

        decodeArgs c = decodeArgs' (numConstrArgs (resType generic) c) c (constrFields c)
        decodeArgs' 0 c _ JSNull = construct c [] -- nullary constructor
        decodeArgs' 1 c [] jd = construct c [jd] -- unary constructor
        decodeArgs' n c [] (JSArray js) | n > 1 = construct c js -- no field names
        decodeArgs' _ c fs@(_ : _) (JSObject o) = selectFields (fromJSObject o) fs >>= construct c -- field names
        decodeArgs' _ c _ jd = Error $ "fromJSON: bad decodeArgs data " ++ show (c, jd)

        construct c = evalStateT $ fromConstrM f c
            where
                f :: (Data a) => StateT [JSValue] Result a
                f = do js <- get; case js of { [] -> lift $ Error "construct: empty list"; j' : js' -> do { put js'; lift $ fromJSON j' } }

        selectFields fjs = mapM sel
            where
                sel f = maybe (Error $ "fromJSON: field does not exist " ++ f) Ok $ lookup f fjs

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