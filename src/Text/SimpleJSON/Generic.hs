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

-- Список возможных типов данных, которые могут быть переведены в Haskell тип данных
-- Здесь не может быть bad data ошибки, поскольку это output из программы в текст, уже проверенный
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

-- Если тип не был найден, то из generic типа делается JSValue тип, который затем будет выведен в JSON файл
toJSONGeneric :: (Data a) => a -> JSValue
toJSONGeneric = generic
    where
        generic a =
            -- let data Tree b = Leaf | Node (Tree b) b (Tree b) deriving (Typeable, Data, Show, Eq)
            -- let instance = Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf)

            -- dataTypeOf :: a -> DataType (datatype name and all public constructors)
            -- DataType {tycon = "Tree", datarep = AlgRep [Leaf,Node]}

            -- dataTypeRep Gets the public presentation of a datatype
            -- AlgRep [Leaf,Node]

            -- toConstr - Obtaining the constructor from a given datum
            -- Node
            -- showConstr "Node"

            -- gmapQ map current instance fields an return results

            case dataTypeRep (dataTypeOf a) of
                AlgRep [] -> JSNull
                AlgRep [c] -> encodeArgs c (toJSON `gmapQ` a)
                AlgRep _ -> encodeConstr (toConstr a) (toJSON `gmapQ` a)
                rep -> error $ "toJSON: not AlgRep " ++ show rep ++ "(" ++ show (dataTypeOf a) ++ ")"

        encodeConstr constr [] = JSString $ toJSString $ showConstr constr
        encodeConstr constr argList = makeObj [(showConstr constr, encodeArgs constr argList)]

        -- constFields Gets the field labels of a constructor.
        encodeArgs constr = encodeArgs' (constrFields constr)
            where
                -- no fields and one value -> value
                encodeArgs' [] [j] = j
                -- no fields and many values -> array
                encodeArgs' [] js = JSArray js
                encodeArgs' ns js = makeObj $ zip ns js

type F a = Result a

-- Список возможных типов данных, которые могут быть переведены в Haskell тип данных
-- Если в JSArray ничего в себе не содержит - ошибка bad data
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

-- Аналогично с предыдущим пунктом, если данный тип не был найден, то создается generic конструктор, в который JSValue будет переведено
fromJSONGeneric :: (Data a) => JSValue -> Result a
fromJSONGeneric j = generic
    where
        -- Создается тип для JSValue
        typ = dataTypeOf $ resType generic

        -- Здесь смотрится, если JSValue не содержит в себе ничего или уже приходит ошибка - вывод ошибки и опционально тип
        -- Если содержится массив чего-либо, алгоритм повторяется для каждого элемента
        -- Если JSValue представлен чем-либо, то вызывается функция getConstr, возвращающая либо объект, либо строку и функцию decodeArgs, описанную ниже
        generic = case dataTypeRep typ of
            AlgRep [] -> case j of JSNull -> return (error "Empty type"); _ -> Error "fromJSON: no-constr bad data"
            -- indexConstr Gets the constructor for an index (algebraic datatypes only)
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

        -- возвращает конструктор, с заданным количеством и названием полей
        decodeArgs c = decodeArgs' (numConstrArgs (resType generic) c) c (constrFields c)
        decodeArgs' 0 c _ JSNull = construct c [] -- nullary constructor
        decodeArgs' 1 c [] jd = construct c [jd] -- unary constructor
        decodeArgs' n c [] (JSArray js) | n > 1 = construct c js -- no field names
        decodeArgs' _ c fs@(_ : _) (JSObject o) = selectFields (fromJSObject o) fs >>= construct c -- field names
        decodeArgs' _ c _ jd = Error $ "fromJSON: bad decodeArgs data " ++ show (c, jd)

        -- fromConstrM -> Build a term and use a generic function for subterms
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

-- в JSON
encodeJSON :: (Data a) => a -> String
encodeJSON x = showJSValue (toJSON x) ""

-- из JSON
decodeJSON :: (Data a) => String -> a
decodeJSON s =
    case parse parseJSValue "" s of
        Left msg -> error $ show msg
        Right j ->
            case fromJSON j of
                Error msg -> error msg
                Ok x -> x
