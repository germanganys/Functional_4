Министерство науки и высшего образования Российской Федерации федеральное государственное автономное образовательное учреждение высшего образования
 
«Национальный исследовательский университет ИТМО»
 
---
__ФПИиКТ, Системное и Прикладное Программное Обеспечение__
 
__Лабораторная работа №4__
 
по Функциональному программированию
 
Выполнил: 
Провоторов А. В.
Ганыс Г. В.
Группа: P34112
 
Преподаватель: Пенской Александр Владимирович
 
###### Санкт-Петербург
###### 2022 г.
 

## Цель работы:
Познакомиться с Data.Generics и концепцией Haskell, реализовать 
сериализацию/десериализацию инстансов алгебраических типов данных в формат json,
разработать набор команд для работы с json используя концепцию Free Monad

## Структура модулей

- Text (Библиотека для работы с json)
  - SimpleJSON
    - Generic.hs (Сериализация / Десериализация структур данных)
    - String.hs (Парсинг json/Форматирование json в строку)
    - Types.hs (Описание внутреннего представления для json)
  - SimpleJSON.hs (Внешний api для работы с библиотекой)
- Free (Библиотека для написания сценариев для работы с json)
  - Commands.hs (Набор команд и описание структуры)
  - Interpreter.hs (Интерпретатор команд из модуля выше)

## Тесты
- GenericTest.hs (unit тесты для Text.SimpleJSON.Generic)
- HUnit.hs (unit тесты для Text и Free)
- QC.hs (quick-check тесты для Text)

# Код
## Free Monad
```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}

module Free.Commands (
    Cmd(..),
    Script,
    printt,
    jsonObjFromFile,
    jsonToText,
    rmJsonField,
    getJsonField,
    setJsonField,
    matchJsonObject
) where

import Control.Monad.Free ( Free, liftF )
import Data.Text ( Text )
import Text.SimpleJSON.Types ( JSValue(..), JSObject )

data Cmd next
    = JSONFromFile String (JSObject -> next)
    | JSONToText JSValue (Text -> next)
    | RemoveJSONField String JSObject (JSObject -> next)
    | GetJSONField String JSObject (JSValue -> next)
    | SetJSONField String JSValue JSObject (JSObject -> next)
    | MatchJSObject JSObject JSObject (Maybe JSObject -> next)
    | Echo Text (Text -> next)
    | Print Text next
    deriving stock (Functor)

type Script a = Free Cmd a

printt :: Text -> Script ()
printt arg = liftF (Print arg ())

jsonObjFromFile :: String -> Script JSObject
jsonObjFromFile filename = liftF (JSONFromFile filename id)

jsonToText :: JSValue -> Script Text
jsonToText jsval = liftF (JSONToText jsval id)

rmJsonField :: String -> JSObject -> Script JSObject
rmJsonField key jsobj = liftF (RemoveJSONField key jsobj id)

getJsonField :: String -> JSObject -> Script JSValue
getJsonField key jsobj = liftF (GetJSONField key jsobj id)

setJsonField :: String -> JSValue -> JSObject -> Script JSObject
setJsonField key jsval jsobj = liftF (SetJSONField key jsval jsobj id)

matchJsonObject :: JSObject -> JSObject -> Script (Maybe JSObject)
matchJsonObject patt jsobj = liftF (MatchJSObject patt jsobj id)
```

## Generic json
```haskell
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
```

## Типы данных json
```haskell
data JSValue
    = JSNull
    | JSBool Bool
    | JSRational Rational
    | JSString JSString
    | JSArray [JSValue]
    | JSObject JSObject
    deriving (Show, Read, Eq, Ord, Typeable, Data)

newtype JSString = JSONString {fromJSString :: String}
    deriving (Eq, Ord, Show, Read, Typeable, Data)

newtype JSObject = JSONObject {fromJSObject :: [(String, JSValue)]}
    deriving (Eq, Ord, Show, Read, Typeable, Data)
```

## Deriving 
```haskell
-- Кодогенерация функций, описанных ниже
splice :: FullDataDecl -> Exp () -> Exp ()
splice d x | x ~= "readJSON" = mkRead d
splice d (H.App _ x (H.Lit _ (H.Int _ y _))) | x~= "showJSON" = mkShow d y
splice _ e = error $ "makeJSON: unrecognized splice: " ++ show e

------------------------------------------------------------------------------
-- showJSON

-- на основе библиотеки Language.Haskell создается образ функции, переводящей объект -> JSValue
-- по данному шаблону, на основе полей конструктора создается функция, переводящая в JSValue с содержанием этих полей
mkShow :: FullDataDecl -> Integer -> Exp ()
mkShow d y = let
    hasFields = any (not . null . fst) (ctorDeclFields c)
    c = dataDeclCtors (snd d) !! fromInteger y
    mkFields = if hasFields then mkShowRecordFields else mkShowPlainFields
  in
    mkJSObject $ H.List ()
        [H.Tuple () H.Boxed [strE (ctorDeclName c), mkFields (ctorDeclFields c)]]

mkShowPlainFields :: FieldDecl -> Exp ()
mkShowPlainFields fs = mkJSArray $ H.List ()
    [H.App () (var "showJSON") xi | xi <- vars "x" fs]

mkShowRecordFields :: FieldDecl -> Exp ()
mkShowRecordFields fs = mkJSObject $ H.List ()
    [ H.Tuple () H.Boxed [strE fn, H.App () (var "showJSON") xi]
    | ((fn, _), xi) <- zip fs (vars "x" fs)]

------------------------------------------------------------------------------
-- readJSON

-- данная функция делает объект из JSValue, тоже на основе Language.Haskell
mkRead :: FullDataDecl -> Exp ()
mkRead (_, d) = let
    readError = H.App () (con "Error") $ strE "malformed JSON for type ...: ..."
  in
    H.Case () (H.App () (var "fromJSObject") $ var "x") $
    map mkReadCtor (dataDeclCtors d) ++
    [H.Alt () (H.PWildCard ()) (H.UnGuardedRhs () readError) Nothing]

mkReadCtor :: CtorDecl -> Alt ()
mkReadCtor c = let
    cn = ctorDeclName c
    fs = ctorDeclFields c
    hasFields = any (not . null . fst) fs
    body | hasFields = mkReadRecord cn fs
         | otherwise = mkReadPlain cn fs
  in
    H.Alt () (H.PList () [H.PTuple () H.Boxed [strP cn, pVar "y"]])
         (H.UnGuardedRhs () body) Nothing

mkReadRecord :: String -> FieldDecl -> Exp ()
mkReadRecord cn fs = H.Do () $
    [H.Generator () (H.PApp () (qname "JSObject") [pVar "z"])
          (H.App () (var "return") $ var "y")] ++
    [H.LetStmt () $ H.BDecls () [H.PatBind () (pVar "d")
          (H.UnGuardedRhs () $ H.App () (var "fromJSObject") $ var "z")
          Nothing]] ++
    zipWith (mkReadRecordField cn) (pVars "x" fs) fs ++
    mkReadTrailer cn fs

mkReadRecordField :: String -> Pat () -> (String, Type ()) -> Stmt ()
mkReadRecordField cn xi (fn, _) = H.Generator () xi $
    apps (var "maybe") [
        H.App () (var "fail") $ strE (unwords ["readJSON: missing field", fn,
                                          "while decoding a", cn]),
        var "return",
        apps (var "lookup") [strE fn, var "d"]]

mkReadPlain :: String -> FieldDecl -> Exp ()
mkReadPlain cn fs = H.Do () $
    [H.Generator () (H.PApp () (qname "JSArray") [H.PList () (pVars "x" fs)])
        (H.App () (var "return") $ var "y")] ++
    mkReadTrailer cn fs

mkReadTrailer :: String -> FieldDecl -> [Stmt ()]
mkReadTrailer cn fs =
    [ H.Generator () yi (H.App () (var "readJSON") xi)
    | (xi, yi) <- zip (vars "x" fs) (pVars "y" fs)] ++
    [H.Qualifier () $ H.App () (var "return") $ apps (con cn) (vars "y" fs)]

------------------------------------------------------------------------------
-- utilites

mkJSObject :: Exp () -> Exp ()
mkJSObject e = H.App () (con "JSObject") (H.App () (var "toJSObject") e)

mkJSArray :: Exp () -> Exp ()
mkJSArray e = H.App () (con "JSArray") e

vars :: String -> FieldDecl -> [Exp ()]
vars pre fs = [var (pre ++ show i) | i <- [1..length fs]]

pVars :: String -> FieldDecl -> [Pat ()]
pVars pre fs = [pVar (pre ++ show i) | i <- [1..length fs]]
```

## Выводы
 
В ходе выполнения данной лабораторной работы освоили библиотеку Data.Generics и концепцией Free Monad
