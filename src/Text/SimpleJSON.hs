{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Text.SimpleJSON(
    JSValue (..),
    JSON (..),
    Result (..),
    encode,
    decode,
    JSString,
    toJSString,
    fromJSString,
    JSObject,
    toJSObject,
    fromJSObject,
    showJSValue,
    makeObj,
    getFromObj,
) where

import Control.Applicative()
import Control.Monad (ap, liftM)
import Control.Monad.Fail ()
import qualified Data.Map as M()
import qualified Data.Text as T
import Text.SimpleJSON.String
    ( StringRepresentable(..), parseJSValue, showJSValue )
import Text.SimpleJSON.Types
    ( JSObject(fromJSObject),
      JSString(fromJSString),
      JSValue(..),
      toJSString,
      toJSObject )
import Text.ParserCombinators.Parsec ( parse )

-- Декодирование JSON файла в конструкторы может вызывать ошибку при неправильном синтаксе, поэтому
-- Right - функция считывания
-- Left - Вывод ошибки
decode :: (StringRepresentable st, JSON a) => st -> Result a
decode s = case parse parseJSValue "" (toString s) of
    Right a -> readJSON a
    Left err -> Error $ show err

-- Вывод JSON файла
encode :: (JSON a) => a -> String
encode = flip showJSValue [] . showJSON

class JSON a where
    readJSON :: JSValue -> Result a
    showJSON :: a -> JSValue

    readJSONs :: JSValue -> Result [a]
    readJSONs (JSArray a) = mapM readJSON a
    readJSONs _ = Error "Unable to read list"

    showJSONs :: [a] -> JSValue
    showJSONs = JSArray . map showJSON

-- так как монада Result требует описание аппликативного функтора, то
-- здесь написана простая реализация, где fmap = liftM, и pure = Ok, если все выполнилось
-- Возвращает pure и применяет f к a
data Result a = Ok a | Error String
    deriving (Eq, Show)

instance Functor Result where
    fmap = liftM

instance Applicative Result where
    (<*>) :: Result (a -> b) -> Result a -> Result b
    (<*>) = ap
    pure :: a -> Result a
    pure = Ok

instance Monad Result where
    return = pure
    Ok a >>= f = f a
    Error x >>= _ = Error x

instance MonadFail Result where
    fail = Error

instance JSON JSValue where
    showJSON = id
    readJSON = Ok

second :: (a -> b) -> (x, a) -> (x, b)
second f (a, b) = (a, f b)

-- Ниже описаны функции readJSON и showJSON для каждого варианта конструктора

instance JSON JSString where
    readJSON (JSString s) = return s
    readJSON _ = Error "Unable to read JSString"
    showJSON = JSString

instance JSON JSObject where
    readJSON (JSObject o) =
        let f (x, y) = do
            y' <- readJSON y
            return (x, y')
         in toJSObject `fmap` mapM f (fromJSObject o)
    readJSON _ = Error "Unable to read JSObject"
    showJSON = JSObject . toJSObject . map (second showJSON) . fromJSObject

instance JSON Bool where
    showJSON = JSBool
    readJSON (JSBool b) = return b
    readJSON _ = Error "Unable to read Bool"

instance JSON Char where
    showJSON = JSString . toJSString . (:[])
    showJSONs = JSString . toJSString

    readJSON (JSString s) = case fromJSString s of
        [c] -> return c
        _ -> Error "Unable to read Char"
    readJSON _ = Error "Unable to read Char"

    readJSONs (JSString s) = return (fromJSString s)
    readJSONs (JSArray a) = mapM readJSON a
    readJSONs _ = Error "Unable to read String"

instance JSON Integer where
    showJSON = JSRational . fromIntegral
    readJSON (JSRational i) = return $ round i
    readJSON _ = Error "Unable to read Integer"

instance JSON Int where
    showJSON = JSRational . fromIntegral
    readJSON (JSRational i) = return $ round i
    readJSON _ = Error "Unable to read Int"

instance JSON Double where
    showJSON = JSRational . toRational
    readJSON (JSRational r) = return $ fromRational r
    readJSON _ = Error "Unable to read Double"

instance JSON Float where
    showJSON = JSRational . toRational
    readJSON (JSRational r) = return $ fromRational r
    readJSON _ = Error "Unable to read Float"

instance JSON a => JSON [a] where
    showJSON = showJSONs
    readJSON = readJSONs

instance JSON T.Text where
    readJSON (JSString s) = return (T.pack . fromJSString $ s)
    readJSON _ = Error "Unable to read JSString"
    showJSON = JSString . toJSString . T.unpack


makeObj :: [(String, JSValue)] -> JSValue
makeObj = JSObject . toJSObject

getFromObj :: JSON a => String -> JSObject -> Result a
getFromObj k o =
    maybe
        (Error $ "valFromObj: Could not find key: " ++ show k)
        readJSON
        (lookup k (fromJSObject o))
