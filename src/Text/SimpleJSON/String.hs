{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Text.SimpleJSON.String(
    parseJSValue,
    parseObj,
    showJSObject,
    showJSValue,
    StringRepresentable (..),
) where

import Data.Ratio (denominator, numerator)
import Numeric (showHex)
import Text.SimpleJSON.Types(
    JSObject,
    JSString,
    JSValue (..),
    fromJSObject,
    fromJSString,
    toJSObject,
    toJSString
 )
import Text.ParserCombinators.Parsec
    ( char,
      digit,
      hexDigit,
      satisfy,
      spaces,
      string,
      between,
      count,
      many1,
      option,
      sepBy,
      (<|>),
      many,
      try,
      Parser )
import Prelude hiding (quot)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyT
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LazyB
import Data.Text.Encoding (decodeUtf8)

class StringRepresentable a where
    toString :: a -> String

instance StringRepresentable String where toString :: String -> String
                                          toString = id

instance StringRepresentable T.Text where toString :: T.Text -> String
                                          toString = T.unpack

instance StringRepresentable LazyT.Text where toString :: LazyT.Text -> String
                                              toString = LazyT.unpack

instance StringRepresentable B.ByteString where toString :: B.ByteString -> String
                                                toString = T.unpack . decodeUtf8

instance StringRepresentable LazyB.ByteString where toString :: LazyB.ByteString -> String
                                                    toString = T.unpack . decodeUtf8 . LazyB.toStrict

-- Перевод файла со строками JSON осуществляется через библиотеку Parsec
-- Тип объекта определяется через комбинатор: функции выполняются до тех пор, пока одна из них не выполнится успешно
-- Библиотека Parsec имеет синтаксис, позволяющий легко парсить между строками
-- Н-р: строка "spaces *> string "null" <* spaces" означает, что в шаблон поступает строка null без пробелов слева и справа
--      вне зависимости от того, сколько стояло, т.е. возвращает просто "null"

parseJSValue :: Parser JSValue
parseJSValue = do
    parseObj <|> parseArray <|> parseBoolean <|> parseNull <|> parseJSONStr <|> parseNumber

parseObj :: Parser JSValue
parseObj = do
    _ <- spaces *> char '{' <* spaces
    props <- sepBy parseKV (spaces *> char ',' <* spaces)
    _ <- spaces *> char '}' <* spaces
    return . JSObject $ toJSObject props

parseKV :: Parser (String, JSValue)
parseKV = do
    key <- parseLabel
    value <- parseJSValue
    return (key, value)

parseStr :: Parser String
parseStr = p
    where
        p = between (char '"') (char '"') $ many oneChar
        oneChar = raw <|> char '\\' *> quoted
        raw = satisfy (\c -> c /= '"' && c /= '\\')
        quoted =
            tab <|> quot <|> revsolidus <|> solidus <|> backspace <|> formfeed <|> nl <|> cr <|> hexUnicode
        tab = char 't' *> pure '\t'
        quot = char '"' *> pure '"'
        revsolidus = char '/' *> pure '/'
        solidus = char '\\' *> pure '\\'
        backspace = char 'b' *> pure '\b'
        formfeed = char 'f' *> pure '\f'
        nl = char 'n' *> pure '\n'
        cr = char 'r' *> pure '\r'
        hexUnicode = char 'u' *> count 4 hexDigit >>= decodeUtf
        decodeUtf x = pure $ toEnum (read ('0' : 'x' : x) :: Int)

parseLabel :: Parser String
parseLabel = do spaces *> parseStr <* (spaces *> char ':' <* spaces)

parseNumber :: Parser JSValue
parseNumber = do
    number <- spaces *> baseNumber <* spaces
    return (JSRational (toRational number))

baseNumber :: Parser Double
baseNumber =
    read . concat <$> sequence
        [opt $ string "-",
        string "0" <|> many1 digit,
        opt $ (:) <$> char '.' <*> many1 digit,
        opt $ concat <$> sequence [string "e" <|> string "E", opt $ string "+" <|> string "-", many1 digit]
        ]
    where
        opt = option ""

parseJSONStr :: Parser JSValue
parseJSONStr = do
    str <- spaces *> parseStr <* spaces
    return . JSString $ toJSString str

parseBoolean :: Parser JSValue
parseBoolean = do
    bool <- spaces *> (string "true" <|> string "false") <* spaces
    if bool == "true" then return (JSBool True) else return (JSBool False)

parseNull :: Parser JSValue
parseNull = do
    _ <- spaces *> string "null" <* spaces
    return JSNull

commaSeparated :: Parser a -> Parser [a]
commaSeparated = flip sepBy comma
    where
        comma = (variant1 <|> try variant2) <* spaces
        variant1 = char ',' <* spaces
        variant2 = spaces *> char ','

parseArray :: Parser JSValue
parseArray = do
    _ <- spaces *> char '[' <* spaces
    a <- commaSeparated (parseObj <|> parseArray <|> parseBoolean <|> parseNull <|> parseJSONStr <|> parseNumber)
    _ <- spaces *> char ']' <* spaces
    return $ JSArray a

-- Функции, начинающиеся с parse переводят объект в JSON текст
showJSValue :: JSValue -> ShowS
showJSValue jv =
    case jv of
        JSNull {} -> showJSNull
        JSBool b -> showJSBool b
        JSRational r -> showJSRational r
        JSArray a -> showJSArray a
        JSString s -> showJSString s
        JSObject o -> showJSObject o

showJSNull :: ShowS
showJSNull = showString "null"

showJSBool :: Bool -> ShowS
showJSBool True = showString "true"
showJSBool False = showString "false"

showJSString :: JSString -> ShowS
showJSString x xs = showChar '"' (encJSString x (showChar '"' xs))

showJSRational :: Rational -> ShowS
showJSRational r
    | denominator r == 1 = shows $ numerator r
    | isInfinite x || isNaN x = showJSNull
    | otherwise = shows x
    where
        x :: Double
        x = realToFrac r

showJSArray :: [JSValue] -> ShowS
showJSArray = showSequence '[' ']' ','

showJSObject :: JSObject -> ShowS
showJSObject = showAssocs '{' '}' ',' . fromJSObject

showAssocs :: Char -> Char -> Char -> [(String, JSValue)] -> ShowS
showAssocs start end sep xs rest = start : go xs
  where
    go [(k, v)] =
      '"'
        : encJSString
          (toJSString k)
          ('"' : ':' : showJSValue v (go []))
    go ((k, v) : kvs) =
      '"'
        : encJSString
          (toJSString k)
          ('"' : ':' : showJSValue v (sep : go kvs))
    go [] = end : rest

showSequence :: Char -> Char -> Char -> [JSValue] -> ShowS
showSequence start end sep xs rest = start : go xs
    where
        go [y] = showJSValue y (go [])
        go (y : ys) = showJSValue y (sep : go ys)
        go [] = end : rest

encJSString :: JSString -> ShowS
encJSString jss ss = go (fromJSString jss)
    where
        go s1 =
            case s1 of
                 (x : xs) | x < '\x20' -> '\\' : encControl x (go xs)
                 ('"' : xs) -> '\\' : '"' : go xs
                 ('\\' : xs) -> '\\' : '\\' : go xs
                 (x : xs) -> x : go xs
                 "" -> ss

        encControl x xs = case x of
            '\b' -> 'b' : xs
            '\f' -> 'f' : xs
            '\n' -> 'n' : xs
            '\r' -> 'r' : xs
            '\t' -> 't' : xs
            _
                | x < '\x10' -> 'u' : '0' : '0' : '0' : hexxs
                | x < '\x100' -> 'u' : '0' : '0' : hexxs
                | x < '\x1000' -> 'u' : '0' : hexxs
                | otherwise -> 'u' : hexxs
            where
                hexxs = showHex (fromEnum x) xs
