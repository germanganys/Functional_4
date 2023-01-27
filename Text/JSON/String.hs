-- | Basic support for working with JSON values.
module Text.JSON.String
  ( -- * Parsing

    --

    parseJSTopType,
    parseJSValue,

    -- ** Writing JSON
    showJSNull,
    showJSBool,
    showJSArray,
    showJSObject,
    showJSRational,
    showJSRational',
    showJSValue,
    showJSTopType,
  )
where

import Data.Ratio (denominator, numerator)
import Numeric (showHex)
import Text.JSON.Types
  ( JSObject,
    JSString,
    JSValue (..),
    fromJSObject,
    fromJSString,
    toJSObject,
    toJSString,
  )
import Text.ParserCombinators.Parsec hiding (label, tab)
import Prelude hiding (quot)

-- -----------------------------------------------------------------

-- | Parsing JSON

-- | Top level JSON can only be Arrays or Objects
parseJSTopType :: Parser JSValue
parseJSTopType = do
  value <- (parseObj <|> parseArray) <* eof
  return value

parseJSValue :: Parser JSValue
parseJSValue = do
  parseObj <|> parseArray <|> parseBoolean <|> parseNull <|> parseJSONStr <|> parseNumber

parseObj :: Parser JSValue
parseObj = do
  _ <- spaces *> char '{' <* spaces
  props <- sepBy parseProps (spaces *> char ',' <* spaces)
  _ <- spaces *> char '}' <* spaces
  return . JSObject $ toJSObject props

parseStr :: Parser String
parseStr = p
  where
    p = between (char '"') (char '"') $ many oneChar
    oneChar = raw <|> char '\\' *> quoted
    raw = satisfy (\c -> c /= '"' && c /= '\\')
    quoted =
      tab
        <|> quot
        <|> revsolidus
        <|> solidus
        <|> backspace
        <|> formfeed
        <|> nl
        <|> cr
        <|> hexUnicode
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

parseProps :: Parser (String, JSValue)
parseProps = do
  label <- parseLabel
  value <- parseJSValue
  return (label, value)

parseLabel :: Parser String
parseLabel = do
  spaces *> parseStr <* (spaces *> char ':' <* spaces)

parseNumber :: Parser JSValue
parseNumber = do
  number <- spaces *> baseNumber <* spaces
  return (JSRational False (toRational number))

baseNumber :: Parser Double
baseNumber =
  read . concat
    <$> sequence
      [ opt $ string "-",
        string "0" <|> many1 digit,
        opt $ (:) <$> char '.' <*> many1 digit,
        opt $
          concat
            <$> sequence
              [ string "e" <|> string "E",
                opt $ string "+" <|> string "-",
                many1 digit
              ]
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
  a <-
    commaSeparated
      ( parseObj
          <|> parseArray
          <|> parseBoolean
          <|> parseNull
          <|> parseJSONStr
          <|> parseNumber
      )
  _ <- spaces *> char ']' <* spaces
  return $ JSArray a

-- -----------------------------------------------------------------

-- | Writing JSON

-- | Show strict JSON top level types. Values not permitted
-- at the top level are wrapped in a singleton array.
showJSTopType :: JSValue -> ShowS
showJSTopType (JSArray a) = showJSArray a
showJSTopType (JSObject o) = showJSObject o
showJSTopType x = showJSTopType $ JSArray [x]

-- | Show JSON values
showJSValue :: JSValue -> ShowS
showJSValue jv =
  case jv of
    JSNull {} -> showJSNull
    JSBool b -> showJSBool b
    JSRational asF r -> showJSRational' asF r
    JSArray a -> showJSArray a
    JSString s -> showJSString s
    JSObject o -> showJSObject o

-- | Write the JSON null type
showJSNull :: ShowS
showJSNull = showString "null"

-- | Write the JSON Bool type
showJSBool :: Bool -> ShowS
showJSBool True = showString "true"
showJSBool False = showString "false"

-- | Write the JSON String type
showJSString :: JSString -> ShowS
showJSString x xs = quote (encJSString x (quote xs))
  where
    quote = showChar '"'

-- | Show a Rational in JSON format
showJSRational :: Rational -> ShowS
showJSRational = showJSRational' False

showJSRational' :: Bool -> Rational -> ShowS
showJSRational' asFloat r
  | denominator r == 1 = shows $ numerator r
  | isInfinite x || isNaN x = showJSNull
  | asFloat = shows xf
  | otherwise = shows x
  where
    x :: Double
    x = realToFrac r

    xf :: Float
    xf = realToFrac r

-- | Show a list in JSON format
showJSArray :: [JSValue] -> ShowS
showJSArray = showSequence '[' ']' ','

-- | Show an association list in JSON format
showJSObject :: JSObject JSValue -> ShowS
showJSObject = showAssocs '{' '}' ',' . fromJSObject

-- | Show a generic sequence of pairs in JSON format
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

-- | Show a generic sequence in JSON format
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
