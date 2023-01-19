{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

import           Control.Applicative (liftA2)
import           Data.Char
import           Data.Foldable       (for_)
import           Data.Functor
import qualified Data.HashMap.Strict as M
import           Data.List           (intercalate)
import           Prelude             hiding (any)
import           System.Environment
import           Text.Printf

data ParseError = ParseError
  { errExpected :: String
  , errFound    :: String
  }

newtype Parser a = Parser { runParser :: String -> (String, Either ParseError a) }
  deriving (Functor)


instance Show ParseError where
  show (ParseError e f) = printf "expected %s but found %s" e f

instance Applicative Parser where
  pure c = Parser $ \s -> (s, Right c)
  pf <*> pa = Parser $ \s -> case runParser pf s of
    (s', Right f) -> fmap f <$> runParser pa s'
    (s', Left  e) -> (s', Left e)

instance Monad Parser where
  pa >>= f = Parser $ \s -> case runParser pa s of
    (s', Right a) -> runParser (f a) s'
    (s', Left  e) -> (s', Left e)


run :: Parser a -> String -> Either ParseError a
run p s = snd $ runParser (p <* eof) s


any :: Parser Char
any = Parser $ \case
  []     -> ("", Left $ ParseError "any character" "the end of the input")
  (x:xs) -> (xs, Right x)

eof :: Parser ()
eof = Parser $ \case
  []      -> ("", Right ())
  s@(c:_) -> (s, Left $ ParseError "the end of the input" [c])

parseError :: String -> String -> Parser a
parseError expected found = Parser $ \s -> (s, Left $ ParseError expected found)

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description predicate = try $ do
  c <- any
  if predicate c
    then pure c
    else parseError description [c]


try :: Parser a -> Parser a
try p = Parser $ \s -> case runParser p s of
  (_s', Left err) -> (s, Left err)
  success         -> success

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \s -> case runParser p1 s of
  (s', Left err)
    | s' == s   -> runParser p2 s
    | otherwise -> (s', Left err)
  success -> success

choice :: String -> [Parser a] -> Parser a
choice description = foldr (<|>) noMatch
  where noMatch = parseError description "no match"

many, many1 :: Parser a -> Parser [a]
many  p = many1 p <|> pure []
many1 p = liftA2 (:) p $ many p

sepBy, sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy  p s = sepBy1 p s <|> pure []
sepBy1 p s = liftA2 (:) p $ many (s >> p)


char c = satisfy [c]     (== c)
space  = satisfy "space" isSpace
digit  = satisfy "digit" isDigit


string = traverse char
spaces = many space
symbol s = string s <* spaces

between o c p = o *> p <* c
brackets = between (symbol "[") (symbol "]")
braces   = between (symbol "{") (symbol "}")


data JValue = JObject (M.HashMap String JValue)
            | JArray  [JValue]
            | JString String
            | JNumber Double
            | JBool   Bool
            | JNull



instance Show JValue where
  show = \case
    JNull     -> "null"
    JBool b   -> toLower <$> show b
    JNumber n -> show n
    JString s -> show s
    JArray  a -> show a
    JObject o -> printf "{%s}" $ intercalate ", " [printf "%s: %s" (show k) (show v) | (k,v) <- M.toList o]


json = spaces >> jsonValue
jsonValue = choice "a JSON value"
  [ JObject <$> jsonObject
  , JArray  <$> jsonArray
  , JString <$> jsonString
  , JNumber <$> jsonNumber
  , JBool   <$> jsonBool
  , JNull   <$  symbol "null"
  ]

jsonObject = do
  assocList <- braces $ jsonEntry `sepBy` symbol ","
  return $ M.fromList assocList
  where
    jsonEntry = do
      k <- jsonString
      symbol ":"
      v <- jsonValue
      return (k,v)

jsonArray = brackets $ jsonValue `sepBy` symbol ","

jsonString =
  between (char '"') (char '"') (many jsonChar) <* spaces
  where
    jsonChar = choice "JSON string character"
      [ try $ '\n' <$ string "\\n"
      , try $ '\t' <$ string "\\t"
      , try $ '"'  <$ string "\\\""
      , try $ '\\' <$ string "\\\\"
      , satisfy "not a quote" (/= '"')
      ]

jsonNumber = read <$> many1 digit

jsonBool = choice "JSON boolean"
  [ True  <$ symbol "true"
  , False <$ symbol "false"
  ]


main :: IO ()
main = do
  args <- getArgs
  for_ args $ \filename -> do
    content <- readFile filename
    case run json content of
        (Right jvalue) -> putStrLn $ show $ jvalue
        (Left err) -> putStrLn $ show $ err

