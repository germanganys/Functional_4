{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Data.Int
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock ()
import Data.Word
import Text.JSON.Generic
import Text.JSON.String ()
import Text.ParserCombinators.Parsec ()

data Foo = Foo {a :: Int, b :: Bool, c :: Baz} | None
  deriving (Typeable, Data, Show, Eq)

data Baz = Baz Int
  deriving (Typeable, Data, Show, Eq)

data Bar = Int :+: Int | Zero
  deriving (Typeable, Data, Show, Eq)

newtype New a = New a
  deriving (Typeable, Data, Show, Eq)

newtype Apples = Apples {noApples :: Int}
  deriving (Typeable, Data, Show, Eq)

data Record = Record {x :: Int, y :: Double, z :: Float, s :: String, t :: (Bool, Int)}
  deriving (Typeable, Data, Show, Eq)

rec :: Record
rec = Record {x = 1, y = 2, z = 3.5, s = "hello", t = (True, 0)}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Typeable, Data, Show, Eq)

atree :: Tree Integer
atree = build 4
  where
    build 0 = Leaf
    build 1 = Node Leaf 100 Leaf
    build n = Node (build (n - 1)) n (build (n - 2))

data Color = Red | Green | Blue
  deriving (Typeable, Data, Show, Eq, Enum)

newtype Times = Times (UTCTime)
  deriving (Typeable, Data, Show, Eq)

from :: Result a -> a
from (Ok xr) = xr
from (Error es) = error es

testJSON :: (Data a, Eq a) => a -> Bool
testJSON x1 =
  -- x == viaJSON x
  x1 == decodeJSON (encodeJSON x1)

tests :: Bool
tests =
  and
    [ testJSON (1 :: Integer),
      testJSON (42 :: Int),
      testJSON (100 :: Word8),
      testJSON (-1000 :: Int64),
      testJSON (4.2 :: Double),
      testJSON (4.1 :: Float),
      testJSON (True :: Bool),
      testJSON 'q',
      testJSON "Hello, World\n",
      testJSON (Nothing :: Maybe Int),
      testJSON (Just "aa"),
      testJSON [],
      testJSON ([1, 2, 3, 4] :: [Integer]),
      testJSON (Left 1 :: Either Int Bool),
      testJSON (Right True :: Either Int Bool),
      testJSON (1 :: Integer, True),
      testJSON (1 :: Integer, 2 :: Integer, True, 'a' :: Char, "apa" :: String, (4.5 :: Double, 99 :: Integer)),
      testJSON $ Baz 11,
      testJSON $ Foo 1 True (Baz 42),
      testJSON None,
      testJSON $ 2 :+: 3,
      testJSON Zero,
      testJSON $ New (2 :+: 3),
      testJSON rec,
      testJSON [LT, EQ, GT],
      testJSON atree,
      testJSON (),
      testJSON $ Apples 42,
      testJSON [Red .. Blue],
      testJSON $ Times (UTCTime (fromGregorian 2025 1 27) (secondsToDiffTime 0))
    ]

main :: IO ()
main =
  if tests
    then return ()
    else error "Generic test failed"
