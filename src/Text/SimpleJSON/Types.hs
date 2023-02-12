{-# LANGUAGE DeriveDataTypeable #-}

module Text.SimpleJSON.Types(
    JSValue (..),
    JSString (..),
    toJSString,
    JSObject (..),
    toJSObject,
    getField,
    setField,
) where

import Data.Typeable (Typeable)

data JSValue
    = JSNull
    | JSBool Bool
    | JSRational Rational
    | JSString JSString
    | JSArray [JSValue]
    | JSObject (JSObject JSValue)
    deriving (Show, Read, Eq, Ord, Typeable)

newtype JSString = JSONString {fromJSString :: String}
    deriving (Eq, Ord, Show, Read, Typeable)

newtype JSObject e = JSONObject {fromJSObject :: [(String, e)]}
    deriving (Eq, Ord, Show, Read, Typeable)

toJSString :: String -> JSString
toJSString = JSONString

toJSObject :: [(String, a)] -> JSObject a
toJSObject = JSONObject

getField :: JSObject a -> String -> Maybe a
getField (JSONObject xs) x = lookup x xs

setField :: JSObject a -> String -> a -> JSObject a
setField (JSONObject xs) k v = JSONObject ((k, v) : filter ((/= k) . fst) xs)
