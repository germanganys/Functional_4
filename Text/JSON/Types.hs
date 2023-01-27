{-# LANGUAGE DeriveDataTypeable #-}

-- | Basic support for working with JSON values.
module Text.JSON.Types(
    -- * JSON Types
    JSValue (..),
    -- * Wrapper Types
    JSString (..),
    toJSString,
    JSObject (..),
    toJSObject,
    getField,
    setField,
) where

import Data.String (IsString (..))
import Data.Typeable (Typeable)

data JSValue
    = JSNull
    | JSBool !Bool
    | JSRational Bool !Rational
    | JSString JSString
    | JSArray [JSValue]
    | JSObject (JSObject JSValue)
    deriving (Show, Read, Eq, Ord, Typeable)

-- | Strings can be represented a little more efficiently in JSON
newtype JSString = JSONString {fromJSString :: String}
    deriving (Eq, Ord, Show, Read, Typeable)

-- | Turn a Haskell string into a JSON string.
toJSString :: String -> JSString
toJSString = JSONString

-- Note: we don't encode the string yet, that's done when serializing.

instance IsString JSString where
    fromString = toJSString

instance IsString JSValue where
    fromString = JSString . fromString

-- | As can association lists
newtype JSObject e = JSONObject {fromJSObject :: [(String, e)]}
    deriving (Eq, Ord, Show, Read, Typeable)

-- | Make JSON object out of an association list.
toJSObject :: [(String, a)] -> JSObject a
toJSObject = JSONObject

-- | Get the value of a field, if it exist.
getField :: JSObject a -> String -> Maybe a
getField (JSONObject xs) x = lookup x xs

-- | Set the value of a field.  Previous values are overwritten.
setField :: JSObject a -> String -> a -> JSObject a
setField (JSONObject xs) k v = JSONObject ((k, v) : filter ((/= k) . fst) xs)
