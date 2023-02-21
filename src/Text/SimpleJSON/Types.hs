{-# LANGUAGE DeriveDataTypeable #-}

module Text.SimpleJSON.Types(
    JSValue (..),
    JSString (..),
    toJSString,
    JSObject (..),
    toJSObject,
    getField,
    setField,
    rmField,
    matchJSStructure,
) where

import Data.Typeable (Typeable)
import Data.Data (toConstr, Data)

-- в данном файле представлены конструкторы JSValue с тем, какие значения они имеют
-- и основные функции работы с JSON файлом
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

toJSString :: String -> JSString
toJSString = JSONString

toJSObject :: [(String, JSValue)] -> JSObject
toJSObject = JSONObject

getField :: JSObject -> String -> Maybe JSValue
getField (JSONObject xs) x = lookup x xs

setField :: JSObject -> String -> JSValue -> JSObject 
setField (JSONObject xs) k v = JSONObject ((k, v) : filter ((/= k) . fst) xs)

rmField :: JSObject -> String -> JSObject
rmField (JSONObject xs) k = JSONObject (filter ((/= k) . fst) xs)

sameConstructor :: (Data a1, Data a2) => a1 -> a2 -> Bool
sameConstructor l r = toConstr l == toConstr r

matchJSStructure :: JSObject -> JSObject -> Maybe JSObject
matchJSStructure patt obj = 
    if check (JSObject patt) (JSObject obj) then Just obj
    else Nothing
    where
        check :: JSValue -> JSValue -> Bool
        check p o = 
            case p of
                JSObject x ->
                    case o of
                        JSObject oo ->
                            case fromJSObject x of
                                (key, value): xs -> 
                                    case getField oo key of
                                        Just field -> check (JSObject (JSONObject xs)) o && check value field
                                        Nothing -> False
                                [] -> True
                        _ -> False
                JSArray (el: xa) ->
                    case o of
                        JSArray (el1: xa1) -> check el el1 && check (JSArray xa) (JSArray xa1)
                        _                  -> False
                JSArray [] ->
                    case o of
                        JSArray [] -> True
                        _          -> False
                otherConstructor -> sameConstructor otherConstructor o
                

