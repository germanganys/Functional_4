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