{-
Name: Brent McIvor
-}

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

--Exercise 01
parseMessage :: String -> MaybeLogMessage
parseMessage str = case words str of
    ("I":timeStamp:msg)          -> parseInfo timeStamp $ unwords msg
    ("W":timeStamp:msg)          -> parseWarning timeStamp $ unwords msg
    ("E":severity:timeStamp:msg) -> parseError severity timeStamp $ unwords msg
    _                            -> InvalidLM str


parseInfo :: String -> String -> MaybeLogMessage
parseInfo timeStamp msg
   | isValidInt timeStamp = ValidLM $ LogMessage Info (readTimeStamp timeStamp) msg
   | otherwise            = InvalidLM msg

parseWarning :: String -> String -> MaybeLogMessage
parseWarning timeStamp msg
   | isValidInt timeStamp = ValidLM $ LogMessage Warning (readTimeStamp timeStamp) msg
   | otherwise            = InvalidLM msg

parseError :: String -> String -> String -> MaybeLogMessage
parseError severity timeStamp msg
   | (isValidInt severity && isValidInt timeStamp) = ValidLM $ LogMessage (Error (readError severity)) (readTimeStamp timeStamp) msg
   | otherwise                                     = InvalidLM msg


isValidInt :: String -> Bool
isValidInt maybeInt = case (readInt maybeInt) of
    ValidInt integer -> True
    InvalidInt -> False
    _ -> False

--effectively casts the timeStamp String into the TimeStamp type
readTimeStamp :: String -> TimeStamp
readTimeStamp timeStamp = read timeStamp 
      
--effectively casts the severity from a String to an Int      
readError :: String -> Int
readError severity = read severity :: Int   