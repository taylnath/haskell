{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Char

-- ToMessageType :: [Char] -> MessageType
-- charToMessageType c = 
--     case c of
--         'I' -> Info
--         'W' -> Warning
--         'E' -> Error

takeFourWords :: String -> [String]
takeFourWords s = 
    let w = words s
    in take 4 w

isNum :: String -> Bool
isNum s = and $ map isDigit s

parseMessage :: String -> LogMessage
parseMessage m = 
    let w = words m
    in case (length w) of
        0 -> Unknown m
        1 -> Unknown m
        _ -> let    mt = head w
                    ts = head $ tail w
                    s = unwords $ tail $ tail w
                    in case (isNum ts) of
                        False -> Unknown m
                        True -> case mt of
                            "I" -> LogMessage Info (read ts :: TimeStamp) s
                            "W" -> LogMessage Warning (read ts :: TimeStamp) s
                            "E" -> case (length $ words s) of
                                0 -> Unknown m
                                _ -> let    t = words s
                                            err = ts
                                            errts = head t
                                            errs = unwords $ tail t
                                            in case (isNum errts) of
                                            False -> Unknown m
                                            True -> LogMessage (Error (read err :: Int)) (read errts :: TimeStamp) errs
                            _ -> Unknown m

parse :: String -> [LogMessage]
parse f = map parseMessage $ lines f


main :: IO ()
main = do
   print $ takeFourWords "hello dude hey yo dude"
   print $ parseMessage "I am an I"
   print $ parseMessage "E 70 3 Way too many pickles"
   print $ parseMessage "E 2 562 help help"
   print $ parseMessage "I 29 la la la "


-- E 70 3 Way too many pickles