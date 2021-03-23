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

-- data LogMessage = LogMessage MessageType TimeStamp String
--                 | Unknown String
-- data MessageTree = Leaf
--                  | Node MessageTree LogMessage MessageTree
-- sorted by timestamp
insert :: LogMessage -> MessageTree -> MessageTree
insert lm mt = 
    case lm of 
        Unknown _ -> mt
        LogMessage _ ts _ -> case mt of
            Leaf -> Node Leaf lm Leaf
            Node mtL (LogMessage _ mtTS _) mtR | ts <= mtTS -> insert lm mtL
            Node mtL (LogMessage _ mtTS _) mtR | ts > mtTS -> insert lm mtR
            _ -> mt
plm :: LogMessage -> IO ()
plm lm = 
    case lm of 
        Unknown _ -> print "Whoops"
        LogMessage _ ts _ -> print ts
pmt :: LogMessage -> MessageTree -> IO()
pmt lm mt = 
    case lm of 
        Unknown _ -> print "Whoops"
        LogMessage _ ts _ -> case mt of 
            Leaf -> print "Leaf"
            Node _ (LogMessage _ mtTS _) _ | ts < mtTS -> print ts
            Node _ (LogMessage _ mtTS _) _ | ts >= mtTS -> print mtTS
            _ -> print "whelp"


main :: IO ()
main = do
--    print $ takeFourWords "hello dude hey yo dude"
--    plm $ parseMessage "I am an I"
--    plm $ parseMessage "E 70 3 Way too many pickles"
--    plm $ parseMessage "E 2 562 help help"
--    plm $ parseMessage "I 29 la la la "
--    pmt (parseMessage "I 29 la la la") (Leaf)
--    pmt (parseMessage "I 29 la la la") (Node Leaf (LogMessage Info 28 "bleah") Leaf)
   let x = insert (LogMessage Info 28 "wassup") Leaf
   print x
   let y = insert (LogMessage Info 27 "hello") x
   print y
   print x


-- E 70 3 Way too many pickles