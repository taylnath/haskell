module Main where

import Buffer
import Editor
import JoinList
import Sized
import Scrabble

main = runEditor editor $ (fromString "hello" :: JoinList (Score, Size) String)