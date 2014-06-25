module Main where

import Data.Hylo

main :: IO ()
main = print $ runHylo (after (fromTo 1 10000) (fromTo 10001 20000)) adding
