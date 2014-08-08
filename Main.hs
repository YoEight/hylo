module Main where

import Data.Foldable
import Data.Hylo
import Data.List.NonEmpty hiding (toList)
import qualified Control.Foldl as F

ua :: Unfold Int
ua = enumerated 1 10

ub :: Unfold Char
ub = enumerated 'a' 'l'

main :: IO ()
main = print $ hylo (droppedWhile (< 3) ua) F.list
