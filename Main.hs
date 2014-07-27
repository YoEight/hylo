module Main where

import Data.Foldable
import Data.Hylo
import Data.List.NonEmpty hiding (toList)
import qualified Control.Foldl as F

ua :: Unfold Int
ua = enumerated (-5) (-1)

ub :: Unfold Char
ub = enumerated 'a' 'l'

main :: IO ()
main = print $ hylo (after ua (taken 5 $ filtered even $ iterated succ 1)) F.list
