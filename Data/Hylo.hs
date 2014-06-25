{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE BangPatterns #-}
module Data.Hylo where

import Control.Monad
import Data.Monoid

data Ana a where
    Ana :: s -> (s -> Maybe (a, s)) -> Ana a

data Cata a b = Cata b (a -> b -> b)

instance Functor Ana where
    fmap f (Ana b k)
        = Ana b $ \b' ->
            case k b' of
                Nothing      -> Nothing
                Just (a,b'') -> Just (f a, b'')

runHylo :: Ana a -> Cata a b -> b
runHylo (Ana s nu) (Cata start mu)
    = loop (nu s) start where
      loop Nothing b      = b
      loop (Just (a,s')) b = let !b' = mu a b in loop (nu s') b'

fromTo :: Integer -> Integer -> Ana Integer
fromTo from to = Ana from go where
  go i
      | i > to    = Nothing
      | otherwise = Just (i, i+1)

adding :: Cata Integer Integer
adding = Cata 0 (+)

filtering :: (a -> Bool) -> Cata a b -> Cata a b
filtering k (Cata b f)
    = Cata b $ \a b -> if k a then f a b else b

foldMapping :: Monoid m => (a -> m) -> Cata a m
foldMapping f = Cata mempty go where go a b = mappend b (f a)

contramap :: (b -> a) -> Cata a c -> Cata b c
contramap k (Cata c f)
    = Cata c $ \b c' -> f (k b) c'

after :: Ana a -> Ana a -> Ana a
after (Ana starta ka) (Ana startb kb) = Ana (False, starta, startb) go where
  go (end, sa, sb)
      | end       = fmap (onRight sa) (kb sb)
      | otherwise = fmap (onLeft sb) (ka sa) `mplus`
                    fmap (onRight sa) (kb sb)

  onRight sa (b, sb') = (b, (True, sa, sb'))
  onLeft sb (a, sa')  = (a, (False, sa', sb))
