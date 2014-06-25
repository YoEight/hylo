{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE BangPatterns #-}
module Data.Hylo where

data Ana a where
    Ana :: b -> (b -> Maybe (a, b)) -> Ana a

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

source :: Integer -> Integer -> Ana Integer
source from to = Ana from go where
  go i
      | i > to    = Nothing
      | otherwise = Just (i, i+1)

adding :: Cata Integer Integer
adding = Cata 0 (+)


filtering :: (a -> Bool) -> Cata a b -> Cata a b
filtering k (Cata b f)
    = Cata b $ \a b -> if k a then f a b else b

contramap :: (b -> a) -> Cata a c -> Cata b c
contramap k (Cata c f)
    = Cata c $ \b c' -> f (k b) c'

-- cons :: a -> b -> Cons a b
-- cons a b = Cons $ \_ onCons -> onCons a b

-- nil :: Cons a b
-- nil = Cons $ \onNil _ -> onNil

-- unCons :: x -> (a -> b -> x) -> Cons a b -> x
-- unCons onNil onCons (Cons k) = k onNil onCons

-- fromTo_mu_adding :: Integer -> Integer -> Acc
-- fromTo_mu_adding from to = hylo (source from to) crush from where

-- source :: Integer -> Integer -> Integer -> Cons Integer Integer
-- source from to !i
--     | i > to    = nil
--     | otherwise = let x = i+1 in cons i x

-- crush :: Cons Integer Acc -> Acc
-- crush xs = unCons (Acc 0) (\i (Acc a) -> let x = i+a in Acc x) xs
