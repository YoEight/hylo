{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.Pandoc
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Hylo where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Foldable (Foldable, foldMap, toList)

--------------------------------------------------------------------------------
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Monoid(..))
import Data.Semigroup
import Data.Semigroup.Foldable

--------------------------------------------------------------------------------
import Control.Foldl hiding (head)

--------------------------------------------------------------------------------
data Pair a b = Pair !a !b

--------------------------------------------------------------------------------
data Unfold a = forall x. Unfold x (x -> Maybe (a, x))

--------------------------------------------------------------------------------
data UnfoldM m a = forall x. UnfoldM (m x) (x -> m (Maybe (a, x)))

--------------------------------------------------------------------------------
instance Functor Unfold where
    fmap f (Unfold x ana)
        = Unfold x $ \x' ->
              do (a, x'') <- ana x'
                 return (f a, x'')

--------------------------------------------------------------------------------
instance Applicative Unfold where
    pure a = Unfold () $ \() -> Just (a, ())

    Unfold sF anaF <*> Unfold sA anaA
        = Unfold (Pair sF sA) $ \(Pair xF xA) ->
              do (f, xF') <- anaF xF
                 (a, xA') <- anaA xA
                 return (f a, Pair xF' xA')

--------------------------------------------------------------------------------
instance Foldable Unfold where
    foldMap k (Unfold s ana)
        = loop mempty (ana s)
      where
        loop !m cs
            = case cs of
                Nothing -> m
                Just (a, s') -> loop (mappend m (k a)) (ana s')

--------------------------------------------------------------------------------
instance Monoid a => Monoid (Unfold a) where
    mempty = pure mempty
    mappend = liftA2 mappend

--------------------------------------------------------------------------------
after :: Unfold a -> Unfold a -> Unfold a
after (Unfold sX anaX) (Unfold sV anaV)
    = Unfold (Left sX) ana
  where
    ana (Left x) =
        case anaX x of
            Nothing       -> ana $ Right sV
            Just (la, x') -> Just (la, Left x')
    ana (Right v) =
        case anaV v of
            Nothing       -> Nothing
            Just (ra, v') -> Just (ra, Right v')

--------------------------------------------------------------------------------
source :: Foldable f => f a -> Unfold a
source fa
    = Unfold (toList fa) $ \xs ->
          case xs of
              a:as -> Just (a, as)
              []   -> Nothing

--------------------------------------------------------------------------------
repeated :: a -> Unfold a
repeated a = Unfold a (const $ Just (a,a))

--------------------------------------------------------------------------------
cycled :: Foldable1 f => f a -> Unfold a
cycled fa
    = Unfold s $ \xs ->
          case xs of
              []  -> Just (head s, tail s)
              h:t -> Just (h, t)
  where
    s = toList fa

--------------------------------------------------------------------------------
taken :: Int -> Unfold a -> Unfold a
taken n (Unfold x anaX) = Unfold (Pair 1 x) ana where
  ana (Pair i x')
      | i > n     = Nothing
      | otherwise =
          case anaX x' of
              Nothing       -> Nothing
              Just (a, x'') -> Just (a, Pair (succ i) x'')

--------------------------------------------------------------------------------
replicated :: Int -> a -> Unfold a
replicated n a = Unfold 0 ana where
  ana x | x > n     = Nothing
        | otherwise = Just (a, succ x)

--------------------------------------------------------------------------------
iterated :: (a -> a) -> a -> Unfold a
iterated k a = Unfold a $ \a' -> Just (a', k a')

--------------------------------------------------------------------------------
enumerated :: (Enum a, Ord a) => a -> a -> Unfold a
enumerated from to = Unfold from ana where
  ana i | i <= to   = Just (i, succ i)
        | otherwise = Nothing

--------------------------------------------------------------------------------
zipped :: Unfold a -> Unfold b -> Unfold (a, b)
zipped a b = (,) <$> a <*> b

--------------------------------------------------------------------------------
filtered :: (a -> Bool) -> Unfold a -> Unfold a
filtered k (Unfold sA anaA)
    = Unfold sA ana
  where
    ana x
        = case anaA x of
            Just (a, x')
                | k a       -> Just (a, x')
                | otherwise -> ana x'
            _ -> Nothing

--------------------------------------------------------------------------------
data InterState a
    = State1
    | State2
    | State3 a

interspersed :: a -> Unfold a -> Unfold a
interspersed a (Unfold sX anaX)
    = Unfold (Pair State1 sX) ana where
  ana (Pair st x)
      = case st of
            State3 a' -> Just (a', Pair State2 x)
            _         -> case anaX x of
                Just (a', x')
                    | State1 <- st -> Just (a', Pair State2 x')
                    | State2 <- st -> Just (a, Pair (State3 a') x')
                Nothing -> Nothing

--------------------------------------------------------------------------------
scanned :: (b -> a -> b) -> b -> Unfold a -> Unfold b
scanned k seed (Unfold sX anaX)
    = Unfold (Pair (Left seed) sX) ana
  where
    ana (Pair (Left b) x) = Just (b, Pair (Right b) x)
    ana (Pair (Right b) x)
        = case anaX x of
              Nothing     -> Nothing
              Just (a,x') -> let !b' = k b a in Just (b', Pair (Right b') x')

--------------------------------------------------------------------------------
dropped :: Int -> Unfold a -> Unfold a
dropped n (Unfold sX anaX)
    = Unfold (Pair 1 sX) ana
  where
    ana (Pair i x)
        | i > n = fmap (\(a, x') -> (a, Pair i x')) (anaX x)
        | otherwise =
            case anaX x of
                Nothing      -> Nothing
                Just (_, x') -> ana (Pair (succ i) x')

--------------------------------------------------------------------------------
takenWhile :: (a -> Bool) -> Unfold a -> Unfold a
takenWhile k (Unfold sX anaX)
    = Unfold sX ana
  where
    ana x
        | Just (a, x') <- anaX x, k a = Just (a, x')
        | otherwise                   = Nothing

--------------------------------------------------------------------------------
droppedWhile :: (a -> Bool) -> Unfold a -> Unfold a
droppedWhile k (Unfold sX anaX)
    = Unfold (Pair True sX) ana
  where
    ana (Pair False x) = fmap (\(a,x') -> (a, Pair False x')) (anaX x)
    ana (Pair _ x)
        = case anaX x of
              Nothing     -> Nothing
              Just (a,x')
                  | k a       -> ana (Pair True x')
                  | otherwise -> Just (a, Pair False x')

--------------------------------------------------------------------------------
transform :: (forall x. x -> m x)
          -> (forall x. m x -> (x -> Maybe (a, x)) -> Maybe (a, m x))
          -> Unfold a
          -> Unfold a
transform lf kf (Unfold sX anaX)
    = Unfold (lf sX) go
  where
    go mx = kf mx anaX

--------------------------------------------------------------------------------
adapt :: Monad m => UnfoldM m a -> Unfold a -> UnfoldM m a
adapt (UnfoldM mSx anaMx) (Unfold sX anaX)

--------------------------------------------------------------------------------
hylo :: Unfold a -> Fold a b -> b
hylo (Unfold sA ana) (Fold cata sB doneB)
    = loop sB (ana sA)
  where
    loop xB Nothing        = doneB xB
    loop xB (Just (a, xA)) = let !xB' = cata xB a in loop xB' (ana xA)
