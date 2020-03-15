{-
 - Written between 7 and 14 April, 2014.
 - An implementation of a discrete random variable as a monad.
-}

module Stochastic (
    Stochastic (..)
    ,toMap) where

import Data.Map (Map, fromListWith)

newtype Stochastic a = WithProbabilities [(a, Double)]

toMap :: (Ord a) => Stochastic a -> Map a Double
toMap (WithProbabilities distribution)
    = fromListWith (+) distribution

instance (Show a, Ord a) => Show (Stochastic a) where
    show distribution = show (toMap distribution)

instance Functor Stochastic where
    fmap f (WithProbabilities distribution)
        = WithProbabilities $ map (\(a,b) -> (f a, b)) distribution

join :: Stochastic (Stochastic a) -> Stochastic a
join (WithProbabilities distribution)
    = WithProbabilities $ concatMap multiplyProbability distribution
        where multiplyProbability (WithProbabilities innerDistribution, p)
                = map (\(a, b) -> (a, p * b)) innerDistribution

instance Monad Stochastic where
    distribution >>= f = join $ fmap f distribution

    return x = WithProbabilities [(x, 1.0)]

