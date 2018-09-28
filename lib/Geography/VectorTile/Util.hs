{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Geography.VectorTile.Util
-- Copyright : (c) Azavea, 2016
-- License   : Apache 2
-- Maintainer: Colin Woodbury <cwoodbury@azavea.com>

module Geography.VectorTile.Util where

import qualified Data.Sequence as Seq
import           Data.Text     (Text)

---

-- | A sort of "self-zip", forming pairs from every two elements in a list.
-- Fails if there is an uneven number of elements.
pairs :: [a] -> Either Text [(a,a)]
pairs []       = Right []
pairs [_]      = Left "Uneven number of parameters given."
pairs (x:y:zs) = ((x,y) :) <$>  pairs zs

-- | Flatten a list of pairs. Equivalent to:
--
-- > ps ^.. each . both
unpairs :: [(a,a)] -> [a]
unpairs = foldr (\(a,b) acc -> a : b : acc) []

-- | Apply a pure function to both elements of a tuple.
both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

-- | Convert a `Maybe` to an `Either`, with some given default value
-- should the result of the `Maybe` be `Nothing`.
mtoe :: a -> Maybe b -> Either a b
mtoe _ (Just b) = Right b
mtoe a _        = Left a

-- | A sort of "self-zip", forming pairs from every two elements in a list.
-- Fails if there is an uneven number of elements.
-- pairsWith :: (a -> Int) -> [a] -> Either Text [Point]
-- pairsWith _ [] = Right []
-- pairsWith f s | odd $ length s = Left "Uneven number of parameters given."
--               | otherwise = Right $ go Empty s
--   where go !acc Empty = acc
--         go !acc (a :<| b :<| cs) = go (acc |> Point (f a) (f b)) cs
--         go !acc (_ :<| Empty) = acc

pairsWith :: (a -> Int) -> [a] -> Seq.Seq (Int,Int)
pairsWith f = Seq.unfoldr g
  where g []       = Nothing
        g [_]      = Nothing
        g (a:b:cs) = Just ((f a, f b), cs)

