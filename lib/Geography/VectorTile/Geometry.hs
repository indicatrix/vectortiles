{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module    : Geography.VectorTile.Geometry
-- Copyright : (c) Azavea, 2016
-- License   : Apache 2
-- Maintainer: Colin Woodbury <cwoodbury@azavea.com>

module Geography.VectorTile.Geometry
  ( -- * Geometries
    -- ** Types
    Point, x, y
  , LineString(..)
  , Polygon(..)
  -- ** Operations
  , area
  , surveyor
  , distance
  ) where

import           Control.DeepSeq (NFData)
import qualified Data.Foldable   as Foldable
import qualified Data.Sequence   as Seq
import           GHC.Generics    (Generic)

---

-- | Points in space. Using "Record Pattern Synonyms" here allows us to treat
-- `Point` like a normal ADT, while its implementation remains an unboxed
-- @(Int,Int)@.
type Point = (Int,Int)
pattern Point :: Int -> Int -> (Int, Int)
pattern Point{x, y} = (x, y)

-- | /newtype/ compiles away to expose only the `U.Vector` of unboxed `Point`s
-- at runtime.
newtype LineString = LineString { lsPoints :: Seq.Seq Point } deriving (Eq,Show,Generic)

instance NFData LineString

-- | A polygon aware of its interior rings.
data Polygon = Polygon { polyPoints :: Seq.Seq Point
                       , inner      :: Seq.Seq Polygon } deriving (Eq,Show,Generic)

instance NFData Polygon

{-
-- | Very performant for the same reason as `LineString`.
newtype Polygon = Polygon { points :: U.Vector Point } deriving (Eq,Show)
-}

-- | The area of a `Polygon` is the difference between the areas of its
-- outer ring and inner rings.
area :: Polygon -> Double
area p = surveyor (polyPoints p) + sum (fmap area $ inner p)

-- | The surveyor's formula for calculating the area of a `Polygon`.
-- If the value reported here is negative, then the `Polygon` should be
-- considered an Interior Ring.
--
-- Assumption: The `U.Vector` given has at least 4 `Point`s.
-- surveyor :: U.Vector Point -> Float
-- surveyor v = (/ 2) . fromIntegral . U.sum $ U.zipWith3 (\xn yn yp -> xn * (yn - yp)) xs yns yps
--   where v' = U.init v
--         xs = fmap x v'
--         yns = fmap y . U.tail $ U.snoc v' (U.head v')
--         yps = fmap y . U.init $ U.cons (U.last v') v'
surveyor :: Seq.Seq Point -> Double
surveyor (v'@((v'head Seq.:<| _) Seq.:|> v'last) Seq.:|> _) = (/ 2) . fromIntegral . Foldable.foldl' (+) 0 $ Seq.zipWith3 (\xn yn yp -> xn * (yn - yp)) xs yns yps
  where xs = fmap x v'
        (_ Seq.:<| tailYns) = (Seq.|>) v' v'head
        (initYps Seq.:|> _) = (Seq.<|) v'last v'
        yns = fmap y tailYns
        yps = fmap y initYps

-- | Euclidean distance.
distance :: Point -> Point -> Float
distance p1 p2 = sqrt . fromIntegral $ dx ^ 2 + dy ^ 2
  where dx = x p1 - x p2
        dy = y p1 - y p2
