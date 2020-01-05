{-# LANGUAGE DeriveFunctor #-}
-- |
-- Module: Data.MaRep.Zipper
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use this.__
module Data.MaRep.Zipper
  ( Decomposable(..),
    Zipper(..),
    initTop,
    initBottom
  ) where

import qualified Data.List as List
import Data.Monoid (Monoid(..))

-- | Types that can be decomposed at the top or bottom.
--
-- 'mempty' cannot be decomposed.
--
-- prop> removeTop mempty == Nothing
-- prop> removeBottom mempty == Nothing
--
-- Decomposed elements can be put back by 'mappend'.
--
-- prop> case removeTop x    of Nothing -> x == mempty ; Just (a, b) -> x == a `mappend` b && a /= mempty
-- prop> case removeBottom x of Nothing -> x == mempty ; Just (a, b) -> x == a `mappend` b && b /= mempty
class Monoid a => Decomposable a where
  -- | Decompose the top of the input data. The result is (top
  -- element, rest). It returns 'Nothing' if the input data is empty.
  removeTop :: a -> Maybe (a, a)
  -- | Decompose the bottom of the input data. The result is (rest,
  -- bottom element). It returns 'Nothing' if the input data is empty.
  removeBottom :: a -> Maybe (a, a)

-- Implementation note:
--
-- We don't support non-Monoid strings (like Data.List.NonEmpty) for
-- Decomposable and MaRep operations. Decomposable instance for
-- NonEmpty would be possible, but if we allowed it, we could not
-- specify almost any property about Decomposable methods.

instance Decomposable [a] where
  removeTop [] = Nothing
  removeTop (x : rest) = Just ([x], rest)
  removeBottom [] = Nothing
  removeBottom (x : rest) =
    case removeBottom rest of
      Nothing -> Just ([], [x])
      Just (rrest, rbottom) -> Just ([x] ++ rrest, rbottom)

-- | One-dimensional zipper over a string with a cursor.
--
-- The cursor can span zero or more elements in the string.
data Zipper a =
  Zipper
  { zipTop :: a, -- ^ the top part
    zipCursor :: a, -- ^ the cursor
    zipBottom :: a -- ^ the bottom part
  }
  deriving (Show,Eq,Ord,Functor)

-- | Init a zipper with the cursor being empty at the top.
initTop :: Decomposable a => a -> Zipper a
initTop s = Zipper mempty mempty s

-- | Init a zipper with the cursor being empty at the bottom.
initBottom :: Decomposable a => a -> Zipper a
initBottom s = Zipper s mempty mempty
