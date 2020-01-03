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

-- | Types that can be decomposed at the top or bottom.
class Decomposable a where
  -- | Decompose the top of the input data. The result is (top
  -- element, rest). It returns 'Nothing' if the input data is empty.
  removeTop :: a -> Maybe (a, a)
  -- | Decompose the bottom of the input data. The result is (rest,
  -- bottom element). It returns 'Nothing' if the input data is empty.
  removeBottom :: a -> Maybe (a, a)
  -- | Return 'True' is the input is empty.
  isEmpty :: a -> Bool

instance Decomposable [a] where
  removeTop [] = Nothing
  removeTop (x : rest) = Just ([x], rest)
  removeBottom [] = Nothing
  removeBottom (x : rest) =
    case removeBottom rest of
      Nothing -> Just ([], [x])
      Just (rrest, rbottom) -> Just ([x] ++ rrest, rbottom)
  isEmpty = List.null

-- | One-dimensional zipper over a sequence with a cursor.
--
-- The cursor can span zero or more elements in the sequence. Inside
-- the zipper, the sequence type @a@ is treated like it's
-- non-empty. If the span of the cursor is zero, it's 'Nothing'.
data Zipper a =
    ZEmpty -- ^ The input sequence is empty.
  | ZTop (Maybe a) a -- ^ The cursor is at the top. Keeps the cursor and the bottom part.
  | ZMiddle a (Maybe a) a -- ^ The cursor is at the middle. Keeps the top, cursor and bottom part.
  | ZBottom a (Maybe a) -- ^ The cursor is at the bottom. Keeps the top part and cursor.

-- | Init a zipper with the cursor being empty at the top.
initTop :: Decomposable a => a -> Zipper a
initTop s = if isEmpty s then ZEmpty else ZTop Nothing s

-- | Init a zipper with the cursor being empty at the bottom.
initBottom :: Decomposable a => a -> Zipper a
initBottom s = if isEmpty s then ZEmpty else ZBottom s Nothing
