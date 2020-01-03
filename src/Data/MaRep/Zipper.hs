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

-- | One-dimensional zipper over a string with a cursor.
--
-- The cursor can span zero or more elements in the string. Inside the
-- zipper, the string type @a@ is treated like it's non-empty. If an
-- empty part is expressed as 'Nothing'.
data Zipper a =
  Zipper
  { zipTop :: Maybe a, -- ^ the top part
    zipCursor :: Maybe a, -- ^ the cursor
    zipBottom :: Maybe a -- ^ the bottom part
  }
  deriving (Show,Eq,Ord,Functor)

toMaybe :: Decomposable a => a -> Maybe a
toMaybe s = if isEmpty s then Nothing else Just s

-- | Init a zipper with the cursor being empty at the top.
initTop :: Decomposable a => a -> Zipper a
initTop s = Zipper Nothing Nothing (toMaybe s)

-- | Init a zipper with the cursor being empty at the bottom.
initBottom :: Decomposable a => a -> Zipper a
initBottom s = Zipper (toMaybe s) Nothing Nothing
