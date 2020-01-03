{-# LANGUAGE DeriveFunctor #-}
-- |
-- Module: Data.MaRep.Zipper
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __Internal module__
module Data.MaRep.Zipper
  ( Decomposable(..),
    Zipper(..),
    init
  ) where

import Prelude hiding (init)

import qualified Data.List as List
import Data.Monoid (Monoid(..))

-- | Types that can be decomposed at the top or bottom.
class Decomposable a where
  -- | Decompose the top of the input data. The result is (top
  -- element, rest). It returns 'Nothing' if the input data is empty.
  removeTop :: a -> Maybe (a, a)
  -- | Decompose the bottom of the input data. The result is (rest,
  -- bottom element). It returns 'Nothing' if the input data is empty.
  removeBottom :: a -> Maybe (a, a)

instance Decomposable [a] where
  removeTop [] = Nothing
  removeTop (x : rest) = Just ([x], rest)
  removeBottom [] = Nothing
  removeBottom (x : rest) =
    case removeBottom rest of
      Nothing -> Just ([], [x])
      Just (rrest, rbottom) -> Just ([x] ++ rrest, rbottom)

-- | One-dimensional zipper.
data Zipper a =
  Zipper
  { zipHead :: a,
    zipBody :: a,
    zipTail :: a
  }
  deriving (Show,Eq,Ord,Functor)

init :: Monoid a => a -> Zipper a
init a = Zipper mempty mempty a
