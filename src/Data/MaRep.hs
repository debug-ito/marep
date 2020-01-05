-- |
-- Module: Data.MaRep
-- Description: Simple Match-and-Replace utilities for lists and texts
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- TODO: add documents. mention parsec, regex, pcre-light
module Data.MaRep
       ( replaceAll,
         Decomposable(..)
       ) where

import Data.Monoid (Monoid, (<>))

import Data.MaRep.Zipper
  ( Decomposable(..),
    Zipper(..), initTop
  )

-- | Match all substrings in the input string with the matcher
-- function, replace the matched substrings with the result of the
-- matcher function, and return the replacement result.
--
-- Substrings are decomposed of the input string and sequentially fed
-- into the matcher function. The order of matching is (1) from the
-- start to the end of the input string (2) from one-element long
-- substring to the longest substring that covers the rest of the
-- input string. Once the matcher function returns 'Just', the
-- match-and-replace procedure is repeated on the rest of the input
-- string again.
--
-- The match-and-replace is performed on non-overlapping substrings of
-- the input string. Replacement results are not matched again.
--
-- >>> replaceAll (\x -> if x == "f" then Just "fff" else Nothing) ("" :: String)
-- ""
-- >>> replaceAll (\x -> if x == "f" then Just "fff" else Nothing) ("uuu" :: String)
-- "uuu"
-- >>> replaceAll (\x -> if x == "f" then Just "fff" else Nothing) ("uffu" :: String)
-- "uffffffu"
-- >>> replaceAll (\x -> if x == "f" then Just "fff" else Nothing) ("fuuf" :: String)
-- "fffuufff"
-- >>> replaceAll (\x -> case x of "f" -> Just "fff"; "go" -> Just "gg"; "hii" -> Just "H"; _ -> Nothing) ("gfgoihgfhiig" :: String)
-- "gfffggihgfffHg"
replaceAll :: Decomposable a
           => (a -> Maybe a)
              -- ^ The matcher function. The argument is a non-empty
              -- substring of the input string. It should return
              -- 'Just' if the substring is matched. Content of 'Just'
              -- is the result of replacement.
           -> a -- ^ Input string
           -> a -- ^ Result of match-and-replace
replaceAll matcher input =
  case maRepStart matcher input of
    Nothing -> input
    Just (rep, mbottom) ->
      case fmap (replaceAll matcher) mbottom of
        Nothing -> rep
        Just rest -> rep <> rest

-- | Match and replace once from the start of the input string. If
-- match is found, it returns the replacement result and the zipper at
-- that moment.
matchStart :: Decomposable a
           => (a -> Maybe b) -- ^ The matcher against non-empty substrings
           -> a -- ^ The input string
           -> Maybe (b, Zipper a)
matchStart = undefined

-- | Like 'matchStart', but this one concatenates the replacement
-- results to other parts. If match is found, it returns
-- (concatenation of top part and replacement, bottom part if any).
maRepStart :: Decomposable a
           => (a -> Maybe a)
           -> a
           -> Maybe (a, Maybe a)
maRepStart = undefined

-- | Starting from the zipper, apply the matcher against the cursor
-- repeatedly by enlarging the span of the cursor until it reaches the
-- end of the string. If match is found, it returns the replacement
-- result and the zipper of that moment. If match is not found, it
-- returns 'Nothing'.
matchLonger :: Decomposable a
            => (a -> Maybe b) -- ^ The matcher
            -> Zipper a -- ^ Starting zipper
            -> Maybe (b, Zipper a)
matchLonger = undefined

-- TODO
--
-- MaRep can be (1) greedy or non-greedy (2) from the start or from
-- the end. So, in theory we have four possible modes. 'replaceAll' is
-- non-greedy and from the start, but in future maybe we should
-- support all three modes.
--
-- With non-greedy and "from the start" mode, MaRep can be performed
-- on data stream with no end of data. To do that, the matcher should
-- be able to signal 'Skip', indicating it won't match longer
-- substrings at that position.
--
-- Maybe we should add MaRep version of 'breakOn' and 'splitOn'.
