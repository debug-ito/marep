-- |
-- Module: Data.MaRep
-- Description: Simple Match-and-Replace utilities for lists and texts
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- TODO: add documents. mention parsec, regex, pcre-light
module Data.MaRep
       ( replaceAll
       ) where

import Data.MaRep.Zipper (Decomposable(..))

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
replaceAll :: Decomposable a
           => (a -> Maybe a)
              -- ^ The matcher function. The argument is a substring
              -- of the input string. It should return 'Just' if the
              -- substring is matched. Content of 'Just' is the result
              -- of replacement.
           -> a -- ^ Input string
           -> a -- ^ Result of match-and-replace
replaceAll = undefined


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
