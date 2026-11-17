{-|

Copyright:
  This file is part of the package zxcvbn-hs. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/sthenauth/zxcvbn-hs

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: MIT

-}
module Text.Password.Strength.Internal.Repeat (
  -- * Repeating Token Matches
  RepeatMap,
  Repeat,
  mkRepeatMap,
  repeatMatch
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((^.))
import Data.Function (on)
import Data.List (sortBy, maximumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Password.Strength.Internal.Token

--------------------------------------------------------------------------------
-- | Internal mapping of repeating tokens.
newtype RepeatMap = RepeatMap
  { getMap :: Map Text [Token] }

--------------------------------------------------------------------------------
-- | Type alias for a count of repeating tokens.
type Repeat = Int

--------------------------------------------------------------------------------
-- | Generate a repeat map from an existing token map.
mkRepeatMap :: Map Token a -> RepeatMap
mkRepeatMap = RepeatMap . Map.foldrWithKey f Map.empty
  where f t _ = Map.insertWith (<>) (t ^. tokenChars) [t]

--------------------------------------------------------------------------------
-- | Test to see if the given token is repeated.
--
-- If a repeat is found, the number of occurrences is returned along
-- with the full token representing the repeating sequence.
--
-- In other words, if the token passed in is "word" and in the map we
-- find that the original password contains "wordword", we return 2 to
-- indicate 2 repeats and the token that represents the sequence
-- "wordword".
repeatMatch :: RepeatMap -> Token -> Maybe (Repeat, Token)
repeatMatch m t =
    mkToken <$>
      (Map.lookup (t ^. tokenChars) (getMap m) >>=
        ordered >>=
          longestSequence)
  where
    ordered :: [Token] -> Maybe [Token]
    ordered []  = Nothing
    ordered [_] = Nothing -- Must have at least two elements to repeat.
    ordered xs  = Just $ sortBy (compare `on` (^. startIndex)) xs

    -- The longest run of back-to-back tokens.  When several runs have
    -- the same length the last one wins.
    longestSequence :: [Token] -> Maybe (Repeat, Token, Token)
    longestSequence ts =
      case filter (\(n, _, _) -> n >= 2) (runs ts) of
        [] -> Nothing
        xs -> Just $ maximumBy (compare `on` (\(n, _, _) -> n)) xs

    -- For every token (ordered by start index), the length and the
    -- first token of the longest run that ends with it.  Each lookup
    -- only needs the run ending right before the token starts, so
    -- this is linear in the number of tokens instead of trying all
    -- of their subsequences.
    runs :: [Token] -> [(Repeat, Token, Token)]
    runs = go Map.empty
      where
        go :: Map Int (Repeat, Token) -> [Token] -> [(Repeat, Token, Token)]
        go _ [] = []
        go ends (x:xs) =
          let (n, s) = maybe (1, x) (\(k, f) -> (k + 1, f))
                         (Map.lookup ((x ^. startIndex) - 1) ends)
          in (n, s, x) : go (Map.insert (x ^. endIndex) (n, s) ends) xs

    mkToken :: (Repeat, Token, Token) -> (Repeat, Token)
    mkToken (n, s, e) =
      let c = Text.replicate n (t ^. tokenChars)
          l = Text.replicate n (t ^. tokenLower)
      in (n, Token c l (s ^. startIndex) (e ^. endIndex))
