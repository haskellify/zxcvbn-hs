{-# LANGUAGE TemplateHaskell #-}

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

module Text.Password.Strength.Internal.Token (
    -- * Splitting a Password into Tokens
    Token(..),
    allTokens,

    -- * Lenses for the 'Token' Type
    tokenChars,
    tokenLower,
    startIndex,
    endIndex,

    -- * Translate the Characters of a Password
    translateMap
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens.TH (makeLenses)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- | A token is a substring of a password.
data Token = Token
  { _tokenChars :: Text
  , _tokenLower :: Text
  , _startIndex :: Int
  , _endIndex   :: Int
  } deriving (Show, Eq, Ord)

makeLenses ''Token

--------------------------------------------------------------------------------
-- | Extract all substrings from the input 'Text'.  A substring has a
-- minimum character length of 3 for performance and to prevent false
-- positives for matches such as sequences and repeats.
--
-- Examples:
--
-- >>> map _tokenChars (allTokens "abcdef")
-- ["abc","abcd","abcde","abcdef","bcd","bcde","bcdef","cde","cdef","def"]
allTokens :: Text -> [Token]
allTokens = outer 0
  where
    outer :: Int -> Text -> [Token]
    outer i t
      | Text.null t = [ ]
      | otherwise   = inner i 2 t ++ outer (i+1) (Text.drop 1 t)

    inner :: Int -> Int -> Text -> [Token]
    inner i j t
      | Text.compareLength t (j+1) == LT = [ ]
      | otherwise = mkT i j t : inner i (j+1) t

    mkT :: Int -> Int -> Text -> Token
    mkT i j t =
      let chars = Text.take (j+1) t
      in Token chars (Text.toLower chars) i (i + j)

--------------------------------------------------------------------------------
-- | Translate the characters of a 'Text' value.
--
-- Given a function that translates a character into one or more
-- characters, return all possible translations.  Like zxcvbn, a
-- character is translated the same way everywhere in the text, so the
-- number of translations depends on the distinct characters and not
-- on the length of the text.
--
-- Examples:
--
-- >>> translateMap l33t2Eng "p111"
-- ["piii","plll"]
translateMap :: (Char -> String) -> Text -> [Text]
translateMap f t = map (\m -> Text.map (\c -> Map.findWithDefault c c m) t) subs
  where
    subs :: [Map.Map Char Char]
    subs = map Map.fromList $ sequence
      [ [ (c, c') | c' <- cs ] | c <- nub (Text.unpack t), let cs = f c, not (null cs) ]
