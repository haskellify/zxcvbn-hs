{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
module Zxcvbn.Repeat
  ( test
  ) where

--------------------------------------------------------------------------------
import Control.Lens ((^.))
import Data.Function (on)
import Data.List (maximumBy, nub, sortBy, subsequences)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog hiding (test)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Text.Password.Strength.Internal

--------------------------------------------------------------------------------
test :: TestTree
test = testGroup "Repeat"
  [ tc "wordword"      "word" $ Just (2, Token "wordword" "wordword" 0 7)
  , tc "wordword@word" "word" $ Just (2, Token "wordword" "wordword" 0 7)
  , tc "word@wordword" "word" $ Just (2, Token "wordword" "wordword" 5 12)
  , tc "word2word"     "word" Nothing
  , tc "word"          "word" Nothing
  , tc "abcdefg"       "abc"  Nothing
  , testCase "long run of one character" $
      repeatMatch (rmap (Text.replicate 200 "a")) (Token "aaa" "aaa" 0 0)
        @?= Just (66, Token (Text.replicate 66 "aaa") (Text.replicate 66 "aaa") 2 199)
  , testProperty "same as trying all subsequences" prop_same_as_subsequences
  ]

  where
    tc :: Text -> Text -> Maybe (Int, Token) -> TestTree
    tc p t r = testCase (Text.unpack p) $
      repeatMatch (rmap p) (Token t (Text.toLower t) 0 0) @?= r

    tmap :: Text -> Map Token ()
    tmap = Map.fromList . map (,()) . allTokens

    rmap :: Text -> RepeatMap
    rmap = mkRepeatMap . tmap

--------------------------------------------------------------------------------
-- | Compare 'repeatMatch' with a reference that tries every
-- subsequence of the occurrences, on passwords short enough for it.
prop_same_as_subsequences :: Property
prop_same_as_subsequences =
  property $ do
    password <- forAll $ Gen.text (Range.linear 1 14) (Gen.element ['a', 'A', 'b'])
    let ts = allTokens password
        rmap = mkRepeatMap (Map.fromList (map (,()) ts))
    mapM_ (\c -> repeatMatch rmap (Token c (Text.toLower c) 0 0)
                    === reference ts c)
          (nub (map (^. tokenChars) ts))

--------------------------------------------------------------------------------
reference :: [Token] -> Text -> Maybe (Int, Token)
reference ts c =
  let occurrences = sortBy (compare `on` (^. startIndex))
                      (filter ((== c) . (^. tokenChars)) ts)
      adjacent xs = and (zipWith (\x y -> (y ^. startIndex) - (x ^. endIndex) == 1)
                                 xs (drop 1 xs))
      runs = filter ((>= 2) . length) (filter adjacent (subsequences occurrences))
  in case runs of
       [] -> Nothing
       _  -> case maximumBy (compare `on` length) runs of
               r@(s:_) -> let n = length r
                          in Just (n, Token (Text.replicate n c)
                                            (Text.replicate n (Text.toLower c))
                                            (s ^. startIndex)
                                            (last r ^. endIndex))
               [] -> Nothing
