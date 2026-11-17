{-# LANGUAGE OverloadedStrings #-}

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
module Zxcvbn.L33t
  ( test
  ) where

--------------------------------------------------------------------------------
import Control.Lens ((^.))
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
import Text.Password.Strength.Internal

--------------------------------------------------------------------------------
test :: TestTree
test = testGroup "L33t"
  [ testCase "translates a character the same way everywhere" $
      translateMap l33t2Eng "p111" @?= ["piii", "plll"]
  , testCase "keeps characters without a translation" $
      translateMap l33t2Eng "word" @?= ["word"]
  , testCase "long run of one character" $
      map (^. l33tText) (l33t (Token long long 0 199))
        @?= [Text.replicate 200 "i", Text.replicate 200 "l"]
  ]

  where
    long = Text.replicate 200 "1"
