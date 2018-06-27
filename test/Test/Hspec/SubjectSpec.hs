{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Test.Hspec.SubjectSpec (spec) where

import           Test.Hspec

import           Test.Hspec.Subject

spec :: Spec
spec = do
  [describe_|23 + 42|] $ do
    [it_|`shouldBe` 65|]
