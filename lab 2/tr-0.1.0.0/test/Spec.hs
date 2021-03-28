-- | Test Haskell tr implementation.
--
-- We provide a few very simple tests here as a demonstration. You should add
-- more of your own tests!
--
-- Run the tests with `stack test`.
module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Tr

type CharSet' = NonEmptyList Char

tr' :: CharSet -> CharSet -> String -> String
tr' inset outset = tr inset (Just outset)

trd' :: CharSet -> String -> String
trd' inset = tr inset Nothing

-- | Test harness.
main :: IO ()
main = hspec $ describe "Testing tr" $ do
    describe "single translate" $
      it "a -> b" $
        tr' "a" "b" "a" `shouldBe` "b" 
        
    
    describe "single translate (nothing happens)" $
      it "a -> b" $
        tr' "a" "c" "c" `shouldBe` "c"
        
    describe "single delete (nothing happens)" $
      it "delete a" $
        trd' "a" "b" `shouldBe` "b"

    describe "single delete #2" $
      it "delete a" $
        trd' "a" "a" `shouldBe` ""

    describe "stream translate" $
      it "a -> b" $
        tr' "a" "b" "aaaa" `shouldBe` "bbbb"

    describe "stream delete" $
      it "delete a" $
        trd' "a" "aabb" `shouldBe` "bb"

    describe "extend input set" $
      it "abc -> d" $
        tr' "abc" "d" "abcd" `shouldBe` "dddd"

    describe "extend input set" $
      it "delete abc" $
        trd' "abc" "abcd" `shouldBe` "d"

    describe "tr quick-check" $
      it "empty input is identity" $ property prop_empty_id

    describe "tr quick-check for delete" $
      it "empty input is identity" $ property prop_empty_id_delete
      
-- | An example QuickCheck test. Tests the invariant that `tr` with an empty
-- input string should produce and empty output string.
prop_empty_id :: CharSet' -> CharSet' -> Property
prop_empty_id (NonEmpty set1) (NonEmpty set2)
  = tr' set1 set2 "" === ""

prop_empty_id_delete :: CharSet' -> Property
prop_empty_id_delete (NonEmpty set)
  = trd' set "" === ""

