module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Random
import Control.Monad
import Data.List
import System.IO.Unsafe

import Risk

main :: IO ()
main = hspec $ describe "Tests" $ do
    describe "Battle work?" $
      it "Army decrease after battle" $
        property $ checkBattle (10, 5) (unsafePerformIO . evalRandIO $ battle (Battlefield 10 5))

    describe "2:2 army units battle" $
      it "2:1 or 1:2" $
        property $ checkBattle2 (2, 1) (unsafePerformIO . evalRandIO $ battle (Battlefield 2 2))

    describe "How many rounds? #1" $
      it "2:1 - only 1 round" $
        property $ checkBattle3 (2, 1) 1 (unsafePerformIO . evalRandIO $ battle (Battlefield 2 1))

    describe "How many rounds? #2" $
      it "1:10 - 0 rounds" $
        property $ checkBattle3 (1,10) 0 (unsafePerformIO . evalRandIO $ battle (Battlefield 1 10))

    describe "How many rounds? #3" $
      it "2:2 - only 1 round" $
        property $ checkBattle3 (2,2) 1 (unsafePerformIO . evalRandIO $ battle (Battlefield 2 2))

    describe "One of armies destroyed after invade?" $
      it "Сheck Invasion" $ 
        property $ checkInvasion $ unsafePerformIO . evalRandIO $ invade (Battlefield 5 5)

    describe "Probability check #1" $
      it "1 probability with no defenders" $
        (unsafePerformIO . evalRandIO $ successProb (Battlefield 10 0)) `shouldBe` (1.0 :: Double)

    describe "Probability check #2" $
      it "0 probability with no attackers" $
         (unsafePerformIO . evalRandIO $ successProb (Battlefield 0 10)) `shouldBe` (0.0 :: Double)

    describe "Probability check #3" $
      it "from 0 to 1" $
        property $ checkSuccessProb (unsafePerformIO . evalRandIO $ successProb (Battlefield 25 50))

checkBattle :: (Army, Army) -> Battlefield -> Bool
checkBattle (a, d) bf = attackers bf < a || defenders bf < d

checkBattle2 :: (Army, Army) -> Battlefield -> Bool
checkBattle2 (num1, num2) bf = (attackers bf == num1 && defenders bf == num2) || (attackers bf == num2 && defenders bf == num1)

checkBattle3 :: (Army, Army) -> Int -> Battlefield -> Bool
checkBattle3 (a, d) num bf = ((a - attackers bf) + (d - defenders bf)) == num

checkInvasion :: Battlefield -> Bool
checkInvasion bf = attackers bf < 2 || defenders bf == 0

checkSuccessProb :: Double -> Bool 
checkSuccessProb probability = probability > 0 && probability < 1