#!/usr/bin/env stack
-- stack --resolver ghc-8.6.3 script --package QuickCheck

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List

newtype DieValue = DV { unDV :: Int } deriving (Eq, Ord, Show)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

type Army = Int 

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

setAttackers :: Army -> Army
setAttackers n 
  | n >= 4    = 3
  | n == 3    = 2
  | n == 2    = 1
  | otherwise = 0

setDefenders :: Army -> Army
setDefenders n 
  | n >= 2    = 2
  | n == 1    = 1
  | otherwise = 0

rollDice :: Army -> Rand StdGen [DieValue]
rollDice n = replicateM n die

sortedDiceRolls :: Army -> Rand StdGen [DieValue]
sortedDiceRolls n = fmap(reverse . sort) (rollDice n)

resultfOfBattle :: [DieValue] -> [DieValue] -> Battlefield -> Battlefield
resultfOfBattle [] _ (Battlefield a d) = Battlefield a d
resultfOfBattle _ [] (Battlefield a d) = Battlefield a d
resultfOfBattle (aPoint:attackersPoints) (dPoint:defendersPoints) (Battlefield a d)
  | aPoint > dPoint     = resultfOfBattle attackersPoints defendersPoints (Battlefield (a-1) d)
  | otherwise = resultfOfBattle attackersPoints defendersPoints (Battlefield a (d-1))

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do 
  attackersPoints <- sortedDiceRolls (setAttackers a)
  defendersPoints <- sortedDiceRolls (setDefenders d)
  return $ resultfOfBattle attackersPoints defendersPoints (Battlefield a d)

invade :: Battlefield -> Rand StdGen Battlefield
invade (Battlefield a d) 
  | a >= 2 && d /= 0 = battle (Battlefield a d) >>= invade  
  | otherwise = return $ Battlefield a d

successProb :: Battlefield -> Rand StdGen Double
successProb (Battlefield a d)  = do
  invasions <- replicateM 1000 (invade (Battlefield a d))
  return $ fromIntegral(length $ filter((<=0).defenders) invasions) / 1000

startGame :: IO ()
startGame = do
  single_invasion <- evalRandIO $ invade (Battlefield 0 10)
  thousand_invasions <- evalRandIO $ successProb (Battlefield 0 10)
  print "Single invasion result:" 
  print single_invasion
  print "Thousand invasions probability:" 
  print thousand_invasions