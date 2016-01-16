module P21 where

import Data.List
import Math.Combinat.Sets

data Equip = Equip { cost :: Int, dmg :: Int, arm :: Int } -- cost, damage, armor
           deriving (Show)

data Guy = Guy {hp :: Int, str :: Int, def :: Int} -- hp, damage, armor
         deriving (Eq, Show)

boss = Guy 103 9 2

fight :: Guy -> Guy -> Bool -- "does the first guy win?"
fight (Guy mHp mDmg mArm) (Guy bHp bDmg bArm)
  | bHp <= 0 = True
  | mHp <= 0 = False
  | True = fight (Guy mNewHp mDmg mArm) (Guy bNewHp bDmg bArm)
  where bDmg' = max 1 $ bDmg - mArm
        mDmg' = max 1 $ mDmg - bArm
        bNewHp = bHp - mDmg'
        mNewHp = mHp - bDmg'

weapons = [ Equip 8 4 0
          , Equip 10 5 0
          , Equip 25 6 0
          , Equip 40 7 0
          , Equip 74 8 0]

armors = [ Equip 13 0 1
         , Equip 31 0 2
         , Equip 53 0 3
         , Equip 75 0 4
         , Equip 102 0 5]

rings = [ Equip 25 1 0
        , Equip 50 2 0
        , Equip 100 3 0
        , Equip 20 0 1
        , Equip 40 0 2
        , Equip 80 0 3]

weaponChoices = choose 1 weapons
armorChoices = [] ++ choose 1 armors
ringChoices = [] ++ choose 1 rings ++ choose 2 rings

kits = map (concatMap id) $ [[w,a,r] | w <- weaponChoices, a <- armorChoices, r <- ringChoices]

loadoutCost kit = sum $ map cost kit

kitUp kit = Guy 100 (sum $ map dmg kit) (sum $ map arm kit)

p21_1 = do
  let wins = filter (\k -> fight (kitUp k) boss) kits
      sorted = sortOn loadoutCost wins
      cheapest = head sorted
    in print $ (loadoutCost cheapest, cheapest)

p21_2 = do
  let losses = filter (\k -> not $ fight (kitUp k) boss) kits
      sorted = sortOn loadoutCost losses
      priciest = last sorted
    in print $ (loadoutCost priciest, priciest)
