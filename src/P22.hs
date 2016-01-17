module P22 where

import Data.List
import P21

data WizSim = WizSim { wiz :: Guy,
                       boss :: Guy,
                       mana :: Int,
                       effects :: [(Spell, Int)] }
            | Win
            | Lose
            deriving (Eq, Show)

data Spell = Missile | Drain | Shield | Poison | Recharge | EndShield
           deriving (Eq, Show)

me22 = Guy 50 0 0
boss22 = Guy 51 9 0
allSpells = [Missile, Drain, Shield, Poison, Recharge]

world22 = WizSim me22 boss22 500 []

world22test1 = WizSim (Guy 10 0 0) (Guy 13 8 0) 250 [(Poison, 2), (Shield, 1)]
world22test2 = WizSim (Guy 10 0 0) (Guy 14 8 0) 250 []

isDead (Guy hp _ _) = hp <= 0

faith w@(WizSim me b m es)
  | isDead b = Win
  | isDead me = Lose
  | m <= 0 = Lose
  | True = let
    w' = applyEffects w
    possibleSpells = filter (\s -> not $ elem s (map fst es)) allSpells
    outcomes = undefined
    in w'

applyEffects w@(WizSim me b m es) =
  clearEffects $ decrementEffects $ foldl apply w $ map fst es
  where decrementEffects (WizSim me b m es) =
          WizSim me b m $ map (\(a, n) -> (a, n - 1)) es
        clearEffects (WizSim me b m es) = let
          (fading, ongoing) = partition (\(_,n) -> n <= 0) es
          ongoing' = if elem Shield (map fst fading)
                     then (EndShield, 0) : ongoing
                     else ongoing
          in WizSim me b m ongoing'

{- spells:
     | Name           | Cost  | Effect
-----+----------------+-------+----------------
     | Magic Missile  | 53    | 4 damage to boss
     | Drain          | 73    | 2 damage to boss, heal 2
     | Shield         | 113   | armor +7 for 6 turns
     | Poison         | 173   | 3 damage to boss per turn, 6 turns
     | Recharge       | 229   | +101 mana per turn, 5 turns
-}

apply (WizSim (Guy hp dmg _) b m es) Shield =
  WizSim (Guy hp dmg (7)) b m es
apply (WizSim me b m es) Poison =
  WizSim me (hurt b 3) m es
apply (WizSim me b m es) Recharge =
  WizSim me b (m + 101) es
apply (WizSim me b m es) EndShield =
  WizSim (me { def = 0 }) b m es
apply w _ = w

cast Missile (WizSim me b m es) =
  WizSim me (hurt b 4) (m - 53) es

cast Drain (WizSim me b m es) =
  WizSim (heal me 2) (hurt b 2) (m - 73) es

cast Shield (WizSim me b m es) =
  WizSim me b (m - 113) ((Shield, 6) : es)

cast Poison (WizSim me b m es) =
  WizSim me b (m - 173) ((Poison, 6) : es)

cast Recharge (WizSim me b m es) =
  WizSim me b (m - 229) ((Recharge, 5) : es)

heal (Guy hp dmg arm) n = Guy (hp + n) dmg arm
hurt (Guy hp dmg arm) n = Guy (hp - n) dmg arm
