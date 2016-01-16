module P22 where

import Data.List
import P21

data WizSim = WizSim { wiz :: Guy,
                       boss :: Guy,
                       cost :: Int,
                       mana :: Int,
                       effects :: [(Spell, Int)] }
            | Win
            | Lose
            deriving (Eq, Show)

data Spell = Missile | Drain | Shield | Poison | Recharge
           deriving (Eq, Show)

me22 = Guy 50 0 0
boss22 = Guy 51 9 0
allSpells = [Missile, Drain, Shield, Poison, Recharge]

world22 = WizSim me22 boss22 0 500 []

world22test1 = WizSim (Guy 10 0 0) (Guy 13 8 0) 0 250 [(Poison, 2), (Shield, 1)]
world22test2 = WizSim (Guy 10 0 0) (Guy 14 8 0) 0 250 []

isDead (Guy hp _ _) = hp <= 0

faith w@(WizSim me b c m es)
  | isDead b = Win
  | isDead me = Lose
  | m <= 0 = Lose
  | True = let
    w' = applyEffects w
    possibleSpells = filter (\s -> not $ elem s (map fst es)) allSpells
    outcomes = undefined
    in w'

applyEffects w@(WizSim me b c m es) =
  clearEffects $ decrementEffects $ foldl apply w $ map fst es
  where decrementEffects (WizSim me b c m es) =
          WizSim me b c m $ map (\(a, n) -> (a, n - 1)) es
        clearEffects (WizSim me b c m es) = let
          (dying, ongoing) = partition (\(_,n) -> n < 0) es
          me' = if elem Shield (map fst dying)
                then me { def = 0 }
                else me
          in WizSim me' b c m ongoing

{- spells:
     | Name           | Cost  | Effect
-----+----------------+-------+----------------
     | Magic Missile  | 53    | 4 damage to boss
     | Drain          | 73    | 2 damage to boss, heal 2
     | Shield         | 113   | armor +7 for 6 turns
     | Poison         | 173   | 3 damage to boss per turn, 6 turns
     | Recharge       | 229   | +101 mana per turn, 5 turns
-}

apply (WizSim (Guy hp dmg _) b c m es) Shield =
  WizSim (Guy hp dmg (7)) b c m es
apply (WizSim me b c m es) Poison =
  WizSim me (hurt b 3) c m es
apply (WizSim me b c m es) Recharge =
  WizSim me b c (m + 101) es
apply w _ = w

cast Missile (WizSim me b c m es) =
  WizSim me (hurt b 4) (c + 53) (m - 53) es

cast Drain (WizSim me b c m es) =
  WizSim (heal me 2) (hurt b 2) (c + 73) (m - 73) es

cast Shield (WizSim me b c m es) =
  WizSim me b (c + 113) (m - 113) ((Shield, 6) : es)

cast Poison (WizSim me b c m es) =
  WizSim me b (c + 173) (m - 173) ((Poison, 6) : es)

cast Recharge (WizSim me b c m es) =
  WizSim me b (c + 229) (m - 229) ((Recharge, 5) : es)

heal (Guy hp dmg arm) n = Guy (hp + n) dmg arm
hurt (Guy hp dmg arm) n = Guy (hp - n) dmg arm
