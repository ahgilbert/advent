module P22 where

import Data.List
import P21

data WizSim = WizSim { wiz :: Guy,
                       boss :: Guy,
                       mana :: Int,
                       effects :: [(Spell, Int)] }
            | BossDead
            | WizardDead
            | OutOfMana
            | CircuitBreak
            deriving (Eq, Show)

data Spell = Missile | Drain | Shield | Poison | Recharge
           deriving (Eq, Show)

p22_1 = print $ manaSpent . snd $ head $ faith world22

me22 = Guy 50 0 0
boss22 = Guy 51 9 0
allSpells = [Missile, Drain, Shield, Poison, Recharge]

world22 = WizSim me22 boss22 500 []

test221 = WizSim (Guy 10 0 0) (Guy 13 8 0) 250 []
test222 = WizSim (Guy 10 0 0) (Guy 14 8 0) 250 []

isDead (Guy hp _ _) = hp <= 0

faith :: WizSim -> [(WizSim, [Spell])]
faith w = sortOn (\(_,ss) -> manaSpent ss) $
          filter (\(outcome,_) -> outcome == BossDead) $ wizardFight (w, [])

wizardFight :: (WizSim, [Spell]) -> [(WizSim, [Spell])]
wizardFight (w@(WizSim me b m _), spellsSoFar)
  | isDead b = [(BossDead, spellsSoFar)]
  | isDead me = [(WizardDead, [])]
  | m <= 0 = [(OutOfMana, [])]
  | length spellsSoFar > 10 = [(CircuitBreak, [])]
  | True = let
    outcomes = wizardTurn (w, spellsSoFar)
    outcomes' = map bossTurn outcomes
    in concatMap wizardFight outcomes'

wizardTurn (w, spellsSoFar) = let
  w' = applyEffects w
  castableSpells = filter (\s -> not $ elem s (map fst (effects w'))) allSpells
  outcomes = map (\s -> (cast s w', (s : spellsSoFar))) castableSpells
  in filter fightContinues outcomes
  where fightContinues ((WizSim _ _ _ _), _) = True
        fightContinues _ = False

bossTurn (w,ss) =
  let w' = applyEffects w
  in (bossAttacks w', ss)

manaSpent ss = sum $ map spellCost ss

spellCost Missile  = 53
spellCost Drain    = 73
spellCost Shield   = 113
spellCost Poison   = 173
spellCost Recharge = 229

applyEffects w@(WizSim me b m es) =
  clearEffects $ decrementEffects $ foldl apply w $ map fst es
  where decrementEffects (WizSim me b m es) =
          WizSim me b m $ map (\(a, n) -> (a, n - 1)) es
        clearEffects (WizSim me b m es) = let
          (fading, ongoing) = partition (\(_,n) -> n <= 0) es
          me' = if elem Shield (map fst fading)
                then me { def = 0 }
                else me
          in WizSim me' b m ongoing

{- spells:
     | Name           | cost  | Effect
-----+----------------+-------+----------------
     | Magic Missile  | 53    | 4 damage to boss
     | Drain          | 73    | 2 damage to boss, heal 2
     | Shield         | 113   | armor +7 for 6 turns
     | Poison         | 173   | 3 damage to boss per turn, 6 turns
     | Recharge       | 229   | +101 mana per turn, 5 turns
-}

apply (WizSim (Guy hp dmg _) b m es) Shield =
  WizSim (Guy hp dmg 7) b m es
apply (WizSim me b m es) Poison =
  WizSim me (hurt b 3) m es
apply (WizSim me b m es) Recharge =
  WizSim me b (m + 101) es
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
hurt (Guy hp dmg arm) n = Guy (hp - n') dmg arm
  where n' = max 1 (n - arm)

bossAttacks (WizSim me b m es) = WizSim (hurt me (str b)) b m es
