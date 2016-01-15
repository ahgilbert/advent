module P22 where

import P21

data WizSim = WizSim { wiz :: Guy,
                       boss :: Guy,
                       cost :: Int,
                       mana :: Int,
                       effects :: [((WizSim -> WizSim), Int)] }

boss22 = Guy 51 9 0

{- spells:
     | Name           | Cost  | Effect
-----+----------------+-------+----------------
     | Magic Missile  | 53    | 4 damage to boss
     | Drain          | 73    | 2 damage to boss, heal 2
     | Shield         | 113   | armor +7 for 6 turns
     | Poison         | 173   | 3 damage to boss per turn, 6 turns
     | Recharge       | 229   | +101 mana per turn, 5 turns
-}

heal :: Guy -> Int -> Guy
heal (Guy hp dmg arm) n = Guy (hp + n) dmg arm

hurt :: Guy -> Int -> Guy
hurt (Guy hp dmg arm) n = Guy (hp - n) dmg arm

missile :: WizSim -> WizSim
missile (WizSim me b c m es) =
  WizSim me (hurt b 4) (c + 53) (m - 53) es

drain :: WizSim -> WizSim
drain (WizSim me b c m es) =
  WizSim (heal me 2) (hurt b 2) (c + 73) (m - 73) es

shield :: WizSim -> WizSim
shield (WizSim me b c m es) =
  WizSim me b (c + 113) (m - 113) ((shield', 6) : es)
  where shield' (WizSim (Guy hp dmg arm) b c m es) =
          WizSim (Guy hp dmg (7)) b c m es

poison :: WizSim -> WizSim
poison (WizSim me b c m es) =
  WizSim me b (c + 173) (m - 173) ((poison', 6) : es)
  where poison' (WizSim me (Guy hp dmg arm) c m es) =
          WizSim me (Guy (hp - 3) dmg arm) c m es

recharge :: WizSim -> WizSim
recharge (WizSim me b c m es) =
  WizSim me b (c + 229) (m - 229) ((recharge', 5) : es)
  where recharge' (WizSim me b c m es) = (WizSim me b c (m + 101) es)

applyEffects :: WizSim -> WizSim
applyEffects (WizSim me b c m es) =
  WizSim me b c m $ map (\e -> e) es
