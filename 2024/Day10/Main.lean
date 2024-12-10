import AoC
import Batteries.Data.HashMap

open Batteries

------------------------------------------------------------
-- Data representation

abbrev Input := Grid Nat

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input :=
  Grid.ofList $ s.lines.map (String.toList ▸ List.map Char.toDigit)

------------------------------------------------------------
-- DFS

structure Trail where
  score : Nat
  rating : Nat
deriving Repr, Inhabited

instance : Zero Trail where
  zero := { score := 0, rating := 0 }

instance : Add Trail where
  add t1 t2 := { score := t1.score + t2.score, rating := t1.rating + t2.rating }

partial def DFS (g : Input) (start : V2 Int) : Trail :=
  ((go start).run HashMap.empty).fst
  where
    go (u : V2 Int) : StateM (HashMap (V2 Int) Nat) Trail := do
      let dp ← get
      if dp.contains u then pure (Trail.mk 0 (dp.find! u))
      else
        let t ← do
          if g.get! u = 9 then pure (Trail.mk 1 1)
          else
            let next := g.neighbors4 u
              |> List.filter (λ v => g.get! v = g.get! u + 1)
            let ps ← next.mapM go
            pure ps.sum
        modify (λ dp => dp.insert u t.rating)
        pure t

------------------------------------------------------------
-- Solver

def solve (g : Input) : Trail :=
  g.toList
    |> List.filter (Prod.snd ▸ (· = 0))
    |> List.map (Prod.fst ▸ DFS g)
    |> List.sum

def solveA (g : Input) : Nat := (solve g).score
def solveB (g : Input) : Nat := (solve g).rating

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day10/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
