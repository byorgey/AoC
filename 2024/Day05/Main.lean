import AoC2024
import Batteries.Data.HashMap
import Batteries.Data.RBMap

-- When I import this, my program stops printing anything!?
-- import Mathlib.Control.Bifunctor

open Batteries

------------------------------------------------------------
-- Data representation

abbrev Page := Nat
abbrev Rules := HashMap Page (List Page)   -- TODO: try making this a Set?
abbrev Update := List Page

structure Input where
  rules : Rules
  updates : List Update
deriving Inhabited

------------------------------------------------------------
-- Parsing

def parseRules (rs : List String) : Rules :=
  let ps₁ := rs.map (λ r => ((r.splitOn "|").map String.toNat!).toPair)
  let ps₂ := ps₁.map (λ (x,y) => (x,[y]))
  HashMap.ofListWith ps₂ List.append

def parseUpdates (us : List String) : List Update :=
  us.map (λ r => (r.splitOn ",").map String.toNat!)

def parse (s : String) : Input :=
  match s.lines.splitOn "" with
    | [rs, us] => { rules := parseRules rs, updates := parseUpdates us }
    | _ => panic "bad input!"

------------------------------------------------------------
-- Part A

def okUpdate (r : Rules) (u : Update) : Bool :=
  u.pairs.all (λ (p₁, p₂) => (r.find! p₁).elem p₂)

def middle (ps : List Page) : Page := ps[ps.length / 2]!

def solveA (i : Input) : Nat :=
  ((i.updates.filter (okUpdate i.rules)).map middle).sum

------------------------------------------------------------
-- Part B

def topSort (r : Rules) (u : Update) : Update :=
  u.mergeSort (λ x y => (r.find! x).elem y)

def solveB (i : Input) : Nat :=
  ((i.updates.filter (not ∘ okUpdate i.rules)).map (middle ∘ topSort i.rules)).sum

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day05/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
