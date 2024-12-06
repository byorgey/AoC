import AoC
import Mathlib.Data.List.Monad

------------------------------------------------------------
-- Data representation

abbrev Input := Grid Char

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input := Grid.ofList $ s.lines.map String.toList

------------------------------------------------------------
-- Part A

inductive DirType where
  | all : DirType
  | diag : DirType

def dirs (dt : DirType) : List (V2 Int) := do
  let dr ← [-1, 0, 1]
  let dc ← [-1, 0, 1]
  match dt with
    | .all => guard (dr ≠ 0 ∨ dc ≠ 0)
    | .diag => guard (dr ≠ 0 ∧ dc ≠ 0)
  pure $ V2.mk dr dc

def inBounds : Nat → V2 Int → Bool
  | n, v => 0 ≤ v.r ∧ v.r < n ∧ 0 ≤ v.c ∧ v.c < n

def wordLocs (dt : DirType) (n : Nat) (m : Nat) : List (List (V2 Int)) := do
  let d ← dirs dt
  let r ← List.range n
  let c ← List.range n
  let l : V2 Int := ⟨r, c⟩
  let e := l + (m-1 : Int) * d
  guard (inBounds n e)
  pure $ (List.range m).map (λ k => l + (k : Int) * d)

example : (wordLocs .all 3 4).length = 0 := by rfl
example : (wordLocs .all 4 4).length = 20 := by rfl
example : (wordLocs .all 3 3).length = 16 := by rfl

def extract (input : Input) (is : List (V2 Int)) : String :=
  String.mk $ is.map input.get!

def solveA (input : Input) : Nat :=
  ((wordLocs .all input.rows 4).map (extract input)).count "XMAS"

------------------------------------------------------------
-- Part B

def crossing [DecidableEq α] : List α → List α → Bool
  | [_,b,_], [_,e,_] => b = e
  | _, _ => false

def solveB (input : Input) : Nat :=
  let locs := wordLocs .diag input.rows 3
  let masLocs := List.filter (λ wl => extract input wl = "MAS") locs
  masLocs.pairs.countP (Function.uncurry crossing)

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day04/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
