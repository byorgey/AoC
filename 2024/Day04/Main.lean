import AoC2024
import Mathlib.Data.List.Monad

------------------------------------------------------------
-- Data representation

abbrev Input := Array (Array Char)

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input := (s.lines.map (List.toArray ∘ String.toList)).toArray

------------------------------------------------------------
-- Part A

inductive DirType where
  | all : DirType
  | diag : DirType

def dirs (dt : DirType) : List (Int × Int) := do
  let dr ← [-1, 0, 1]
  let dc ← [-1, 0, 1]
  match dt with
    | .all => guard (dr ≠ 0 ∨ dc ≠ 0)
    | .diag => guard (dr ≠ 0 ∧ dc ≠ 0)
  pure (dr, dc)

instance : HAdd (Nat × Nat) (Int × Int) (Int × Int) where
  hAdd | (a, b), (c, d) => (a+c,b+d)

instance : HMul Nat (Int × Int) (Int × Int) where
  hMul | k, (a,b) => (k * a, k * b)

def inBounds : Nat → Int × Int → Bool
  | n, (a,b) => 0 ≤ a ∧ a < n ∧ 0 ≤ b ∧ b < n

def wordLocs (dt : DirType) (n : Nat) (m : Nat) : List (List (Int × Int)) := do
  let d ← dirs dt
  let r ← List.range n
  let c ← List.range n
  let e := (r,c) + (m-1) * d
  guard (inBounds n e)
  pure $ (List.range m).map (λ k => (r,c) + k * d)

example : (wordLocs .all 3 4).length = 0 := by rfl
example : (wordLocs .all 4 4).length = 20 := by rfl
example : (wordLocs .all 3 3).length = 16 := by rfl

def index : Input → Int × Int → Char
  | grid, (r,c) => (grid[r.toNat]!)[c.toNat]!

def extract (input : Input) (is : List (Int × Int)) : String :=
  String.mk $ is.map (index input)

def solveA (input : Input) : Nat :=
  ((wordLocs .all (input.size) 4).map (extract input)).count "XMAS"

------------------------------------------------------------
-- Part B

def crossing : List (Int × Int) → List (Int × Int) → Bool
  | [_,b,_], [_,e,_] => b = e
  | _, _ => false

def solveB (input : Input) : Nat :=
  let locs := wordLocs .diag (input.size) 3
  let masLocs := List.filter (λ wl => extract input wl = "MAS") locs
  masLocs.pairs.countP (Function.uncurry crossing)

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day04/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
