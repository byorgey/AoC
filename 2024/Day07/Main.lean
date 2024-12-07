import AoC
import Mathlib.Data.List.Monad

------------------------------------------------------------
-- Data representation

abbrev Equation := Nat × List Nat
abbrev Input := List Equation

------------------------------------------------------------
-- Parsing

def parseEquation (s : String) : Equation := match s.words with
  | [] => panic "no input!"
  | x :: xs => ((x.dropRight 1).toNat!, xs.map String.toNat!)

def parse (s : String) : Input := s.lines.map parseEquation

------------------------------------------------------------
-- Part A

inductive Operator where
  | add : Operator
  | mul : Operator
  | cat : Operator

def interpOp : Operator → (Nat → Nat → Nat)
  | .add => (· + ·)
  | .mul => (· * ·)
  | .cat => (λ x y => (x.repr.append y.repr).toNat!)

-- TODO: convince Lean this is terminating?
partial def outcomes : List Operator → List Nat → List Nat
  | _, [] => []
  | _, [a] => [a]
  | ops, a :: b :: cs => do
      let op ← ops
      outcomes ops (interpOp op a b :: cs)

partial def satisfiable : List Operator → Equation → Bool
  | ops, (lhs, xs) => (outcomes ops xs).elem lhs

def solve (ops : List Operator) (i : Input) : Nat :=
  i.filter (satisfiable ops)
    |> List.map Prod.fst
    |> List.sum

------------------------------------------------------------
-- Part A

def solveA : Input → Nat := solve [.add, .mul]

------------------------------------------------------------
-- Part B

def solveB : Input → Nat := solve [.add, .mul, .cat]

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day07/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
