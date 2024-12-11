import AoC
import Batteries.Data.HashMap
open Batteries

------------------------------------------------------------
-- Data representation + parsing

abbrev Input := List Nat

def parse (s : String) : Input := s.words.map String.toNat!

------------------------------------------------------------
-- Solution

def rule : Nat → List Nat
  | 0 => [1]
  | n =>
      let ns := n.digits
      let m := ns.length
      if m.mod 2 = 0 then
        let (x,y) := ns.splitAt (m / 2)
        [Nat.ofDigits y, Nat.ofDigits x]
      else
        [n*2024]

abbrev Stones := HashMap Nat Nat

def blink (stones : Stones) : Stones :=
  HashMap.ofListWith (stones.toList.flatMap ruleN) (· + ·)
  where
    ruleN : (Nat × Nat) → List (Nat × Nat)
      | (stone, count) => (rule stone).map (λ s' => (s', count))

def solve (n : Nat) (stones : Input) : Nat :=
  let stonesMap := HashMap.ofListWith (stones.map (λ s => (s,1))) (· + ·)
  stonesMap
    |> (blink ^ n)
    |> HashMap.toList
    |> List.map Prod.snd
    |> List.sum

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day11/input"
  IO.println s!"{solve 25 input}"
  IO.println s!"{solve 75 input}"
