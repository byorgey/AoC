import AoC
import Batteries.Data.HashMap
open Batteries

------------------------------------------------------------
-- Data representation

abbrev Input := List Nat

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input := s.lines.map String.toNat!

------------------------------------------------------------
-- Part A

def Nat.shift (n : Nat) (k : Int) : Nat :=
  if (k < 0) then n.shiftRight k.natAbs else n.shiftLeft k.natAbs

def Nat.step (k : Int) (n : Nat) : Nat := (n.xor (n.shift k)) &&& 0xffffff

def Nat.secret (n : Nat) : Nat :=
  n.step 6 |> .step (-5) |> .step 11

-- 6 0
-- 6 1 0 -5
-- 17 12 11 1 0 -5

def solveA (i : Input) : Nat :=
  i.map (Nat.secret ^ 2000)
  |> List.sum

------------------------------------------------------------
-- Part B

def changePrices (init : Nat) : HashMap (List Int) Nat :=
  let prices := (iter 2000 (some ∘ Nat.secret) init).map (·.mod 10)
  let changes := prices.tail!.zipWith (λ (a b : Nat) => (↑a : Int) - (↑b : Int)) prices
  let changes4 := (changes.tails.map (List.take 4)).takeWhile (·.length ≥ 4)
  let m := HashMap.ofList $ (changes4.zip (List.drop 4 prices)).reverse
  m

def solveB (i : Input) : Nat :=
  i.map changePrices
  |> List.foldl (HashMap.mergeWith (λ _ => (· + ·))) HashMap.empty
  |> HashMap.toList
  |> List.map Prod.snd
  |> List.max?
  |> Option.get!

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day22/input"
  IO.println s!"{solveA input}"
  -- IO.println s!"{solveB input}"
