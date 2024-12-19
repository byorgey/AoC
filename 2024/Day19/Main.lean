import AoC
import Batteries.Data.HashMap
open Batteries

------------------------------------------------------------
-- Data representation

inductive Color where
  | white : Color
  | blue : Color
  | black : Color
  | red : Color
  | green : Color
deriving Repr, Inhabited, BEq, Hashable

abbrev Pattern := List Color

structure Input where
  towels : List Pattern
  designs : List Pattern
deriving Repr, Inhabited

------------------------------------------------------------
-- Parsing

def readColor : Char → Color
  | 'w' => .white
  | 'u' => .blue
  | 'b' => .black
  | 'r' => .red
  | 'g' => .green
  | _ => panic "unknown color"

def readPattern (s : String) : Pattern :=
  s.toList.map readColor

def readTowels (s : String) : List Pattern :=
  (s.map (λ c => if c == ',' then ' ' else c)).words.map readPattern

def parse (s : String) : Input :=
  match s.lines.splitOn "" with
  | [[ts], ds] => { towels := readTowels ts, designs := ds.map readPattern }
  | _ => panic "no parse"

------------------------------------------------------------
-- Trie

structure Trie (κ α : Type) where
  value : Option α
  children : κ → Option (Trie κ α)
deriving Inhabited

def Trie.empty : Trie κ α := Trie.mk none (λ _ => none)

def ins [BEq α] : α → β → (α → β) → (α → β)
  | a, b, f => λ x => if x == a then b else f x

def Trie.insert [BEq κ] : Trie κ α → List κ → α → Trie κ α
  | t, [], a => { t with value := a }
  | t, k :: ks, a =>
      let c := (t.children k).getD Trie.empty
      { t with children := ins k (some (c.insert ks a)) t.children }

def Trie.accept (t : Trie κ Unit) (s : List κ) : Nat := go s [(t,1)]
  where
    go (key : List κ) (ts : List (Trie κ Unit × Nat)) : Nat :=
      let numFinished :=
        ts.filter (λ t => t.fst.value.isSome)
          |> List.map Prod.snd
          |> List.sum
      match key with
        | [] => numFinished
        | k :: ks =>
            let ts' := if numFinished > 0 then (t, numFinished) :: ts else ts
            go ks (ts'.filterMap (λ (t,n) => (·,n) <$> t.children k))

def mkTrie (i : Input) : Trie Color Unit :=
  i.towels.foldl (λ t p => t.insert p ()) Trie.empty

------------------------------------------------------------
-- Part A

def solveA (i : Input) : Nat :=
  let t := mkTrie i
  i.designs.countP (λ p => t.accept p > 0)

------------------------------------------------------------
-- Part B

def solveB (i : Input) : Nat :=
  let t := mkTrie i
  (i.designs.map t.accept).sum

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day19/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
