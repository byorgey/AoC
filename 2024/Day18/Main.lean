import AoC
import Batteries.Data.HashMap
import Batteries.Data.Nat.Bisect
open Batteries

------------------------------------------------------------
-- Data representation

abbrev Input := List (V2 Int)

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input :=
  s.lines.map (λ l =>
    l.splitOn ","
    |> List.map String.toInt!
    |> List.toPair
    |> (λ (x,y) => V2.mk y x)
  )

------------------------------------------------------------
-- Part A

def reachable (n : Nat) (vs : List (V2 Int)) : Option Nat :=
  let g : Grid Bool := Grid.mk n n (HashMap.ofList (vs.map (·,false))) (some true)
  let neighbors (v : V2 Int) : List (V2 Int) := (g.neighbors4 v).filter g.get!
  let sssp := BFS [V2.mk 0 0] neighbors
  sssp.dist.find? (V2.mk (n-1) (n-1))

def solveA (n : Nat) (i : Input) : Nat :=
  (reachable n i).get!

------------------------------------------------------------
-- Part B

def solveB (n : Nat) (is : Input) : String :=
  let stillOpen (k : Nat) : Bool :=
    if k = 0 then true
    else if k > is.length then false
    else (reachable n (is.take k)).isSome

  let k := Nat.bisect (start := 0) (stop := is.length + 1) (p := stillOpen)
    (by simp) (by simp [stillOpen]) (by simp [stillOpen])
  let v := is[k]!
  s!"{v.c},{v.r}"

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day18/input"
  IO.println s!"{solveA 71 (input.take 1024)}"
  IO.println s!"{solveB 71 input}"
