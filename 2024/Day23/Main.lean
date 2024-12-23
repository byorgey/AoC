import AoC
import Batteries.Data.HashMap
open Batteries

------------------------------------------------------------
-- Data representation

abbrev Node := String
abbrev Input := List (Node × Node)

------------------------------------------------------------
-- Parsing

def readEdge (s : String) : Node × Node :=
  match s.splitOn "-" with
  | [a,b] => (a,b)
  | _ => panic "no parse"

def parse (s : String) : Input := s.lines.map readEdge

------------------------------------------------------------
-- Part A

abbrev Graph := HashMap Node (Std.HashSet Node)

def Graph.ofList (edges : List (Node × Node)) : Graph :=
  HashMap.ofListWith (edges.flatMap (λ (a,b) => [(a, {b}), (b, {a})])) Std.HashSet.union

def solveA (edges : Input) : Nat :=
  let g := Graph.ofList edges
  let nodes := g.toList.map Prod.fst
  let triples :=
    [ (a,b,c)
    | for a in nodes
    , for b in (g.find! a).toList
    , for c in (g.find! b).toList
    , if c ∈ g.find! a
    , if [a,b,c].any (·.startsWith "t")
    ]
  triples.length / 6

------------------------------------------------------------
-- Part B

-- Find largest clique!
-- There is probably a faster way but this only takes 8s.

abbrev Clique := Std.HashSet Node

def extendClique (g : Graph) (nodes : List Node) (c : Clique) : List Clique :=
  let biggest : Node := c.toList.mergeSort.reverse.head!
  let toAdd : List Node := nodes.dropWhile (· ≤ biggest)
  toAdd.filterMap (λ y => if c.all (λ x => y ∈ g.find! x) then some (c.insert y) else none)

def solveB (edges : Input) : String :=
  let g := Graph.ofList edges
  let s2 : List (Std.HashSet Node) := edges.map (λ (a,b) => {a,b})
  let nodes := (g.toList.map Prod.fst).mergeSort
  let cliques := iter 20 (·.flatMap (extendClique g nodes)) s2
  let maxClique := (cliques.takeWhile (not ∘ List.isEmpty)).reverse.head!
  ",".intercalate (maxClique.head!.toList.mergeSort)

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day23/input"
  IO.println s!"{solveA input}"
  IO.println (solveB input)
