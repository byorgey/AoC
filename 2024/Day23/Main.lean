import AoC
import Batteries.Data.HashMap
import Batteries.Data.RBMap
open Batteries

------------------------------------------------------------
-- Data representation

abbrev Node := String
abbrev Input := List (Node × Node)

abbrev NodeSet := RBSet Node compare

------------------------------------------------------------
-- Parsing

def readEdge (s : String) : Node × Node :=
  match s.splitOn "-" with
  | [a,b] => (a,b)
  | _ => panic "no parse"

def parse (s : String) : Input := s.lines.map readEdge

------------------------------------------------------------
-- Part A

abbrev Graph := HashMap Node NodeSet

def Graph.ofList (edges : List (Node × Node)) : Graph :=
  HashMap.ofListWith (edges.flatMap (λ (a,b) => [(a, RBSet.single b), (b, RBSet.single a)])) RBSet.union

def solveA (edges : Input) : Nat :=
  let g := Graph.ofList edges
  let nodes := g.toList.map Prod.fst
  let triples :=
    [ (a,b,c)
    | for a in nodes
    , for b in (g.find! a).toList
    , for c in (g.find! b).toList
    , if (g.find! a).contains c
    , if [a,b,c].any (·.startsWith "t")
    ]
  triples.length / 6

------------------------------------------------------------
-- Part B

abbrev Clique := NodeSet

-- Bron-Kerbosch algorithm for enumerating all maximal cliques
partial def bronKerbosch (g : Graph) : List Clique :=
  let nodes := RBSet.ofList (g.toList.map Prod.fst) compare
  ((go RBSet.empty nodes RBSet.empty).run #[]).snd.toList
 where
  -- List all maximal cliques that include all nodes in R, some in P, and none in X
  go (R P X : NodeSet) : StateM (Array Clique) Unit := do
    if P.isEmpty && X.isEmpty then
      modify (·.push R)
    else
      let pivot := (P.union X).toList.head!
      let mut P' := P
      let mut X' := X
      for v in P'.sdiff (g.find! pivot) do
        let N := g.find! v
        go (R.insert v) (P'.intersectWith compare (λ a _ => a) N) (X'.intersectWith compare (λ a _ => a) N)
        P' := P'.erase (compare v)
        X' := X'.insert v

instance : Max Clique where
  max a b := if a.size > b.size then a else b

def solveB (edges : Input) : String :=
  let g := Graph.ofList edges
  ",".intercalate ((bronKerbosch g).max?.get!.toList.mergeSort)

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day23/input"
  IO.println s!"{solveA input}"
  IO.println (solveB input)

------------------------------------------------------------
-- Old Part B solution

-- Old solution

-- def extendClique (g : Graph) (nodes : List Node) (c : Clique) : List Clique :=
--   let biggest : Node := c.toList.mergeSort.reverse.head!
--   let toAdd : List Node := nodes.dropWhile (· ≤ biggest)
--   toAdd.filterMap (λ y => if c.all (λ x => (g.find! x).contains y) then some (c.insert y) else none)

-- def solveB (edges : Input) : String :=
--   let g := Graph.ofList edges
--   let s2 : List NodeSet := edges.map (λ (a,b) => RBSet.ofList [a,b] compare)
--   let nodes := (g.toList.map Prod.fst).mergeSort
--   let cliques := iter 20 (·.flatMap (extendClique g nodes)) s2
--   let maxClique := (cliques.takeWhile (not ∘ List.isEmpty)).reverse.head!
--   ",".intercalate (maxClique.head!.toList.mergeSort)
