import AoC
import Std.Data.HashSet

------------------------------------------------------------
-- Data representation

structure Maze where
  maze : Grid Bool
  start : V2 Int
  «end» : V2 Int
deriving Inhabited

abbrev Input := Maze

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input :=
  let gc : Grid Char := Grid.ofList $ s.lines.map String.toList
  Maze.mk (gc.map (· != '#')) (gc.findIdx? (· == 'S')).get! (gc.findIdx? (· == 'E')).get!

------------------------------------------------------------
-- Part A

structure Reindeer where
  pos : V2 Int
  heading : V2 Int
deriving Inhabited, Repr, BEq, Hashable

def Reindeer.lt (r : Reindeer) : Reindeer := { r with heading := r.heading.lt }
def Reindeer.rt (r : Reindeer) : Reindeer := { r with heading := r.heading.rt }
def Reindeer.fwd (r : Reindeer) : Reindeer := { r with pos := r.pos + r.heading }

def solveA (i : Input) : Nat :=
  let start : Reindeer := Reindeer.mk i.start V2.E
  let neighbors (r : Reindeer) : List (Reindeer × Nat) :=
    [(r.lt, 1000), (r.rt, 1000)] ++ if (i.maze.get! r.fwd.pos) then [(r.fwd, 1)] else []
  let res := Dijkstra start neighbors
  let e1 := res.dist.find? (Reindeer.mk i.end V2.N)
  let e2 := res.dist.find? (Reindeer.mk i.end V2.E)
  (min <$> e1 <*> e2).get!

------------------------------------------------------------
-- Part B

def solveB (i : Input) : Nat :=
  let start : Reindeer := Reindeer.mk i.start V2.E
  let neighbors (r : Reindeer) : List (Reindeer × Nat) :=
    [(r.lt, 1000), (r.rt, 1000)] ++ if (i.maze.get! r.fwd.pos) then [(r.fwd, 1)] else []
  let res := Dijkstra start neighbors
  let end1 := Reindeer.mk i.end V2.N
  let end2 := Reindeer.mk i.end V2.E
  let e1 := res.dist.find? end1
  let e2 := res.dist.find? end2
  let bestEnd := match (e1, e2) with
    | (some d1, some d2) => if d1 < d2 then end1 else end2
    | (some _, _) => end1
    | _ => end2
  let dfs := DFS bestEnd (λ u => res.parents.findD u [])
  1 + (Std.HashSet.ofList (dfs.toList.map (Prod.fst ▸ Reindeer.pos))).size

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day16/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
