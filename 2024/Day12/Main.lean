import AoC
import Batteries.Data.HashMap
open Batteries

------------------------------------------------------------
-- Data representation

abbrev Input := Grid Char

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input := Grid.ofList $ s.lines.map String.toList

------------------------------------------------------------
-- Part A

structure Measurements where
  area : Nat
  perim : Nat
  corners : Nat
deriving Repr, Inhabited

instance : ToString Measurements where
  toString m := toString (m.area, m.perim, m.corners)

def Measurements.priceA (m : Measurements) : Nat := m.area * m.perim

-- Corners
--
-- ..     #.    .#       ab
-- #.     ##    #.       #c

def isCorner (g : Grid Char) (i : V2 Int) (d : V2 Int) : Bool :=
  let sameRegion dir := g.contains (i + dir) && g.get! (i + dir) == g.get! i
  let a := sameRegion d
  let b := sameRegion (d + d.rt)
  let c := sameRegion d.rt
  (¬a ∧ ¬b ∧ ¬c) ∨ (a ∧ ¬b ∧ c) ∨ (¬a ∧ b ∧ ¬c)

def BFS (g : Grid Char) : List Measurements := Id.run do
  let mut visited : Std.HashSet (V2 Int) := Std.HashSet.empty
  let mut measures : List Measurements := []
  let mut q : Std.Queue (V2 Int) := Std.Queue.empty

  for v in g.keys do
    if not (visited.contains v) then
      visited := visited.insert v

      q := q.enqueue v
      let mut m : Measurements := Measurements.mk 0 0 0

      while not q.isEmpty do
        match q.dequeue? with
        | none => pure ()
        | some (u, q') =>
            q := q'
            m := { m with area := m.area + 1 }

            -- Count corners
            for d in V2.cardinals do
              if isCorner g u d then
                m := { m with corners := m.corners + 1 }

            -- Count perimeter and area
            for v in u.neighbors4 do
              if not (g.contains v) then
                m := { m with perim := m.perim + 1 }
              else if g.get! v != g.get! u then
                m := { m with perim := m.perim + 1 }
              else if not (visited.contains v) then
                visited := visited.insert v
                q := q.enqueue v
      measures := m :: measures

  pure measures


def solveA (grid : Input) : Nat :=
  let ms := BFS grid
  (ms.map Measurements.priceA).sum

------------------------------------------------------------
-- Part B

def Measurements.priceB (m : Measurements) : Nat := m.area * m.corners

def solveB (grid : Input) : Nat :=
  BFS grid
    |> List.map Measurements.priceB
    |> List.sum

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day12/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
