import AoC
import Batteries.Data.HashMap
import Std.Data.HashSet

open Batteries
open Std

------------------------------------------------------------
-- Data representation

structure Guard where
  pos : V2 Int
  dir : V2 Int
deriving Repr, BEq, Hashable

abbrev Input := Grid Bool × Guard

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input :=
  let charGrid := s.lines.map String.toList
  let grid : Grid Bool := Grid.ofList $ charGrid.map (List.map (· ≠ '#'))
  let guardCols := charGrid.map (λ row => row.findIdx (· = '^'))
  let guardRow := guardCols.findIdx (· < grid.rows)
  let guard := Guard.mk (V2.mk guardRow guardCols[guardRow]!) V2.N
  (grid, guard)

------------------------------------------------------------
-- Part A

def step (grid : Grid Bool) (guard : Guard) : Option Guard :=
  let next : Guard := Guard.mk (guard.pos + guard.dir) guard.dir
  match grid.contains next.pos, grid.get! (next.pos) with
    | true, true => next
    | true, false => Guard.mk guard.pos guard.dir.rt
    | _, _ => none

def solveA : Input → Nat
  | (grid, guard) =>
    let guardPath : List Guard := iter (2 * grid.rows * grid.cols) (step grid) guard
    (HashSet.ofList (List.map Guard.pos guardPath)).size

------------------------------------------------------------
-- Part B

def hasRepeat [BEq α] [Hashable α] : List α → Bool := go HashSet.empty
  where
    go [BEq α] [Hashable α] : HashSet α → List α → Bool
      | _, [] => false
      | seen, x :: xs => seen.contains x ∨ go (seen.insert x) xs

def guardLoops (grid : Grid Bool) (guard : Guard) (obstacle : V2 Int) : Bool :=
  hasRepeat (iter (2 * grid.rows * grid.cols) (step (grid.insert obstacle false)) guard)

def solveB : Input → Nat
  | (grid, guard) =>
      let candidates : List (V2 Int) := (grid.toList.filter Prod.snd).map Prod.fst
      (candidates.filter (guardLoops grid guard)).length

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day06/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
