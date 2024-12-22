import AoC
import Batteries.Data.HashMap
open Batteries

------------------------------------------------------------
-- Data representation

inductive Object where
  | wall : Object
  | robot : Object
  | boxL : Object
  | boxR : Object
deriving Repr, Inhabited, BEq

def drawObject : Option Object → Char
  | some .wall => '#'
  | some .robot => '@'
  | some .boxL => '['
  | some .boxR => ']'
  | _ => '.'

abbrev Floor := Grid (Option Object)
abbrev Move := V2 Int

structure Warehouse where
  floor : Floor
  robot : V2 Int
deriving Inhabited

instance : ToString Warehouse where
  toString w :=
    w.floor
    |> (λ g => g.insert w.robot (some .robot))
    |> Grid.map drawObject
    |> toString

structure Input where
  warehouse : Warehouse
  moves : List Move
deriving Inhabited

------------------------------------------------------------
-- Parsing

def parseObject : Char → Option Object
  | '#' => some .wall
  | '@' => some .robot
  | 'O' => some .boxL
  | _ => none

def parseMove : Char → Move
  | '^' => V2.N
  | '>' => V2.E
  | 'v' => V2.S
  | '<' => V2.W
  | _ => 0

def removeRobot : Option Object → Option Object
  | some .robot => none
  | o => o

def parse (s : String) : Input :=
  match s.lines.splitOn "" with
  | [fs, is] =>
      let g := Grid.ofList (fs.map (String.toList ▸ .map parseObject))
      let moves := (String.join is).toList.map parseMove
      let warehouse := Warehouse.mk (g.map removeRobot) (g.findIdx? (· == some .robot)).get!
      Input.mk warehouse moves
  | _ => panic "no parse"

------------------------------------------------------------
-- Part A

def move (w : Warehouse) (move : Move) : Warehouse := Id.run do
  let next := w.robot + move
  let mut cur := next
  while w.floor.get! cur == some .boxL do
    cur := cur + move

  if w.floor.get! cur == none then
    let mut w' := w.floor.insert next none
    if cur != next then
      w' := w'.insert cur (some .boxL)
    pure $ Warehouse.mk w' next
  else
    pure w

def GPS (v : V2 Int) : Nat := Int.natAbs (100 * v.r + v.c)

def solveA (i : Input) : Nat :=
  i.moves.foldl move i.warehouse
  |> Warehouse.floor
  |> Grid.toList
  |> List.filterMap (λ | (v, some .boxL) => some v | _ => none)
  |> List.map GPS
  |> List.sum

------------------------------------------------------------
-- Part B

def V2.widen (v : V2 Int) : V2 Int :=
  { v with c := v.c * 2 }

def Object.widen (o : Object) : Object :=
  match o with
  | .boxL => .boxR
  | o => o

def Warehouse.widen (w : Warehouse) : Warehouse :=
  let objs' := w.floor.toList.flatMap $
    λ (v, o) => [(v.widen, o), (v.widen + V2.E, o.map Object.widen)]
  let m' :=
    Grid.mk (w.floor.rows) (2 * w.floor.cols)
      (HashMap.ofList objs')
      w.floor.background
  let robot' := w.robot.widen
  Warehouse.mk m' robot'

def moveB (w : Warehouse) (move : Move) : Warehouse := Id.run do
  let pushes (v : V2 Int) : List (V2 Int) :=
    match w.floor.get! v with
    | some .boxL => [V2.mk v.r (v.c + 1), v + move]
    | some .boxR => [V2.mk v.r (v.c - 1), v + move]
    | _ => []

  let bfs := BFS [w.robot + move] pushes

  let reachable := bfs.dist.toList.map Prod.fst
  let moveSucceeds := reachable.all (λ v => w.floor.get! v != some Object.wall)
  let movingBoxes := reachable.filter (λ v => [some Object.boxL, some Object.boxR].elem (w.floor.get! v))

  if !moveSucceeds then w
  else
    -- move robot + all pushed boxes
    let erased : Floor := List.foldl (λ f v => f.insert v none) w.floor movingBoxes
    let moved : Floor := List.foldl (λ f v => f.insert (v + move) (w.floor.get! v)) erased movingBoxes
    { w with robot := w.robot + move, floor := moved }

def solveB (i : Input) : Nat :=
  i.moves.foldl moveB i.warehouse.widen
  |> Warehouse.floor
  |> Grid.toList
  |> List.filterMap (λ | (v, some .boxL) => some v | _ => none)
  |> List.map GPS
  |> List.sum

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day15/input"
  IO.println s!"{solveA input}"
  -- IO.println s!"{ww}"
  -- IO.println s!"{moveB ww V2.W}"
  -- IO.println s!"{moveB (moveB (moveB ww V2.W) V2.S) V2.W}"
  -- IO.println s!"{moveB (moveB (moveB (moveB ww V2.W) V2.S) V2.W) V2.N}"
  -- IO.println s!"{moveB (moveB (moveB (moveB (moveB ww V2.W) V2.S) V2.W) V2.N) V2.N}"
  -- IO.println s!"{move (move (move input.warehouse V2.N) V2.E) V2.E}"
  IO.println s!"{solveB input}"
