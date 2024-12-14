import AoC
import Batteries.Data.HashMap
import Std.Data.HashSet
open Batteries

------------------------------------------------------------
-- Data representation

structure Robot where
  p : V2 Int
  v : V2 Int
deriving Repr, Inhabited

abbrev Input := List Robot

------------------------------------------------------------
-- Parsing

def readRobot : List Int → Robot
  | [px,py,vx,vy] => Robot.mk (V2.mk py px) (V2.mk vy vx)
  | _ => panic "No parse"

def parseRobot (s : String) : Robot :=
  s.map (λ c => if c.isDigit ∨ c == '-' then c else ' ')
  |> String.words
  |> List.map String.toInt!
  |> readRobot

def parse (s : String) : Input :=
  s.lines |> List.map parseRobot

------------------------------------------------------------
-- Part A

def Robot.step (n : Nat) (rob : Robot) (rect : V2 Int) : Robot :=
  { rob with p := (rob.p + (n : Int) * rob.v) % rect }

def Robot.quadrant (rob : Robot) (rect : V2 Int) : V2 Int :=
  (rob.p - rect / (2:Int)).map Int.sign

def safetyFactor (rect : V2 Int) (rs : List Robot) : Nat :=
  rs.map (λ r : Robot => r.quadrant rect)
  |> List.filter (λ v => v.all (· != 0))
  |> List.cardinality
  |> HashMap.toList
  |> List.map Prod.snd
  |> List.prod

def stepRobots (n : Nat) (rect : V2 Int) (rs : List Robot) : List Robot :=
  rs.map (λ r => r.step n rect)

def bigRect : V2 Int := V2.mk 103 101

def solveA (rs : Input) (rect : V2 Int) : Nat :=
  rs
  |> stepRobots 100 rect
  |> safetyFactor rect

------------------------------------------------------------
-- Part B

def graphRobots (rs : List Robot) : String :=
  let rSet : Std.HashSet (V2 Int) := Std.HashSet.ofList (rs.map (λ r => r.p))
  let cs :=
    (List.range 51).map $ λ (r : Nat) =>
      (List.range 101).map $ λ (c : Nat) =>
        let top := rSet.contains (V2.mk (2*r) c)
        let bot := rSet.contains (V2.mk (2*r + 1) c)
        if bot ∧ top then '█'
        else if bot then '▄'
        else if top then '▀'
        else ' '
  (cs.map String.mk).unlines

def solveB (rs : Input) : IO Unit := do
  let mut cur := (stepRobots 79 bigRect) rs
  for i in [0:500] do
    IO.println (graphRobots cur)
    cur := (stepRobots 101 bigRect) cur
    IO.println s!"{i}"
    IO.sleep 500

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day14/input"
  IO.println s!"{solveA input bigRect}"
  solveB input
