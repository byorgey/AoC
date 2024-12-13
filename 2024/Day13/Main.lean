import AoC

------------------------------------------------------------
-- Data representation

structure Machine where
  buttonA : V2 Int
  buttonB : V2 Int
  prize : V2 Int
deriving Repr, Inhabited

abbrev Input := List Machine

------------------------------------------------------------
-- Parsing

def readV2 (s : String) : V2 Int :=
  s.toList
  |> List.filter (λ x => x.isDigit ∨ (x == ' '))
  |> String.mk
  |> String.words
  |> List.map String.toInt!
  |> (λ xs => V2.mk (xs[0]!) (xs[1]!))

#eval readV2 "Button A: X+94, Y+34"
#eval readV2 "Prize: X=8400, Y=5400"

def parseMachine (ls : List String) : Machine :=
  match ls with
  | [a,b,c] => Machine.mk (readV2 a) (readV2 b) (readV2 c)
  | _ => panic "No parse"

def parse (s : String) : Input :=
  s.lines.splitOn ""
  |> List.map parseMachine

------------------------------------------------------------
-- Part A

/-
   a(rx + sy) + b(tx + uy) = (vx + wy)
   [r  s] [a] = [v]
   [t  u] [b]   [w]

-/

def solveMachine (m : Machine) : Option Int := match m with
  | { buttonA := V2.mk r t, buttonB := V2.mk s u, prize := V2.mk v w } =>
    let det := r*u - s*t
    if det == 0 then
      none
    else
      let a := (v*u - w*s)/det
      let b := (r*w - v*t)/det
      if a*m.buttonA + b*m.buttonB == m.prize then
        3*a + b
      else
        none

def solveA (ms : Input) : Int := (ms.filterMap solveMachine).sum

------------------------------------------------------------
-- Part B

def solveB (ms : Input) : Int :=
  let offset : V2 Int := V2.mk 10000000000000 10000000000000
  ms.map (λ m => { m with prize := m.prize + offset })
  |> List.filterMap solveMachine
  |> List.sum

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day13/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
