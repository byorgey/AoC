import AoC2024
import Parser

------------------------------------------------------------
-- Parsing

inductive Instruction where
  | mul : Nat → Nat → Instruction
  | «do» : Instruction
  | don't : Instruction

abbrev Input := List Instruction

namespace Day03

open Parser Char

protected abbrev Parser := Parser Unit Substring Char

protected def parseMul : Day03.Parser (Nat × Nat) :=
  string "mul" *>
  char '(' *>
  ASCII.parseNat >>= λ x =>
  char ',' *>
  ASCII.parseNat >>= λ y =>
  char ')' *>
  pure (x, y)

#check Function.const

protected def parseInstr : Day03.Parser Instruction :=
  ((λ _ => Instruction.don't) <$> string "don't") <|>
  ((λ _ => Instruction.«do») <$> string "do") <|>
  (Function.uncurry Instruction.mul <$> Day03.parseMul)

protected def parseInstrs : Day03.Parser (Array Instruction) :=
  takeMany (dropUntil Day03.parseInstr anyToken)

end Day03

def parse (s : String) : Input := match Parser.run Day03.parseInstrs s with
  | .error _ _ => []
  | .ok _ r => r.toList

------------------------------------------------------------
-- Part A

def List.filterMuls : List Instruction → List (Nat × Nat)
  | [] => []
  | .mul a b :: is => (a,b) :: filterMuls is
  | _ :: is => filterMuls is

def solveA (i : Input) := (i.filterMuls.map (Function.uncurry (· * ·))).sum

------------------------------------------------------------
-- Part B

def interpMuls : Bool → List Instruction → List Nat
  | _, [] => []
  | _, .«do» :: is => interpMuls True is
  | _, .don't :: is => interpMuls False is
  | true, .mul x y :: is => x*y :: interpMuls true is
  | false, .mul _ _ :: is => interpMuls false is

def solveB (is : Input) := (interpMuls True is).sum

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day03/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
