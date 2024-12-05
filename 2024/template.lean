import AoC2024

------------------------------------------------------------
-- Data representation

abbrev Input := Unit

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input := sorry

------------------------------------------------------------
-- Part A

def solveA (i : Input) : Nat := sorry

------------------------------------------------------------
-- Part B

def solveB (is : Input) : Nat := sorry

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "DayXX/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
