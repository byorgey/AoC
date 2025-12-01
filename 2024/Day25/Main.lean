import AoC

------------------------------------------------------------
-- Data representation

abbrev Pins := List Nat

inductive Item where
  | key : Item
  | lock : Item
deriving Inhabited, Repr, BEq

abbrev Input := List (Item × Pins)

------------------------------------------------------------
-- Parsing

def readPins (ls : List String) : Pins :=
  ls.map String.toList
  |> List.transpose
  |> List.map (List.filter (· == '#') ▸ List.length)

def parseItem (ls : List String) : Item × Pins :=
  match ls with
  | ("#####" :: ls') => (.lock, readPins (ls'.take 5))
  | (_ :: ls') => (.key, readPins (ls'.take 5))
  | _ => panic "no parse"

def parse (s : String) : Input := (s.lines.splitOn "").map parseItem

------------------------------------------------------------
-- Part A

#eval [1,2,3].partition (· % 2 == 0)

def fit (p1 : Pins) (p2 : Pins) : Bool :=
  (p1.zipWith (· + ·) p2).all (· ≤ 5)

def solveA (i : Input) : Nat :=
  let (keys, locks) := i.partition (Prod.fst · == .key)
  [ 1 | for (_,k) in keys, for (_,l) in locks, if fit k l ].sum

------------------------------------------------------------
-- Part B

def solveB (i : Input) : Nat := sorry

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day25/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
