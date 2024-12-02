import AoC2024

------------------------------------------------------------
-- Parsing

abbrev Input := List (List Int)

def parse (s : String) : Input := s.lines.map (List.map String.toInt! ∘ String.words)

------------------------------------------------------------
-- Part A

def fwdDiff (xs : List Int) : List Int := List.zipWith (· - ·) (xs.drop 1) xs

def safeA (record : List Int) : Bool :=
  [[1,2,3], [-1,-2,-3]].any (λ ds => (fwdDiff record).all (List.elem · ds))

def solveA := List.countP safeA

------------------------------------------------------------
-- Part B

def dampen : List α → List (List α)
  | [] => []
  | a :: as => as :: List.map (a :: ·) (dampen as)

def safeB (record : List Int) : Bool := safeA record || (dampen record).any safeA

def solveB := List.countP safeB

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day02/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
