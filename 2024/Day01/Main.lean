import AoC2024

def splitPair (s : String) : Nat × Nat :=
  match s.words with
    | [x,y] => (x.toNat!, y.toNat!)
    | _ => (0, 0)

abbrev Input := List Nat × List Nat
abbrev Output := Nat

def diff (a : Nat) (b : Nat) : Nat := max a b - min a b

def solveA : Input → Output
  | (xs, ys) => (List.zipWith diff xs.mergeSort ys.mergeSort).sum

def solveB : Input → Output
  | (xs, ys) => (xs.map (fun x => x * ys.count x)).sum

-- which style is better?
def parse : String → Input := List.unzip ∘ List.map splitPair ∘ fun s => s.splitOn "\n"
def parse2 (input : String) : Input := ((input.splitOn "\n").map splitPair).unzip

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day01/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
