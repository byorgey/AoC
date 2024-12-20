import AoC

------------------------------------------------------------
-- Data representation

structure Input where
  track : Grid Bool
  start : V2 Int
  «end» : V2 Int
deriving Inhabited

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input :=
  let gc : Grid Char := Grid.ofList $ s.lines.map String.toList
  let s := (gc.findIdx? (· == 'S')).get!
  let e := (gc.findIdx? (· == 'E')).get!
  Input.mk (gc.map (· != '#')) s e

------------------------------------------------------------
-- Shortcuts

-- From a given base point p compute all q such that (1) the Manhattan
-- distance between p and q is ≤ d, and (2) p < q lexicographically
def V2.manhattanHalfNeighborhood (p : V2 Int) (d : Nat) : List (V2 Int) :=
  [ V2.mk p.r c' | for c' in Int.range (p.c + 1) (p.c + d + 1) ] ++
  [ V2.mk r' c'
  | for r' in Int.range (p.r + 1) (p.r + d + 1)
  , let rem := d - (r' - p.r)
  , for c' in Int.range (p.c - rem) (p.c + rem + 1)
  ]

def shortcuts (i : Input) (cheat : Nat) (cutoff : Nat) : Nat :=
  let neighbors v := v.neighbors4.filter i.track.get!
  let sssp := BFS [i.end] neighbors

  let timesave (p q : V2 Int) : Nat :=
    (sssp.dist.find! p).absdiff (sssp.dist.find! q) - (p.manhattan q)

  let isShortcut (p q : V2 Int) : Bool :=
    i.track.get! p && i.track.get! q && timesave p q ≥ cutoff

  let shortcutsFrom (p : V2 Int) : Nat :=
    p.manhattanHalfNeighborhood cheat
      |> List.filter (i.track.contains)
      |> List.countP (isShortcut p)

  sssp.dist.toList
    |> List.map Prod.fst
    |> List.map shortcutsFrom
    |> List.sum

------------------------------------------------------------
-- Part A

def solveA (i : Input) : Nat := shortcuts i 2 100

------------------------------------------------------------
-- Part B

def solveB (i : Input) : Nat := shortcuts i 20 100

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day20/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
