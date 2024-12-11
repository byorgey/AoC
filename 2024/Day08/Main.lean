import AoC
import Batteries.Data.HashMap

open Batteries

------------------------------------------------------------
-- Data representation

abbrev Input := Grid Char

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input :=
  Grid.ofList $ s.lines.map String.toList

------------------------------------------------------------
-- Generic solution

def antennaMap (g : Grid Char) : HashMap Char (List (V2 Int)) :=
  let antennas := g.toList.filter (Prod.snd ▸ (· ≠ '.'))
  HashMap.ofListWith (antennas.map (Prod.swap ▸ second List.singleton)) (· ++ ·)

def solve (antinodes : Grid Char → V2 Int × V2 Int → List (V2 Int)) (g : Input) : Nat :=
  antennaMap g
    |> HashMap.toList
    |> List.map (Prod.snd ▸ List.pairs ▸ (λ l => l.flatMap (antinodes g)))
    |> List.flatten
    |> List.filter g.contains
    |> Std.HashSet.ofList
    |> Std.HashSet.size

------------------------------------------------------------
-- Part A

def antinodesA : V2 Int × V2 Int → List (V2 Int)
  | (p,q) => [q + q - p, p + p - q]

def solveA := solve (λ _ => antinodesA)

------------------------------------------------------------
-- Part B

-- Attempt at a fancy analytic solution, which does not work

-- -- Let q.x > p.x  (if q.x = p.x this does not generate a constraint)
-- -- p.x + k*(q.x - p.x) >= 0
-- -- k*(q.x - p.x) >= -p.x
-- -- k >= -p.x/(q.x - p.x)

-- -- Similarly,
-- --
-- -- p.x + k*(q.x - p.x) < R
-- -- k < (R - p.x)/(q.x - p.x)

-- def kmin (a b : Int) : Option Int :=
--   let p := min a b
--   let q := max a b
--   if p = q then none else (-p + q - p - 1)/(q - p)

-- def kmax (r a b : Int) : Option Int :=
--   let p := min a b
--   let q := max a b
--   if p = q then none else (r - p - 1)/(q - p)

-- def antinodesB : Int → Int → V2 Int × V2 Int → List (V2 Int)
--   | r, c, (p,q) =>
--       let lo := (max <$> kmin p.r q.r <*> kmin p.c q.c).get!
--       let hi := (min <$> kmax r p.r q.r <*> kmax c p.c q.c).get!
--       (Int.range lo (hi+1)).map (λ k => p + k*(q-p))

-- #eval antinodesB 10 10 (V2.mk 3 3, V2.mk 2 4)

------------------------------------------------------------

-- Return the largest span of integers centered around 0 which all
-- satisfy a given predicate.
partial def Int.spanP (P : Int → Bool) : List Int :=
  (go (P ∘ Int.neg) 1).reverse.map Int.neg ++ go P 0
 where
  go (Q : Int → Bool) (k : Int) : List Int :=
    if Q k then k :: go Q (k+1) else []

def antinodesB : Grid Char → V2 Int × V2 Int → List (V2 Int)
  | g, (p,q) =>
    let node (k : Int) := p + k*(q-p)
    (Int.spanP (g.contains ∘ node)).map node

def solveB :=  solve antinodesB

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day08/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
