import AoC.Basic
import AoC.V2
import Batteries.Data.HashMap

open Batteries

structure Grid (α : Type) where
  rows : Nat
  cols : Nat
  grid : HashMap (V2 Int) α
  background : Option α
deriving Inhabited

def Grid.ofList (rs : List (List α)) : Grid α :=
  let g : HashMap (V2 Int) α := rs
             |> List.enum
             |> List.map (λ (r, lst) => (List.enum lst).map (λ (c, x) => (V2.mk r c, x)))
             |> List.flatten
             |> HashMap.ofList
  Grid.mk (rs.length) (rs[0]!.length) g none

def Grid.map (f : α → β) (g : Grid α) : Grid β :=
  { g with grid := g.grid.mapVal (λ _ a => f a), background := f <$> g.background }

def Grid.get! [Inhabited α] (g : Grid α) (i : V2 Int) : α :=
  match g.background with
  | none => g.grid.find! i
  | some a => g.grid.findD i a

def Grid.get? (g : Grid α) (i : V2 Int) : Option α := g.grid.find? i <|> g.background

def Grid.findIdx? (g : Grid α) (p : α → Bool) : Option (V2 Int) :=
  (g.grid.toList.find? (p ∘ Prod.snd)).map Prod.fst

def Grid.insert (g : Grid α) (i : V2 Int) (a : α) : Grid α :=
  { g with grid := g.grid.insert i a }

def Grid.erase (g : Grid α) (i : V2 Int) : Grid α :=
  { g with grid := g.grid.erase i }

def Grid.contains (g : Grid α) (i : V2 Int) : Bool :=
  0 ≤ i.r ∧ i.r < g.rows ∧ 0 ≤ i.c ∧ i.c < g.cols

def Grid.toList (g : Grid α) : List (V2 Int × α) := g.grid.toList

def Grid.keys (g : Grid α) : List (V2 Int) := g.toList.map Prod.fst

/-- Get a list of all the values contained in a `Grid`. -/
def Grid.values (g : Grid α) : List α := g.toList.map Prod.snd

def Grid.neighbors4 (g : Grid α) (i : V2 Int) : List (V2 Int)
  := i.neighbors4.filter g.contains

def Grid.neighbors8 (g : Grid α) (i : V2 Int) : List (V2 Int)
  := i.neighbors8.filter g.contains

------------------------------------------------------------

instance : ToString (Grid Char) where
  toString g :=
    let cs :=
      (List.range g.rows).map $ λ (r : Nat) =>
        (List.range g.cols).map $ λ (c : Nat) =>
          (g.get? (V2.mk r c)).getD ' '
    (cs.map String.mk).unlines
