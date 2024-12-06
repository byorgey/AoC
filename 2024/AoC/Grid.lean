import AoC.Basic
import AoC.V2
import Batteries.Data.HashMap

open Batteries

structure Grid (α : Type) where
  rows : Nat
  cols : Nat
  grid : HashMap (V2 Int) α

def Grid.ofList (rs : List (List α)) : Grid α :=
  let g : HashMap (V2 Int) α := rs
             |> List.enum
             |> List.map (λ (r, lst) => (List.enum lst).map (λ (c, x) => (V2.mk r c, x)))
             |> List.flatten
             |> HashMap.ofList
  Grid.mk (rs.length) (rs[0]!.length) g

def Grid.map (f : α → β) (g : Grid α) : Grid β :=
  { g with grid := g.grid.mapVal (λ _ a => f a) }

def Grid.get! [Inhabited α] (g : Grid α) (i : V2 Int) : α := g.grid.find! i

def Grid.get? (g : Grid α) (i : V2 Int) : Option α := g.grid.find? i

def Grid.findIdx? (g : Grid α) (p : α → Bool) : Option (V2 Int) :=
  (g.grid.toList.find? (p ∘ Prod.snd)).map Prod.fst

def Grid.insert (g : Grid α) (i : V2 Int) (a : α) : Grid α :=
  Grid.mk g.rows g.cols (g.grid.insert i a)

def Grid.contains (g : Grid α) (i : V2 Int) : Bool :=
  0 ≤ i.r ∧ i.r < g.rows ∧ 0 ≤ i.c ∧ i.c < g.cols

def Grid.toList (g : Grid α) : List (V2 Int × α) := g.grid.toList
