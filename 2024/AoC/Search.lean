import Batteries.Data.HashMap
import Batteries.Data.BinaryHeap
open Batteries

------------------------------------------------------------
-- DFS

partial def DFS [BEq α] [Hashable α] (start : α) (neighbors : α → List α) : HashMap α α :=
  ((go start).run HashMap.empty).snd
  where
    go (u : α) : StateM (HashMap α α) Unit := do
      let parent ← get
      for v in neighbors u do
        if parent.contains v then pure ()
        else
          modify (λ p => p.insert v u)
          go v

------------------------------------------------------------

structure SSSP (α : Type) (γ : Type) [BEq α] [Hashable α] where
  parents : HashMap α (List α)
  dist : HashMap α γ

------------------------------------------------------------
-- BFS

def BFS [BEq α] [Hashable α] (start : List α) (neighbors : α → List α) : SSSP α Nat := Id.run do
  let mut parents : HashMap α (List α) := HashMap.empty
  let mut level : HashMap α Nat := HashMap.ofList (start.map (λ v => (v,0)))
  let mut q : Std.Queue α := Std.Queue.empty

  q := q.enqueueAll start

  while !q.isEmpty do
    match q.dequeue? with
    | none => pure ()
    | some (u, q') => do
      q := q'

      for v in neighbors u do
        if !level.contains v then
          parents := parents.insert v [u]
          level := level.insert v (level.find! u + 1)
          q := q.enqueue v
        else if level.find! v == level.find! u + 1 then
          parents := parents.modify v (λ _ ps => u :: ps)

  pure $ SSSP.mk parents level

------------------------------------------------------------
-- Dijkstra

def Dijkstra
  [BEq α] [Hashable α]
  [Inhabited γ] [Zero γ] [Add γ] [BEq γ] [LT γ] [(x y : γ) → Decidable (x < y)]
  (start : α) (neighbors : α → List (α × γ)) : SSSP α γ := Id.run do

  let mut parents : HashMap α (List α) := HashMap.empty
  let mut dist : HashMap α γ := HashMap.ofList [(start, 0)]
  let cmp : α × γ → α × γ → Bool
    | (_,d₁), (_,d₂) => decide (d₁ > d₂)
  let mut pq : BinaryHeap (α × γ) cmp := BinaryHeap.empty cmp

  pq := pq.insert (start, 0)
  while pq.size > 0 do
    match pq.extractMax with
    | (none, _) => pure ()
    | (some (u, ud), pq') =>
        pq := pq'
        if ud > dist.find! u then
          pure ()
        else
          for (v, d) in neighbors u do
            let vd' := dist.find! u + d
            let same : Bool := ((dist.find? v).map (· == vd')).getD false
            let better : Bool := ((dist.find? v).map (λ vd => decide (vd' < vd))).getD true
            if better then
              dist := dist.insert v vd'
              parents := parents.insert v [u]
              pq := pq.insert (v, vd')
            else if same then
              parents := parents.modify v (λ _ ps => u :: ps)

  pure $ SSSP.mk parents dist
