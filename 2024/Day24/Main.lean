import AoC
import Batteries.Data.HashMap
open Batteries

------------------------------------------------------------
-- Data representation

inductive Op where
  | AND : Op
  | OR : Op
  | XOR : Op
deriving Repr, Inhabited, BEq, Hashable

def Op.eval : Op → Bool → Bool → Bool
  | .AND => (· && ·)
  | .OR => (· || ·)
  | .XOR => xor

def Op.label : Op → String
  | .AND => "∧"
  | .OR => "∨"
  | .XOR => "⊕"

abbrev Wire := String

structure Gate where
  op : Op
  input1 : Wire
  input2 : Wire
  output : Wire
deriving Repr, Inhabited, BEq, Hashable

abbrev Circuit := HashMap Wire Bool × List Gate

------------------------------------------------------------
-- Parsing

def parseWire : String → (Wire × Bool) :=
  String.map (λ c => if c == ':' then ' ' else c) ▸
  String.words ▸
  (λ | [w, b] => (w, b == "1") | _ => panic "no parse")

def parseWires : List String → HashMap Wire Bool :=
  List.map parseWire ▸ HashMap.ofList

def parseOp : String → Op
  | "AND" => .AND
  | "OR" => .OR
  | "XOR" => .XOR
  | _ => panic "no parse"

def parseGate : String → Gate :=
  String.words ▸
  (λ | [w1, op, w2, _, o] => Gate.mk (parseOp op) w1 w2 o
     | _ => panic "no parse")

def parseGates : List String → List Gate := List.map parseGate

def parse (s : String) : Circuit :=
  match s.lines.splitOn "" with
  | [ws, gs] => (parseWires ws, parseGates gs)
  | _ => panic "no parse"

------------------------------------------------------------
-- Part A

def Circuit.run (c : Circuit) : HashMap Wire Bool :=
  -- build maps from wires to gates
  -- let mOut : HashMap Wire Gate := HashMap.ofList $ c.2.map (λ g => (g.output, g))
  let mIn : HashMap Wire (List Gate) :=
    HashMap.ofListWith (c.2.flatMap (λ g => [(g.input1, [g]), (g.input2, [g])])) (· ++ ·)
  -- build directed graph of gates
  let gates : HashMap Gate (List Gate) :=
    HashMap.ofList (c.2.map (λ g => (g, mIn.findD g.output [])))
  -- map from gates to # of inputs full

  -- topologically sort gates
  let topsort : List Gate := Id.run do
    let mut q := Std.Queue.empty
    let mut full : HashMap Gate Nat :=
      HashMap.ofList (c.2.map (λ g => (g, (c.1.contains g.input1).toNat + (c.1.contains g.input2).toNat)))
    let mut T : Array Gate := #[]
    for (g, f) in full.toList do
      if f == 2 then
        q := q.enqueue g

    while !q.isEmpty do
      match q.dequeue? with
      | none => ()
      | some (g,q') =>
          q := q'

          T := T.push g
          for g' in gates.findD g [] do
            full := full.modify g' (λ _ n => n + 1)
            if full.find! g' == 2 then
              q := q.enqueue g'

    pure T.toList

  -- iterate through topsort and fill in values
  Id.run do
    let mut vals : HashMap Wire Bool := c.1
    for g in topsort do
      vals := vals.insert g.output (g.op.eval (vals.find! g.input1) (vals.find! g.input2))

    pure vals

def solveA (c : Circuit) : Nat :=
  let outputs := c.run.toList.filter (λ (w, _) => w.startsWith "z")
  let sortedOutputs := outputs.mergeSort (le := λ (w1,_) (w2,_) => w1 ≤ w2)
  (sortedOutputs.map Prod.snd).foldr (λ b n => 2*n + (if b then 1 else 0)) 0

------------------------------------------------------------
-- Part B

def gateVertex : Gate × Nat → String
  | (g, i) => s!"g{i} [label=\"{g.op.label}\", shape=\"invtrapezium\"];"

def wireVertex (w : String) : String := s!"{w} [shape=\"circle\"];"

def gateEdges : Gate × Nat → List String
  | (g, i) => [s!"{g.input1} -> g{i};", s!"{g.input2} -> g{i};", s!"g{i} -> {g.output};"]

def toDot (c : Circuit) : String :=
  let gateList : List (Gate × Nat) := c.2.zip (List.range c.2.length)
  let wireList : List Wire := c.1.toList.map Prod.fst ++ c.2.map Gate.output

  (["digraph G {"] ++
   gateList.map gateVertex ++
   wireList.map wireVertex ++
   gateList.flatMap gateEdges ++
   ["}"]
  ).unlines

-- z06, dhg
-- dpd, brk
-- z23, bhd
-- z38, nbf

-- bhd,brk,dhg,dpd,nbf,z06,z23,z38

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day24/input"
  IO.println s!"{solveA input}"
  IO.println (toDot input)
