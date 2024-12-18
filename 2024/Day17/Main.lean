import AoC
import Mathlib.Data.List.Monad

------------------------------------------------------------
-- Data representation

structure Computer where
  regA : Nat
  regB : Nat
  regC : Nat
  opcodes : Array Nat
  ip : Nat
  out : Array Nat
deriving Repr, Inhabited

abbrev Input := Computer

------------------------------------------------------------
-- Parsing

def readComputer : List Nat → Computer
  | a :: b :: c :: os => Computer.mk a b c (Array.mk os) 0 #[]
  | _ => panic "no parse"

def parse (s : String) : Input :=
  s
  |> String.map (λ c => if c.isDigit then c else ' ')
  |> String.words
  |> List.map String.toNat!
  |> readComputer

------------------------------------------------------------
-- Part A

def Computer.step (c : Computer) : Option Computer :=
  if h : c.ip + 1 < c.opcodes.size then
    let opcode := c.opcodes[c.ip]
    let operand := c.opcodes[c.ip+1]
    let combo := match operand with
      | 4 => c.regA
      | 5 => c.regB
      | 6 => c.regC
      | _ => operand
    match opcode with
      | 0 => some { c with regA := c.regA.shiftRight combo, ip := c.ip + 2 }
      | 1 => some { c with regB := c.regB.xor operand, ip := c.ip + 2 }
      | 2 => some { c with regB := combo % 8, ip := c.ip + 2 }
      | 3 =>
          if c.regA == 0 then
            some { c with ip := c.ip + 2 }
          else
            some { c with ip := operand }
      | 4 => some { c with regB := c.regB.xor c.regC, ip := c.ip + 2 }
      | 5 => some { c with out := c.out.push (combo % 8), ip := c.ip + 2 }
      | 6 => some { c with regB := c.regA.shiftRight combo, ip := c.ip + 2 }
      | 7 => some { c with regC := c.regA.shiftRight combo, ip := c.ip + 2 }
      | _ => none
  else
    none

def Computer.run (c : Computer) : List Nat :=
  (iter' Computer.step c).out.toList

def solveA (i : Input) : String := ",".intercalate (i.run.map toString)

------------------------------------------------------------
-- Part B

-- B ← (A % 8) ^ 5
-- C ← A >> B
-- B ← B ^ 6
-- B ← B ^ C
-- out (B % 8)

-- A ← A >> 3
-- jnz 0

def solveB (i : Input) : Nat := (go i.opcodes.toList).head!
  where
    go : List Nat → List Nat
    | [] => pure 0
    | n :: ns => do
        let A ← go ns
        let op ← List.range 8
        let A' := A * 8 + op
        match ({ i with regA := A' }.run) with
          | x :: _ => if x == n then pure A' else []
          | _ => []

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day17/input"
  IO.println (solveA input)
  IO.println s!"{solveB input}"
