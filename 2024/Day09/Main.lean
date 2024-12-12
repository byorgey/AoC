import AoC
import Batteries.Data.HashMap

open Batteries

------------------------------------------------------------
-- Data representation

abbrev Input := List Nat
abbrev FileID := Nat
abbrev Size := Nat

inductive Fragment
  | file : FileID → Size → Fragment
  | empty : Size → Fragment
deriving Repr, Inhabited

instance : ToString Fragment where
  toString
    | .empty sz => s!"<{sz}>"
    | .file f sz => s!"{f}@<{sz}>"

def Fragment.getFile : Fragment → Option (FileID × Size)
  | .file f s => (f, s)
  | .empty _ => none

def Fragment.size : Fragment → Size
  | .file _ s => s
  | .empty s => s

abbrev Disk := List Fragment

def List.toDisk : List Nat → Disk := go 0
  where
    go : Nat → List Nat → Disk
      | _, [] => []
      | f, [l] => [.file f l]
      | f, l :: e :: ls => .file f l :: .empty e :: go (f+1) ls

-- f*(pos) + f*(pos + 1) + f*(pos + 2) + ... + f*(pos + sz - 1)
-- = f*(sz*pos + sz*(sz-1)/2))

def Disk.checksum : Disk → Nat := go 0
  where
    go : Nat → Disk → Nat
    | _, [] => 0
    | pos, .file f sz :: fs => f*(sz*pos + sz*(sz-1)/2) + go (pos + sz) fs
    | pos, .empty sz :: fs => go (pos + sz) fs

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input := s.toList.map (λ c => c.toNat - '0'.toNat)

------------------------------------------------------------
-- Part A

abbrev Mem := Array (Option Nat)

mutual

def expandSpace : Nat → Mem → Input → Mem
  | _, mem, [] => mem
  | i, mem, 0 :: xs => expandFile i mem xs
  | i, mem, (s + 1) :: xs => expandSpace i (mem.push none) (s :: xs)

def expandFile : Nat → Mem → Input → Mem
  | _, mem, [] => mem
  | i, mem, 0 :: xs => expandSpace (i+1) mem xs
  | i, mem, (f + 1) :: xs => expandFile i (mem.push i) (f :: xs)

end

def expand : Input → Mem := expandFile 0 #[]

partial def compactHelper (m : Mem) (i j : Nat) (c : Mem) : Mem :=
  if inBounds : i ≤ j ∧ j < m.size then
    if m[j] = none then
       compactHelper m i (j-1) c
    else if m[i] = none then
       compactHelper m (i+1) (j-1) (c.push (m[j]))
    else compactHelper m (i+1) j (c.push (m[i]))
  else c
-- termination_by j-i

def compact (m : Mem) : Mem := compactHelper m 0 (m.size - 1) #[]

def checksum (m : Mem) : Nat := Id.run do
  let mut i := 0
  let mut s := 0
  for f in m do
    s := s + i * f.getD 0
    i := i + 1
  return s

def solveA (i : Input) : Nat := checksum (compact (expand i))

------------------------------------------------------------
-- Part B

def removeFile : FileID → Disk → Disk
  | _, [] => []
  | f, .empty sz :: fs => .empty sz :: removeFile f fs
  | f, .file g sz :: fs =>
      if f == g then
        .empty sz :: fs
      else
        .file g sz :: removeFile f fs

def insertFile : (FileID × Size) → Disk → Disk
  | _, [] => []
  | (f, sz), .empty space :: fs =>
      if sz == space then
        .file f sz :: removeFile f fs
      else if sz < space then
        .file f sz :: .empty (space - sz) :: removeFile f fs
      else
        .empty space :: insertFile (f,sz) fs
  | (f, fsz), .file g gsz :: fs =>
      if f == g then
        .file g gsz :: fs
      else
        .file g gsz :: insertFile (f, fsz) fs

def defrag (fs : List (FileID × Size)) (d : Disk) : Disk :=
  fs.foldr insertFile d

def solveB (is : Input) : Nat :=
  let disk := is.toDisk
  let files := disk.filterMap Fragment.getFile
  let disk' := defrag files disk
  disk'.checksum

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day09/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
