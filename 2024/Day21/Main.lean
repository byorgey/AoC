import AoC
import Batteries.Data.HashMap
open Batteries

------------------------------------------------------------
-- Data representation

inductive Key where
  | digit : Nat → Key
  | A : Key
  | arrow : V2 Int → Key
deriving Inhabited, Hashable, Repr, BEq

instance : ToString Key where
  toString k := match k with
    | .digit n => s!"{n}"
    | .A => "A"
    | .arrow v =>
        if v == V2.N then "^"
        else if v == V2.E then ">"
        else if v == V2.S then "v"
        else if v == V2.W then "<"
        else "?"

abbrev Code := List Key
abbrev Input := List Code

def Code.toNat (c : Code) : Nat :=
  Nat.ofDigits $ (c.filterMap (λ | .digit d => some d | _ => none)).reverse

def readKey : Char → Key
  | 'A' => .A
  | '^' => .arrow V2.N
  | '>' => .arrow V2.E
  | 'v' => .arrow V2.S
  | '<' => .arrow V2.W
  | d => .digit d.toDigit

def readCode (s : String) : Code := s.toList.map readKey

def allKeys : List Key := (List.range 10).map .digit ++ [.A] ++ V2.cardinals.map .arrow

------------------------------------------------------------
-- Parsing

def parse (s : String) : Input := s.lines.map readCode

------------------------------------------------------------
-- Keypads + robots

abbrev Keypad := Grid Key

def numKP : Keypad := (Grid.ofList
  [ [.digit 7, .digit 8, .digit 9]
  , [.digit 4, .digit 5, .digit 6]
  , [.digit 1, .digit 2, .digit 3]
  , [.A      , .digit 0, .A      ]
  ]).erase (V2.mk 3 0)

def dirKP : Keypad := (Grid.ofList
  [ [.A         , .arrow V2.N, .A         ]
  , [.arrow V2.W, .arrow V2.S, .arrow V2.E]
  ]).erase (V2.mk 0 0)

structure State where
  robots : List (V2 Int)
  code : Code
deriving Inhabited, Hashable, BEq, Repr

def initState (n : Nat) : State :=
  State.mk (List.replicate n (V2.mk 0 2) ++ [V2.mk 3 2]) []

def finalState (n : Nat) (c : Code) : State := { initState n with code := c }

def sendKey (k : Key) (rs : List (V2 Int)) : Option (List (V2 Int) × Option Key) :=
  match rs with
  | [] => some ([], k)
  | p :: ps =>
     let kp := if ps.isEmpty then numKP else dirKP
     match k with
     | .arrow dir => do
         let p' := p + dir
         guard (kp.contains p')
         pure (p' :: ps, none)
     | .digit _ => none
     | .A => match kp.get? p with
         | none => none
         | some k' => first (p :: ·) <$> sendKey k' ps

def send (k : Key) (s : State) : Option State :=
  match sendKey k s.robots with
  | none => none
  | some (rs', none) => some { s with robots := rs' }
  | some (rs', some k') => some { s with robots := rs', code := s.code ++ [k'] }

-- #eval send (.arrow V2.W) (initState 2) >>= send (.arrow V2.S) >>= send .A
--   >>= send (.arrow V2.W) >>= send .A >>= send .A
--   >>= send (.arrow V2.E) >>= send (.arrow V2.E) >>= send (.arrow V2.N)
--   >>= send .A

------------------------------------------------------------
-- Part A

def next (c : Code) (s : State) : List State :=
  (Key.A :: [V2.W, V2.E, V2.N, V2.S].map Key.arrow).filterMap $ λ k => do
    let s' ← send k s
    guard $ s'.code.isPrefixOf c
    pure s'

def shortestFor (n : Nat) (c : Code) : Nat :=
  let sssp := BFS [initState n] (next c) (goal := λ s => s.code == c)
  sssp.dist.find! (finalState n c)

def complexity (n : Nat) (c : Code) : Nat :=
  shortestFor n c * c.toNat

def solveA (i : Input) : Nat := (i.map (complexity 2)).sum

------------------------------------------------------------
-- Part B

-- -- Compute the optimal sequence of keypresses to move from one key to another
-- def optimalSeq (k1 k2 : Key) : List Key := _

-- -- Optimal sequence of keypresses to move from any key to any other
-- def optimalMap : HashMap (Key × Key) (List Key) :=
--   HashMap.ofList [ ((k1, k2), optimalSeq k1 k2) | for k1 in allKeys, for k2 in allKeys ]

-- Calculate the list of keypresses on a directional keypad that will
-- cause the controlled robot to move from 'fr' to 'to' and then press
-- the button at 'to'.  However, we are careful to push the
-- directional buttons in an order that will be optimal for subsequent
-- keypads, but also avoids blank spaces.
def navigate (avoid : V2 Int) (fr : V2 Int) (to : V2 Int) : List Key := Id.run do
  let mut keys : Array Key := #[]

  let mut did_left : Bool := false
  if to.c < fr.c && !(fr.r == avoid.r && to.c == avoid.c) then
    for _ in List.range (to.c.absdiff fr.c) do
      keys := keys.push (.arrow V2.W)
    did_left := true

  -- Up
  if to.r < fr.r then
    for _ in List.range (to.r.absdiff fr.r) do
      keys := keys.push (.arrow V2.N)

  -- Down
  if to.r > fr.r then
    for _ in List.range (to.r.absdiff fr.r) do
      keys := keys.push (.arrow V2.S)

  -- Right
  if to.c > fr.c then
    for _ in List.range (to.c.absdiff fr.c) do
      keys := keys.push (.arrow V2.E)

  -- Left
  if to.c < fr.c && !did_left then
    for _ in List.range (to.c.absdiff fr.c) do
      keys := keys.push (.arrow V2.W)

  -- Finally push A
  keys := keys.push .A
  pure $ keys.toList

def Key.numCoords (k : Key) : V2 Int := (numKP.findIdx? (· == k)).get!

def Key.dirCoords (k : Key) : V2 Int := (dirKP.findIdx? (· == k)).get!

def expand (init : V2 Int) (coords : Key → V2 Int) (avoid : V2 Int) (ks : List Key) : List (List Key) :=
  let cs := (init :: ks.map coords)
  cs.zipWith (navigate avoid) (cs.drop 1)

abbrev SeqCounts := HashMap (List Key) Nat

def initSC (ks : List Key) : SeqCounts :=
  let ss := (expand (V2.mk 3 2) Key.numCoords (V2.mk 3 0) ks).map (· , 1)
  HashMap.ofListWith ss (· + ·)

def expandSC (sc : SeqCounts) : SeqCounts :=
  let sce := sc.toList.flatMap (λ (ks, n) => (expand (V2.mk 0 2) Key.dirCoords (V2.mk 0 0) ks).map (· , n))
  HashMap.ofListWith sce (· + ·)

-- <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
--    <   A > A  v <<   AA >  ^ AA > A  v  AA ^ A   < v  AAA >  ^ A
--        ^   A         <<      ^^   A     >>   A        vvv      A
--            3                      7          9                 A

-- v<<A>>^AvA^Av<<A>>^AAv<A<A>>^AAvAA^<A>Av<A>^AA<A>Av<A<A>>^AAAvA^<A>A
--    <   A > A   <   AA  v <   AA >>  ^ A  v  AA ^ A  v <   AAA >  ^ A
--        ^   A       ^^        <<       A     >>   A        vvv      A
--            3                          7          9                 A

-- <v<A>>^AAAvA^A<vA<AA>>^AAvAA<^A>A<v<A>A>^AAvA^AA<A>A<v<A>A>^A<Av>A^A

def keypress (st1 st2 : State) : Key :=
  let p1 := st1.robots.head!
  let p2 := st2.robots.head!
  if p1 != p2 then
    .arrow (p2 - p1)
  else
    .A

partial def reconstruct (sssp : SSSP State Nat) (st : State) : List Key := (go st).reverse
 where
  go st := match sssp.parents.find? st with
    | none => []
    | some [] => []
    | some (p :: _) => keypress p st :: go p

def complexityB (k : Nat) (c : Code) : Nat :=
  let warmup := 2
  let sssp := BFS [initState warmup] (next c) (goal := λ s => s.code == c)
  let seq2 := reconstruct sssp (finalState warmup c)
  -- dbg_trace seq2
  let initSegs := ((seq2.splitOn .A).reverse.drop 1).reverse.map (· ++ [.A])
  let scs := HashMap.ofListWith (initSegs.map (· , 1)) (· + ·)
  let scs' := (expandSC ^ (k - warmup)) scs
  (scs'.toList.map (λ (s,n) => s.length * n)).sum * c.toNat

def solveB (i : Input) : Nat :=
  (i.map (complexityB 26)).sum

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day21/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"
