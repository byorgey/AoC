import AoC.Basic

structure V2 (α : Type) where
  r : α
  c : α
deriving Repr, DecidableEq, Hashable, Inhabited

instance [Coe α β] : Coe (V2 α) (V2 β) where
  coe p := V2.mk (↑ p.r) (↑ p.c)

instance [Zero α] : Zero (V2 α) where
  zero := V2.mk 0 0

instance [Add α] : HAdd (V2 α) (V2 α) (V2 α) where
  hAdd p q := { r := p.r + q.r, c := p.c + q.c }

instance [Sub α] : HSub (V2 α) (V2 α) (V2 α) where
  hSub p q := { r := p.r - q.r, c := p.c - q.c }

instance [Neg α] : Neg (V2 α) where
  neg p := { r := -p.r, c := -p.c }

instance [Mul α] : HMul α (V2 α) (V2 α) where
  hMul k p := { r := k * p.r, c := k * p.c }

instance [Div α] : HDiv (V2 α) α (V2 α) where
  hDiv p k := { r := p.r / k, c := p.c / k }

instance [Mod α] : HMod (V2 α) (V2 α) (V2 α) where
  hMod p q := { r := p.r % q.r, c := p.c % q.c }

instance [ToString α] : ToString (V2 α) where
  toString v := "(" ++ toString (v.r) ++ ", " ++ toString (v.c) ++ ")"

def V2.map (v : V2 α) (f : α → β) : V2 β :=
  { r := f v.r, c := f v.c }

def V2.all (v : V2 α) (p : α → Bool) : Bool := p v.r ∧ p v.c

def V2.rt [Neg α] (v : V2 α) : V2 α := { r := v.c, c := -v.r }

def V2.lt [Neg α] (v : V2 α) : V2 α := { r := -v.c, c := v.r }

def V2.N : V2 Int := V2.mk (-1) 0
def V2.E : V2 Int := V2.mk 0 1
def V2.S : V2 Int := V2.mk 1 0
def V2.W : V2 Int := V2.mk 0 (-1)
def V2.NE : V2 Int := V2.N + V2.E
def V2.SE : V2 Int := V2.S + V2.E
def V2.SW : V2 Int := V2.S + V2.W
def V2.NW : V2 Int := V2.N + V2.W

def V2.cardinals : List (V2 Int) := [V2.N, V2.E, V2.S, V2.W]

def V2.neighbors4 (v : V2 Int) : List (V2 Int) :=
  V2.cardinals.map (v + ·)

def V2.neighbors8 (v : V2 Int) : List (V2 Int) :=
  [V2.N, V2.NE, V2.E, V2.SE, V2.S, V2.SW, V2.W, V2.NW].map (v + ·)

def V2.manhattan (p q : V2 Int) : Nat :=
  p.r.absdiff q.r + p.c.absdiff q.c
