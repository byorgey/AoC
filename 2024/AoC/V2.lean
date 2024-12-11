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

instance [ToString α] : ToString (V2 α) where
  toString v := "(" ++ toString (v.r) ++ ", " ++ toString (v.c) ++ ")"

def V2.rt [Neg α] (v : V2 α) : V2 α := { r := v.c, c := -v.r }

def V2.lt [Neg α] (v : V2 α) : V2 α := { r := -v.c, c := v.r }

def V2.N : V2 Int := V2.mk (-1) 0
def V2.E : V2 Int := V2.mk 0 1
def V2.S : V2 Int := V2.mk 1 0
def V2.W : V2 Int := V2.mk 0 (-1)
