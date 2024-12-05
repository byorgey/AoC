structure V2 (α : Type) where
  x : α
  y : α
deriving Repr

instance [Coe α β] : Coe (V2 α) (V2 β) where
  coe p := V2.mk (↑ p.x) (↑ p.y)

instance [Zero α] : Zero (V2 α) where
  zero := V2.mk 0 0

instance [Add α] : HAdd (V2 α) (V2 α) (V2 α) where
  hAdd p q := { x := p.x + q.x, y := p.y + q.y }

instance [Sub α] : HSub (V2 α) (V2 α) (V2 α) where
  hSub p q := { x := p.x - q.x, y := p.y - q.y }

instance [Neg α] : Neg (V2 α) where
  neg p := { x := -p.x, y := -p.y }

instance [Mul α] : HMul α (V2 α) (V2 α) where
  hMul k p := { x := k * p.x, y := k * p.y }

instance [Div α] : HDiv (V2 α) α (V2 α) where
  hDiv p k := { x := p.x / k, y := p.y / k }

