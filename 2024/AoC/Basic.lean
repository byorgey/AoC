import Std.Data.HashSet
import Batteries.Data.HashMap
open Std

def String.lines (s : String) : List String :=
  dropFinalEmpty (s.splitOn "\n")
 where
  dropFinalEmpty : List String → List String
    | [] => []
    | [ "" ] => []
    | x :: xs => x :: dropFinalEmpty xs

def List.unlines (ls : List String) : String :=
  "\n".intercalate ls ++ "\n"

-- Split a string into words on whitespace
def String.words (s : String) : List String :=
  (s.split Char.isWhitespace).filter (not ∘ String.isEmpty)

def List.toPair [Inhabited α] : List α → (α × α)
  | [x,y] => (x, y)
  | _ => panic "Not a pair!"

def first : (α → β) → (α × γ) → (β × γ)
  | f, (a, c) => (f a, c)

def second : (α → β) → (γ × α) → (γ × β)
  | f, (c, a) => (c, f a)

infixr:90 " ▸ " => fun f g ↦ Function.comp g f

def iter (n : Nat) (f : α → Option α) (a : α) : List α :=
  a :: match n, f a with
    | 0, _ => []
    | _, none => []
    | n' + 1, some a' => iter n' f a'

partial def iter' (f : α → Option α) (a : α) : α :=
  match f a with
    | none => a
    | some a' => iter' f a'

-- Copied from Mathlib.Data.Int.Range
def Int.range (a : Int) (b : Int) : List Int :=
  (List.range (toNat (b - a))).map λ (r : Nat) => (a + r : Int)

def Char.toDigit (c : Char) : Nat := c.toNat - '0'.toNat

def Nat.digits : Nat → List Nat
  | 0 => []
  | (n+1) => (n+1).mod 10 :: ((n+1) / 10).digits

def Nat.ofDigits (ds : List Nat) : Nat := ds.foldr (λ d n => 10*n + d) 0

def funPow : (α → α) → Nat → (α → α)
  | _, 0 => id
  | f, n+1 => funPow f n ∘ f

instance : NatPow (α → α) where
  pow := funPow

def List.cardinality [BEq α] [Hashable α] (xs : List α) : Batteries.HashMap α Nat :=
  Batteries.HashMap.ofListWith (xs.map (·,1)) (·+·)

def List.prod : List Nat → Nat := foldr (· * ·) 1
