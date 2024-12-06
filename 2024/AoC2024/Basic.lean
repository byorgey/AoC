import Std.Data.HashSet
open Std

def String.lines (s : String) : List String :=
  dropFinalEmpty (s.splitOn "\n")
 where
  dropFinalEmpty : List String → List String
    | [] => []
    | [ "" ] => []
    | x :: xs => x :: dropFinalEmpty xs

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
