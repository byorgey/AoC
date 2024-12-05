@[simp]
def dropFinalEmpty : List String → List String
  | [] => []
  | [ "" ] => []
  | x :: xs => x :: dropFinalEmpty xs

@[simp]
def String.lines (s : String) : List String :=
  dropFinalEmpty (s.splitOn "\n")

-- Split a string into words on whitespace
@[simp]
def String.words (s : String) : List String :=
  (s.split Char.isWhitespace).filter (not ∘ String.isEmpty)

def List.pairs : List α → List (α × α)
  | [] => []
  | a :: as => as.map (a, ·) ++ as.pairs

def List.toPair [Inhabited α] : List α → (α × α)
  | [x,y] => (x, y)
  | _ => panic! "Not a pair!"

def first : (α → β) → (α × γ) → (β × γ)
  | f, (a, c) => (f a, c)

def second : (α → β) → (γ × α) → (γ × β)
  | f, (c, a) => (c, f a)
