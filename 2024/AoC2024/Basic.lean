def dropFinalEmpty : List String → List String
  | [] => []
  | [ "" ] => []
  | x :: xs => x :: dropFinalEmpty xs

def String.lines (s : String) : List String :=
  dropFinalEmpty (s.splitOn "\n")

-- Split a string into words on whitespace
def String.words (s : String) : List String :=
  (s.split Char.isWhitespace).filter (not ∘ String.isEmpty)

def List.pairs : List α → List (α × α)
  | [] => []
  | a :: as => as.map (a, ·) ++ as.pairs
