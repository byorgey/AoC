-- Split a string into words on whitespace
def String.words (s : String) : List String :=
  (s.split Char.isWhitespace).filter (not ∘ String.isEmpty)
