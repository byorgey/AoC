def List.pairs : List α → List (α × α)
  | [] => []
  | a :: as => as.map (a, ·) ++ as.pairs

def choose : Nat → List α → List (List α)
  | 0, _ => [[]]
  | _, [] => []
  | k, (x :: xs) => ((choose (k-1) xs).map (List.cons x)).append (choose k xs)

example : choose 2 [5,6,7] = [[5,6], [5,7], [6,7]] := by rfl
