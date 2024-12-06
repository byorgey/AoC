import AoC

------------------------------------------------------------
-- Parsing

abbrev Input := List (List Int)

def parse (s : String) : Input := s.lines.map (List.map String.toInt! ∘ String.words)

------------------------------------------------------------
-- Part A

def fwdDiff (xs : List Int) : List Int := List.zipWith (· - ·) (xs.drop 1) xs

def safeA (record : List Int) : Bool :=
  [[1,2,3], [-1,-2,-3]].any (λ ds => (fwdDiff record).all (List.elem · ds))

def solveA := List.countP safeA

------------------------------------------------------------
-- Part B

def dampen : List α → List (List α)
  | [] => []
  | a :: as => as :: List.map (a :: ·) (dampen as)

def safeB (record : List Int) : Bool := safeA record || (dampen record).any safeA

def solveB := List.countP safeB

------------------------------------------------------------
-- main

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "Day02/input"
  IO.println s!"{solveA input}"
  IO.println s!"{solveB input}"

------------------------------------------------------------
-- Practice proving a random theorem.

-- Alternative definition of fwdDiff which will be easier to prove things about
def fwdDiff2 : List Int → List Int
  | [] => []
  | [_] => []
  | (x1 :: x2 :: xs) => (x2 - x1) :: fwdDiff2 (x2 :: xs)

-- fwdDiff and fwdDiff2 are the same function.
theorem fwdDiff_fwdDiff2 : (xs : List Int) → fwdDiff xs = fwdDiff2 xs
  | [] => rfl
  | [_] => rfl
  | (x1 :: x2 :: xs) => calc
      fwdDiff (x1 :: x2 :: xs)
      _ = List.zipWith (· - ·) ((x1 :: x2 :: xs).drop 1) (x1 :: x2 :: xs) := by rfl
      _ = List.zipWith (· - ·) (x2 :: xs) (x1 :: x2 :: xs)                := by rfl
      _ = (x2 - x1) :: List.zipWith (· - ·) xs (x2 :: xs)                 := by rfl
      _ = (x2 - x1) :: fwdDiff (x2 :: xs)                                 := by rfl
      _ = (x2 - x1) :: fwdDiff2 (x2 :: xs)             := by simp [fwdDiff_fwdDiff2]
      _ = fwdDiff2 (x1 :: x2 :: xs)                                       := by rfl

theorem fwdDiff2_append :
  (xs : List Int) →
  {x1 x2 : Int} →
  fwdDiff2 ((xs ++ [x2]) ++ [x1]) = fwdDiff2 (xs ++ [x2]) ++ [x1 - x2] := λ xs {x1} {x2} =>
  match xs with
  | [] => by simp [fwdDiff2]
  | [a] => by simp [fwdDiff2]
  | a1 :: a2 :: as => calc
     fwdDiff2 (a1 :: a2 :: as ++ [x2] ++ [x1])
     _ = (a2 - a1) :: fwdDiff2 (a2 :: as ++ [x2] ++ [x1]) := rfl
     _ = (a2 - a1) :: (fwdDiff2 (a2 :: as ++ [x2]) ++ [x1 - x2]) := by rewrite [fwdDiff2_append]; rfl
     _ = (a2 - a1) :: fwdDiff2 (a2 :: as ++ [x2]) ++ [x1 - x2] := rfl
     _ = fwdDiff2 ((a1 :: a2 :: as) ++ [x2]) ++ [x1 - x2] := rfl

theorem fwdDiff2_reverse : (xs : List Int) → fwdDiff2 (List.reverse xs) = List.reverse (List.map Int.neg (fwdDiff2 xs))
  | [] => rfl
  | [_] => rfl
  | x1 :: x2 :: xs => calc
    fwdDiff2 (List.reverse (x1 :: x2 :: xs))
    _ = fwdDiff2 ((List.reverse xs ++ [x2]) ++ [x1])                                                     := by simp [List.reverse_cons]
    _ = fwdDiff2 (List.reverse xs ++ [x2]) ++ [x1 - x2]                                                  := fwdDiff2_append (List.reverse xs)
    _ = List.reverse (List.reverse (fwdDiff2 (List.reverse xs ++ [x2]))) ++ [x1 - x2]                    := by simp [List.reverse_reverse]
    _ = List.reverse ((x1 - x2) :: List.reverse (fwdDiff2 (List.reverse xs ++ [x2])))                    := by simp [List.reverse_cons]
    _ = List.reverse ((x1 - x2) :: List.reverse (fwdDiff2 (List.reverse (x2 :: xs))))                    := by simp [List.reverse_cons]
    _ = List.reverse ((x1 - x2) :: List.reverse (List.reverse (List.map Int.neg (fwdDiff2 (x2 :: xs))))) := by rewrite [fwdDiff2_reverse (x2 :: xs)]; rfl
    _ = List.reverse ((x1 - x2) :: List.map Int.neg (fwdDiff2 (x2 :: xs)))                               := by simp [List.reverse_reverse]
    _ = List.reverse (Int.neg (x2 - x1) :: List.map Int.neg (fwdDiff2 (x2 :: xs)))                       := by rewrite [Eq.symm (Int.neg_sub x2 x1)]; rfl
    _ = List.reverse (List.map Int.neg ((x2 - x1) :: fwdDiff2 (x2 :: xs)))                               := by rfl
    _ = List.reverse (List.map Int.neg (fwdDiff2 (x1 :: x2 :: xs)))                                      := by rfl

-- The theorem!
theorem fwdDiff_reverse (xs : List Int) : fwdDiff (List.reverse xs) = List.reverse (List.map Int.neg (fwdDiff xs)) := by
  rewrite [fwdDiff_fwdDiff2 xs, fwdDiff_fwdDiff2 (List.reverse xs), fwdDiff2_reverse]; rfl
