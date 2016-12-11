import           Control.Applicative
import           Data.List

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

(<&&>) = liftA2 (&&)

isNice :: String -> Bool
isNice = enoughVowels <&&> hasDouble <&&> noForbidden
  where
    enoughVowels = (>=3) . count isVowel
    isVowel = (`elem` "aeiou")

    hasDouble = any isDouble . tails
    isDouble (x:y:_) = x == y
    isDouble _ = False

    noForbidden str = not (any (`isInfixOf` str) ["ab", "cd", "pq", "xy"])

isNice2 :: String -> Bool
isNice2 = hasRepeatedPair <&&> hasRepeatedLetter
  where
    hasRepeatedPair = any firstPairRepeats . tails
    firstPairRepeats (x:y:rest) = [x,y] `isInfixOf` rest
    firstPairRepeats _ = False

    hasRepeatedLetter = any firstRepeats . tails
    firstRepeats (x:_:z:_) = x == z
    firstRepeats _ = False

main = do
  inp <- getContents
  print . count isNice . lines $ inp
  print . count isNice2 . lines $ inp
