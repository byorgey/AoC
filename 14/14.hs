import           Crypto.Hash.MD5        (hash)
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8  (pack, unpack)
import           System.Environment

hashStream :: Int -> String -> [String]
hashStream stretch salt = map mkHash [0..]
  where
    mkHash = unpack . (!!(stretch + 1)) . iterate (encode . hash) . pack . (salt++) . show

triple :: String -> Maybe Char
triple (a:b:c:s)
  | a == b && b == c = Just c
  | otherwise = triple (b:c:s)
triple _ = Nothing

quints :: String -> [Char]
quints xs | length xs < 5 = []
quints (x:xs)
  | all (==x) (take 4 xs) = x : quints xs
  | otherwise = quints xs

main = do
  [salt] <- getArgs
  let hashes = zip [0..] (hashStream 0 salt)

  print $ (findKeys hashes !! 63)

  let hashes2 = zip [0..] (hashStream 2016 salt)

  print $ (findKeys hashes2 !! 63)

findKeys :: [(Int, String)] -> [Int]
findKeys ((i,h):hs) =
  case triple h of
    Just x -> if (any (\(_,h') -> x `elem` quints h') (take 1000 hs)) then i : findKeys hs else findKeys hs
    Nothing -> findKeys hs
