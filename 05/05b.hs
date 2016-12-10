import           Crypto.Hash.MD5        (hash)
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8  (pack, unpack)
import qualified Data.Map               as M
import           Data.Maybe             (catMaybes)
import           System.Environment

hits salt
  = catMaybes
  . map extract
  . map mkHash
  $ [0..]
  where
    mkHash = unpack . encode . hash . pack . (salt++) . show

password salt = go M.empty (hits salt)
  where
    go m _ | M.size m == 8 = map snd $ M.assocs m
    go m ((pos, c):rest)
      | not (M.member pos m) = go (M.insert pos c m) rest
      | otherwise            = go m rest

extract :: String -> Maybe (Int, Char)
extract h
  = case (splitAt 5 h) of
      ("00000", p:c:_) | p `elem` "01234567" -> Just (read [p], c)
      _                                      -> Nothing

main = do
  [salt] <- getArgs
  putStrLn (password salt)
