import System.Environment
import Data.Int(Int64)

loop :: Int64 -> Int64
loop n = go n 0
  where go n sum | n > 0 = go (n-1) (sum+n)
                 | otherwise = sum

main = do
  [size] <- getArgs
  print (loop (read size))
