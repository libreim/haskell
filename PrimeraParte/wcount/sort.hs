import Data.List (partition)
import Control.Monad (forever, void)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lower <> [x] <> qsort higher
  where (lower, higher) = partition (<x) xs

main = forever ((readLn :: IO [Int]) >>= print . qsort)
