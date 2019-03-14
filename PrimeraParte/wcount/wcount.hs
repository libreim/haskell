import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy    as T
import qualified Data.Map.Strict   as M
import Data.List
import Text.Printf
import Data.Char

toWords = T.words . T.filter (not . isPunctuation) . T.map toLower

countWords = foldl' addWord mempty
  where addWord counter word = M.insertWith (+) word 1 counter

showMap :: M.Map T.Text Int -> IO ()
showMap hm = putStrLn $ concat $ map showPair selected
  where selected = take 200 $ sortOn (negate . snd) $ M.toList hm
        maxLen   = maximum (map (T.length . fst) selected)
        showPair (k,v) = printf ("%" <> show maxLen <> "s: %d\n") k v

main = T.getContents >>= showMap . countWords . toWords
