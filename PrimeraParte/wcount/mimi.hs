import Control.Monad (forever)

mimi :: String -> String
mimi = map mimiChar
  where mimiChar x =
          if x `elem` "aeiou" then 'i'
          else if x `elem` "áéíóú" then 'í'
          else x

main = forever (interact mimi)
