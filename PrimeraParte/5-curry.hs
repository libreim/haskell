suma5 :: Int -> Int
suma5 = (5+)

mult4 :: Int -> Int
mult4 = (4*)

--
g :: Int -> Int
g = suma5 . mult4 -- g x = 4*x + 5
