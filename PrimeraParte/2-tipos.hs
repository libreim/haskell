powerLevel :: Int
powerLevel = 9001

over9000 :: Bool
over9000 = powerLevel > 9000

holaMundo :: String
holaMundo = (if over9000 then "Hola" else "Adiós") <> ", 🌍!"

brillicos :: Char
brillicos = '✨'

main :: IO ()
main = putStrLn holaMundo

e :: Double
e = exp 1

-- ¿-(x+3) o -x+3?
f :: Int -> Int
f x = negate x + 3

-- Podemos especificar o no el tipo de nand
nand :: Bool -> Bool -> Bool
nand x y = not (x && y)
