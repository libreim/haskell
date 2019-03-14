{- Reconocimiento de patrones -}

not' :: Bool -> Bool
not' True  = False
not' False = True

-- :t ifThenElse?
ifThenElse True  x _ = x
ifThenElse False _ y = y

-- El orden es importante
esNulo :: Int -> Bool
esNulo 0 = True
esNulo _ = False -- _ cuando no usamos el argumento

factorialErroneo :: Int -> Int
factorialErroneo n = n * factorialErroneo (n - 1)
factorialErroneo 0 = 1
