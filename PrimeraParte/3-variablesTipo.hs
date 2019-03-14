-- :t id?
id' x = x

const' :: a -> b -> a
const' x y = x

fst' :: (a,b) -> a
fst' (x,y) = x

swap (x,y) = (y,x)


-- shimweasel.com/2015/02/17/typed-holes-for-beginners
g :: (a -> b) -> (a,c) -> (c,b)
g x y = _resultado
