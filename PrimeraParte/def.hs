
-- Instaladlo!!
haskellPlatform = "haskell.org/platform"

a = 42
--a = 5
f x = x + 2
b = f a + f a + f a

{-
int b = f(a) + f(a) + f(a);
b == 3*f(a) ??
-}
