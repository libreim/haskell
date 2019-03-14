{-#LANGUAGE DeriveFunctor, DeriveFoldable #-}

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Eq, Show, Functor, Foldable)


singleton x = Node x Empty Empty

arbol :: Int -> Tree Int
arbol 0 = singleton 1
arbol n = Node 1 subarbol subarbol
  where subarbol = fmap (2*) (arbol (n-1))
