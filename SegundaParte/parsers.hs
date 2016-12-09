-- Calcula la tabla de multiplicación de un grupo
-- usando el Algoritmo de Knuth-Bendix.
--
-- Usa HaskellForMaths, que puede instalarse como:
--  cabal install HaskellForMaths
import Math.Algebra.Group.StringRewriting
import Text.ParserCombinators.Parsec
import Data.List hiding (group)


-- Definiciones de tipos.
--  * Un generador es un caracter. 'a'
--  * Una expresión es una cadena de generadores. 'aab'
--  * Una relación es un par de expresiones. 'ab = ba'
--  * Una tabla de multiplicación es una tabla de expresiones.
--  * Un grupo está definido por sus generadores y sus relaciones.
type Gen = Char
type Exp = [Gen]
type Rel = (Exp,Exp)
newtype Table = Table [[Exp]]

data Group = Group [Gen] [Rel]
             deriving (Show)

-- Enseñamos cómo debe mostrar por pantalla las tablas de multiplicación
-- y las expresiones. ¡La lista vacía de elementos será la identidad del grupo!
instance Show Table where
  show (Table table) = intercalate "\n" $ fmap (intercalate "\t" . (fmap showexp)) table

showexp :: Exp -> String
showexp "" = "1"
showexp ex = ex




-- Calcula la tabla de multiplicación del grupo,
-- dados sus generadores y sus relaciones.
multtable :: Group -> Table
multtable (Group gens rels) = Table [[rewrite cs (a ++ b) | a <- nf] | b <- nf]
    where cs = knuthBendix rels
          nf = nfs (gens, cs)



-- ¡Parsers!
-- Vamos a usar parsers monádicos. Estos envuelven el resultado
-- dentro de una mónada, y así podemos trabajar con el como si ya lo
-- tuviéramos. Parser a, intenta parsear algo de tipo a.
-- La mónada nos permite componer Parsers.
--
-- Nuestra idea final es la de parsear la presentación de un grupo:
--    < x,y,z | xy = z; xx = 1; yy = 1; zz = 1 >


-- Un generador es una letra.
-- Usamos un parser ya construído.
generator :: Parser Gen
generator = letter

-- La identidad la parseamos aparte, pero es también una
-- expresión. Leemos el caracter 1, pero lo que queremos
-- devolver es la lista vacía, porque ("aa","") representará
-- que "aa" puede sustituirse por el vacío, o la identidad.
identity :: Parser Exp
identity = char '1' >> return ""

-- Una lista de generadores la forman generadores separados
-- por comas: x,y,z
--
-- sepBy toma dos parsers y devuelve varias instancias del
-- primero separadas por el segundo.
generatorList :: Parser [Gen]
generatorList = generator `sepBy` (char ',')

-- Una expresión la forman varios generadores, literalmente,
-- o quizás es sólo un 1, identidad del grupo
expression :: Parser Exp
expression = try identity <|> many generator

-- Para leer una relación ab = ba debo leer la primera parte, un
-- igual, y la segunda parte.
relation :: Parser Rel
relation = do
  lefthand <- expression
  char '='
  righthand <- expression
  return (lefthand, righthand)

-- Lee un grupo finitamente generado como:
--  < a,b | ab = ba >
group :: Parser Group
group = do
  char '<'
  genList <- generatorList
  char '|'
  relations <- relation `sepBy` (char ';')
  char '>'
  return (Group genList relations)



-- Lee desde la entrada estándar, filtrando los espacios
-- devuelve la tabla de multiplicación del grupo
main :: IO ()
main = do
  solv <- fmap multtable <$> (parse group "") <$> filteredinput
  putStrLn $ either (const "Error") show solv
  where
    filteredinput :: IO String
    filteredinput = filter (/= ' ') <$> getLine


{-

Ejemplos de uso.

Grupo de Klein. Z/2Z x Z/2Z
< a,b | aa = 1; bb = 1; ab = ba >
1	a	b	ab
a	1	ab	b
b	ab	1	a
ab	b	a	1

Grupo diédrico de orden 3. D3
< s,t | ss=1; ttt=1; ts=stt >       
1	s	t	st	ts	tt
s	1	ts	tt	t	st
t	st	tt	ts	s	1
st	t	s	1	tt	ts
ts	tt	st	t	1	s
tt	ts	1	s	st	t

-}
