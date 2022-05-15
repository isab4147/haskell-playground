module Lista where
import Data.Monoid

-- 1.a
list1 = [x * 5 | x <- [0..10]]

-- 2.a
inList :: Eq a => a -> [a] -> Bool
inList e [] = False
inList e (x:xs) = e == x || inList e xs

-- 1.b
list2 = [x | x <- ['a'..'z'], not $ inList x "aeiou"]

-- 1.c
list3 = [x | x <- [0..50], not $ inList x [2,7,13,35,42]]

-- 2.c
pow :: (Eq a, Fractional a) => a -> Int -> a
pow 0 0 = undefined
pow b 0 = 1
pow b e | e > 0 = b * pow b (e - 1)
        | otherwise = pow (1 / b) (- e)

-- 1.d
list4 = [500 / (2 `pow` x) | x <- [0..9]]

-- 1.e
list5 = [(x, y) | x <- ['a'..'h'], y <- [1..8]]

maxList :: (Num a, Ord a) => [a] -> Maybe a
maxList [] = Nothing
maxList [a] = Just a
maxList [a, b] = Just $ max a b
maxList (a:b:xs) = maxList $ max a b : xs

maybeCompare :: Maybe Bool -> Bool
maybeCompare (Just True) = True
maybeCompare _ = False

-- 2.b
triangleSides :: (Num a, Ord a) => a -> a -> a -> Bool
triangleSides a b c | maybeCompare $ fmap (/= a) $ maxList [a, b, c] = triangleSides b c a
                    | otherwise = a > b + c

-- 3.a
-- data Cor = Cor Int Int Int


-- 3.b
-- red :: Cor -> Int
-- red (Cor r _ _) = r

-- green :: Cor -> Int
-- green (Cor _ g _) = g

-- blue :: Cor -> Int
-- blue (Cor _ _ b) = b


-- 3.c
data Cor = Cor { red :: Int, green :: Int, blue :: Int } deriving Show

-- 3.d
somaCor :: (Cor, Cor) -> Cor
somaCor (a, b) = Cor (min 255 $ red a + red b) --
                     (min 255 $ green a + green b) --
                     (min 255 $ blue a + blue b)

-- 3.e
(<+>) :: Cor -> Cor -> Cor
(<+>) = curry somaCor

-- 3.f
instance Semigroup Cor where
  (<>) = (<+>)

instance Monoid Cor where
  mappend = (<+>)
  mempty = Cor 0 0 0

-- 3.g
-- Podemos notar que, sendo o nosso conjunto fechado por (<+>), que constitui
-- um operador associativo, já que a soma de cores é, em si, linear e, a adição
-- é associativa. Por fim, havendo o elemento neutro (i.e. a · e = e · a = a)
-- definido por (Cor 0 0 0), temos que o typeclass Cor é um monóide e,
-- portanto, é válido seu uso como instância da classe homônima.

-- 4.a
data Cofre a = Cofre [a] deriving Show

-- 4.b
instance Functor Cofre where
  fmap _ (Cofre []) = Cofre []
  fmap f (Cofre (x:xs)) = Cofre $ f x : fmap f xs

-- 4.c
instance Applicative Cofre where
  -- pure :: a -> Cofre a
  pure xs = Cofre [xs]

  -- (<*>) :: Cofre (a -> b) -> Cofre a -> Cofre b
  (Cofre fs) <*> (Cofre xs) = Cofre [f x | x <- xs, f <- fs]

-- 5.a
data Automovel = Carro
               | Moto deriving Show

-- 5.b
data Veiculo = Veiculo { automovel :: Automovel, placa :: String } deriving Show

-- 5.c
class EhCarro a where
  ehCarro :: a -> Bool

instance EhCarro Automovel where
  ehCarro Carro = True
  ehCarro _ = False

-- 5.d
instance EhCarro Veiculo where
  ehCarro v = ehCarro $ automovel v

-- 5.e
instance EhCarro Int where
  ehCarro _ = False

-- 6.a
data Natural = Nil
             | Succ Natural deriving Show

-- 6.b
somaNatural :: Natural -> Natural -> Natural
somaNatural Nil x = x
somaNatural x Nil = x
somaNatural x (Succ Nil) = Succ x
somaNatural x (Succ y) = somaNatural (Succ x) y

-- 6.c
convertNatural :: Natural -> Int
convertNatural Nil = 0
convertNatural (Succ Nil) = 1
convertNatural (Succ x) = 1 + convertNatural x

-- 7
-- A forma mais genérica de descrever a assinatura dessa função seria algo como
-- compose :: (a -> b) -> (c -> d) -> x -> (r, s)
-- mas de cara notamos que r deve ser do mesmo tipo que b, já que seria uma aplicação de f em última instância, assim como s deve ser do mesmo tipo que d. Isso nos leva à assinatura
-- compose :: (a -> b) -> (c -> d) -> x -> (d, b)
-- porém, usando o mesmo raciocínio, notamos que c = b, já que devemos aplicar a saída de f em g, e idem para d = a. O que nos leva à
-- compose :: (a -> b) -> (b -> a) -> x -> (b, a)
-- porém notamos que x deve ser aplicado nas duas funções, e que, portanto, a = b = x, o que nos dá, por fim, a resposta:
compose :: (a -> a) -> (a -> a) -> a -> (a, a)
compose f g x = (f . g $ x, g . f $ x)

-- 8
data JoKenPo = Pedra
             | Papel
             | Tesoura deriving (Enum, Show, Eq, Ord)

data Resultado = Primeiro
               | Segundo
               | Empate deriving Show

jogar :: JoKenPo -> JoKenPo -> Resultado
jogar Pedra Tesoura = Primeiro
jogar Tesoura Pedra = Segundo
jogar x y | x > y = Primeiro
          | x == y = Empate
          | otherwise = Segundo
