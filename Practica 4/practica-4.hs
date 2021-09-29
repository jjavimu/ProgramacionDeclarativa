-- Sesión 4 de laboratorio
-- 30 de Noviembre de 2020
-- Javier Mulero

-- Ejercicio 1

data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Eq, Ord, Show)
type Punto = (Int, Int)

mover:: Punto -> Direccion -> Punto
mover (x,y) Arriba = (x,y+1)
mover (x,y) Abajo = (x,y-1)
mover (x,y) Izquierda = (x-1,y)
mover (x,y) Derecha = (x+1,y)

destino:: Punto -> [Direccion] -> Punto
destino p [] = p
--destino p (x:xs) = destino (mover p x) xs -- recursivo
destino origen acciones = foldr (\x p -> mover p x) origen acciones


-- Ejercicio 2

data Nat = Cero | Suc Nat deriving (Eq, Ord)

instance Num (Nat) where
  (+) Cero n = n
  (+) (Suc m) n = Suc (n + m)

  (*) Cero n = Cero
  (*) (Suc m) n = n + (m*n)

natToInt::Nat -> Int
natToInt Cero = 0
natToInt (Suc n) = 1 + natToInt n

instance Show (Nat) where
  show n = show (natToInt n)

-- Ejercicio 3

data Complejos a = C a a deriving (Eq)

instance Num a => Num (Complejos a) where
  (+) (C a b) (C c d) = (C (a+c) (b+d))
  (-) (C a b) (C c d) = (C (a-c) (b-d))
  (*) (C a b) (C c d) = (C (a*c - b*d) (a*d + b*c))

instance (Num a, Ord a, Show a) => Show (Complejos a) where
  show (C a 0) = show a
  show (C 0 b) = show b ++ "i"
  show (C a b) = if b>0 then show a ++ "+" ++ show b ++ "i" else show a ++ "-" ++ show (-b) ++ "i"

-- Ejercicio 4

class Medible a where
  medida::a->Int

instance Medible Bool where
  medida True = 1
  medida False = 0

instance Medible a => Medible [a] where
  medida xs = foldr (\e acc -> (medida e) + acc) 0 xs

instance (Medible a, Medible b) => Medible (a,b) where
  medida (a,b) = medida a + medida b

instance Medible Int where
  medida n = abs n
