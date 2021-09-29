-- Javier Mulero Martín
-- Sesion de laboratorio 1

-- Ej 1
-- a)
años = 10^6/(60*60*24*365)

-- b)
--En este apartado me he liado un poco con los tiempos, no he entendido bien lo de restantes y eso

-- c)
calculaAños:: Fractional a => a -> a
calculaAños segundos = segundos / (60*60*24*365)

calculaTiempos:: Integral a => a -> (a,a,a,a,a)
calculaTiempos segundos = (a, d - a * 365, h - d *24, m - h * 60, s - m * 60 )
   where a = div segundos (60*60*24*365)
         d = div segundos (60*60*24)
         h = div segundos (60*60)
         m = div segundos 60
         s = segundos
-- Ej 2
media:: Fractional a => [a] -> a
-- media lista = sum[lista]/length[lista] error de tipo, length devuelve Int
media [] = error "Lista vacía"
media lista = sum lista / fromIntegral (length lista)

-- Ej 3
--a)
num_digitos::Integral a => a -> a
num_digitos n
   | (abs n) `div` 10 == 0   = 1
   | otherwise = num_digitos ( (abs n) `div` 10) + 1

--b)
reduccion::Integral a => a -> a
reduccion n
   | x < 10    = x
   | otherwise = reduccion (reduccion (x `div` 10) + x `mod` 10)
 where x = abs n

--c)
comb:: Integral a => a -> a -> a -- esta forma tarda mucho
comb n m
   | m > n = error "m no puede ser mayor que n"
   | m == 0 || m == n   = 1
   |otherwise = comb (n-1) (m-1) + comb (n-1) m

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

comb':: Integral a => a -> a -> a
comb' n m
      | n < 0 || m <= 0   = error "Numeros negativos o 0"
      | m > n = error "m es mayor que n"
      |otherwise = factorial n `div` (factorial m * factorial (n-m))

-- Ej 4

conj_v1::Bool -> Bool -> Bool -- Estricta en los dos argumentos
conj_v1 True True = True
conj_v1 False True = False
conj_v1 True False = False
conj_v1 False False = False

conj_v2::Bool -> Bool -> Bool -- Estricta en primer argumento
conj_v2 False _ = False
conj_v2 _ False = False
conj_v2 True True = True

conj_v3::Bool -> Bool -> Bool -- Estricta en el primer argumento
conj_v3 a b = a && b

conj_v4::Bool -> Bool -> Bool -- Estricta en primer argumento y no segundo
conj_v4 _ False = False
conj_v4 True a = a
conj_v4 a True = a
