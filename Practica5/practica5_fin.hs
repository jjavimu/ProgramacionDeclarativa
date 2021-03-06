-- Sesión 5 de laboratorio
-- 14 de Diciembre de 2020
-- Javier Mulero

-- Ejercicio 1
getInt:: IO Int
getInt = do line <- getLine
            return (read line::Int)

adivina:: Int -> IO ()
adivina n = do
           putStrLn "Escribe un número:"
           m <- getInt
           if m  == n
             then putStrLn "¡Has acertado!"
             else
                if m < n then do
                         putStrLn "Has fallado... Prueba con uno mayor:"
                         adivina n
                else do
                         putStrLn "Has fallado... Prueba con uno menor:"
                         adivina n



-- Ejercicio 2
--Ejemplo:
--hola que tal                  - hola         que          tal
--mi nombre es Javier           - mi     nombre    es    Javier
--estudio programación dinámica - estudio programación dinámica

-- Voy a explicar un poco porque me he hecho un lio pero al final lo he conseguido
-- No sé si de la forma más eficiente, pero la idea es:
-- 1. Leo el fichero entero
-- 2. Lo separo en sus lineas
-- 3. Espacio las lineas
--    Para ello voy añadiendo un espacio " " a cada palabra de la lista (excepto la última)
--    hasta que consigo tener la longitud deseada. La funcion espaciar aux recibe una lista
--    de palabras, la longitud querida y un índice que representa la posicion de la palabra
--    a la que le vamos añadir el espacio. Para añadir el espacio hago zip de la lista con [0..]
--    para tener la posición de cada una y luego miro si se corresponde con la j (pos de la palabra a la que añadir " ")
-- 4. Deshago las lineas y lo escribo en el fichero
-- Al final me he quedado con espaciar' que es mucho más eficiente pero dejo la otra para tenerla guardada

formatea::String -> String -> Int -> IO ()
formatea fileIn fileOut n = do
                            file <- readFile fileIn
                            lineas <- return $ lines file
                            writeFile fileOut (unlines (map (espaciar' n) lineas))



espaciar:: Int -> String -> String
espaciar n linea = if (palabras linea == 1 || palabras linea == 0 || length linea > n) then linea else espaciarAux (words linea) n 0

espaciarAux:: [String] -> Int -> Int -> String
espaciarAux palabrasLinea n j =
  if length nuevaLinea < n then
    if (j == length palabrasLinea -2) then
      espaciarAux nuevaLineaPalabras n 0
      else espaciarAux nuevaLineaPalabras n (j+1)
  else nuevaLinea
    where nuevaLinea = concat nuevaLineaPalabras
          nuevaLineaPalabras = map (\(posx, x) -> if posx == j then (x++" ") else x) $ zip [0..] palabrasLinea

-- Al final he creado otra versión de espaciar usando fold. En este caso lo que hago es más calculos previos
-- para no tener que llamar muchas veces a la función, así que calculo el número de espacios que hay que meter entre
-- cada dos palabras, los coloco con el fold, y el resto (#sobrantes) de espacios para llegar justo al número pedido lo añado
-- con un zipWith concatenando las primeras #sobrantes palabras un espacio " " más, y al resto pues concateno con ""
-- Al final eso con un concat ya tengo todo lo que quería
espaciar':: Int -> String -> String
espaciar' n linea = if (palabras linea == 1 || length linea > n) then linea
  else concat $ zipWith (\p1 p2 -> p1 ++ p2)
  (foldr (\p lp-> if lp == [] then p:lp else (p ++ replicate tam_espacios ' '):lp) [] lista_palabras)
  (replicate sobrantes " " ++ replicate (num_espacios - sobrantes + 1) "")
  where lista_palabras = words linea
        longitud_palabras = map length lista_palabras
        caracteres_ocupados = sum longitud_palabras
        num_espacios = length lista_palabras - 1
        tam_espacios = div (n - caracteres_ocupados) num_espacios
        sobrantes = mod (n - caracteres_ocupados) num_espacios

palabras :: String -> Int
palabras str = (length.words) str

-- Ejercicio 3
type Matriz = [[Float]]

-- a) Operaciones con matrices
transp:: Matriz -> Matriz
transp m = [[fila !! j | fila <- m] | j<-[0..(length (m!!0))-1]]

sumaMat::Matriz -> Matriz -> Matriz
sumaMat m1 m2 = zipWith (\v1 v2 -> zipWith (+) v1 v2) m1 m2

prodMat m1 m2 = [ [sum $ zipWith (*) v1 v2 | v2<-m2t] | v1<-m1]
              where m2t = transp m2


-- b) Dibujar matriz
dibujaMatriz:: Matriz -> IO ()
dibujaMatriz m = do
              dibujaMatrizAux m 0

dibujaMatrizAux:: Matriz -> Int -> IO ()
dibujaMatrizAux m n =
  if n < length m then
    do
    putStrLn $ stringVector (m!!n)
    dibujaMatrizAux m (n+1)
  else return ()

stringVector:: Show a =>[a] -> [Char]
stringVector [] = ""
stringVector (v:vs) =
  if length (v:vs) > 1 then -- Separo casos para que me haga, por ejemplo "1 2 3" y no "1 2 3 "
    (show v) ++ " " ++ stringVector vs
  else (show v)
