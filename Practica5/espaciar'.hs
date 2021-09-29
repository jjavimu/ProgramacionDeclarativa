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
