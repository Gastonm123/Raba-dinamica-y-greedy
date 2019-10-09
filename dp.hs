
maximo_subpalindromo :: [Char]-> Int
maximo_subpalindromo string = foldr max 0 largos
    where
        longitud = (length string) - 1

        substrings = [(y,x) | y <- [0..longitud], x <- [(y+1)..longitud]]
        
        pali x y = if palindromo (x+1) (y-1)
            then (string!!x == string!!y)
            else False
        
        palindromo x y = if x == y
            then True
            else if x == (y-1)
                then (string!!x == string!!y)
                else pali x y
        
        largoSubstring (x, y) = if palindromo x y then (y-x+1) else 0
        largos = map cantidad substrings


-- array = "adadada"

-- largo = (length array) - 1 
-- substrings = [(y,x) | y <- [0..largo], x <- [(y+1)..largo]]

-- cantidad (x, y) = if palindromo x y then (y-x) else 0
-- cantidades = map cantidad substrings

-- aplanado = [x | y <- cantidades, x <- y]

-- pali x y = if palindromo (x+1) (y-1)
--     then (array!!x == array!!y)
--     else False

-- palindromo x y = if x == y
--     then True
--     else if x == (y-1)
--         then (array!!x == array!!y)
--         else pali x y