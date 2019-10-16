import Data.Matrix

maximo_subpalindromo :: [Char] -> Int
maximo_subpalindromo string = foldr max 0 resultados
    where
        longitud = (length string) - 1

        substrings = [(x,y) | x <- [0..longitud], y <- [x..longitud]]
        
        compare x y = if string!!x == string!!y then 1 else -1

        pali :: Matrix Integer -> Int -> Int -> Matrix Integer
        pali dp x y = if dp!(x+1, y+1) /= 0
            then dp
            else setElem value (x+1, y+1) dpRecursion
                where
                    dpRecursion = palindromo dp (x+1) (y-1)
                    value = if dpRecursion!(x+2, y) == 1 
                        then compare x y
                        else -1

        palindromo :: Matrix Integer -> Int -> Int -> Matrix Integer
        palindromo dp x y = if dp!(x+1, y+1) /= 0
            then dp
            else if x == y
                then setElem 1 (x+1, y+1) dp
                else if x == (y-1)
                    then setElem (compare x y) (x+1, y+1) dp
                    else pali dp x y

        calcularResultados :: Matrix Integer -> [(Int, Int)] -> [Int]
        calcularResultados dp [] = []
        calcularResultados dp (x:xs) = resultado : (calcularResultados dpRecursion xs)
            where
                (a, b) = x
                dpRecursion = palindromo dp a b
                resultado = if dpRecursion ! (a+1, b+1) == 1
                    then (b-a+1)
                    else 0
        
        dp = zero (longitud + 1) (longitud + 1)
        resultados = calcularResultados dp substrings