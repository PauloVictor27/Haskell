--Inverte lista
inverter [] = []
inverter (x:xs) = (inverter xs)++[x]

--Verifica Palindromo
palindromo string = string == inverter(string)

--Verifica se o elemento existe na lista
pertence p [] = False
pertence p (x:xs) = if (p == x) then True else (pertence p xs)

--Faz intercessao entre duas listas
intercessao [] = []
intercessao (x:xs) ys = if(pertence x ys) then x:intercessao xs ys else intercessao xs ys 

--Verificar se a repitidos
repitido [] = False
repitido (x:xs) = if(pertence x xs) then True else repitido xs

--Retorna o menor
menor [x] = x
menor (x:xs) = if x < menor xs then x else menor xs

--Exclui elemento
exclui [x] = []
exclui (x:xs) = if x == (menor (x:xs)) then xs else x:exclui xs

--Ordene os elementos usando o menor
ordena [] = []
ordena [x] = [x]
ordena l = (menor l) : (ordena (exclui l))
