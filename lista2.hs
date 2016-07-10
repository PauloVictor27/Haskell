--Inverte lista
inverter [] = []
inverter (x:xs) = (inverter xs)++[x]

--Par ou Impar
par 0 = True
par x = if x `mod` 2 == 0 then True else False

--Primeiros
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x : primeiros (n-1) xs

--Ultimos
ultimos n (x:xs) = inverter(inverter (primeiros n (x:xs)))

--Verifica Palindromo
palindromo string = string == inverter(string)

--Verifica se o elemento existe na lista
pertence p [] = False
pertence p (x:xs) = if (p == x) then True else (pertence p xs)

--Faz intercessao entre duas listas
intercessao [] = []
intercessao (x:xs) ys = if(pertence x ys) then x:intercessao xs ys else intercessao xs ys

--Faz uniao entre duas listas
uniao [] ys = ys
uniao (x:xs) ys =  x:uniao xs ys

--Faz exponenciação de dois numeros
expoente x 0 = 1
expoente x y = x*(expoente x (y-1))  

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
ordena x = (menor x) : (ordena (exclui x))

--Inserção
inserir y [] = [y]
inserir y (x:xs) = if y < x then y:x:xs else if y == x then (x:xs) else x:inserir y xs

--Converter binario para inteiro
--bin "0" = 0
--bin "1" = 1
