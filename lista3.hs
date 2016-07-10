--Lista de PaP exercicio 9 em diante

--Par ou Impar
par 0 = True
par x = if x `mod` 2 == 0 then True else False

--Impar
impar 0 = False
impar x = if x `mod` 2 /= 0 then True else False

--filter
filterr _ [] = []
filterr p (x:xs) = if p x then x:filterr p xs else filterr p xs

--Lista Impar
listaimpar =  filterr impar

--Lista de duplas
--fst (x,_) = x
primeirosdupla [] = []
primeirosdupla (x:xs) = fst(x):primeirosdupla xs

--Somatorio com fold
fold1 f a [] = a
fold1 f a (x:xs) = f x (fold1 f a xs)

somatorio' xs = fold1 (+) 0 xs
somatorio'' = fold1 (+) 0

--Cria lista dupla com duas lista
listadupla _ [] = []
listadupla [] _ = []
listadupla (x:xs) (y:ys) = (x,y):listadupla xs ys

--Lista booleana
booleana xs = fold