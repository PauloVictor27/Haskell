maior a b = if a > b then a else b
--maiorigual5 = maior 5

inclista [] = []
inclista (x:xs) = 1+x:inclista xs

dobralista [] = []
dobralista (x:xs) = 2*x:dobralista xs

--fst(x,_) = x
primeirosdupla [] = []
primeirosdupla (x:xs) = fst(x):primeirosdupla xs

map' _ [] = []
map' f (x:xs) = f x:map' f xs

--inclista xs = map' (+1) xs
--inclista = map' (+1)

somatorio [] = 0
somatorio (x:xs) = x+somatorio xs

prod [] = 1
prod (x:xs) = x*prod xs

fold f a [] = a
fold f a (x:xs) = f x (fold f a xs)

somatorio' xs = fold (+) 0 xs
somatorio'' = fold (+) 0

--foldl _ a [] = a
--foldl f a (x:xs) = foldl f (f a x) xs

remova e [] = []
remova e (x:xs) = if x == e then remova e xs else x:remova e xs

maiores _ [] = []
maiores e (x:xs) = if x > e then x:maiores e xs else maiores e xs

menores _ [] = []
menores e (x:xs) = if x < e then x:menores e xs else menores e xs

filter' _ [] = []
filter' p (x:xs) = if p x then x:filter' p xs else filter' p xs

maiores2 e xs = filter'(>e) xs
remova2 e xs = filter'(e/=) xs

--quicksort (x:xs) = quicksort(filter'(<x) xs)++[x]++quicksort(filter'(>=x) xs)
quicksort [] = []
quicksort (x:xs) = quicksort esq++[x]++quicksort dir 
 where esq = filter' (<x) xs 
       dir = filter' (>=x) xs
