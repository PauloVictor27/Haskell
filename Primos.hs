fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

listaFibonacci 0 = [0]
listaFibonacci n = listaFibonacci (n-1) ++ [fibonacci n]

listaFibonacci2 n = take n (listaFibonacci n)

divisor n [] = False
divisor n (x:xs) = if (mod n x) == 0 then True else divisor n xs

primo 1 = True
primo n = not (divisor n [2..(n-1)])

nPrimos n = zip [1..n] (filter primo [1..])

removePrimeiro [] = []
removePrimeiro ((x,y):xs) = y:removePrimeiro xs

nPrimos2 n = removePrimeiro( nPrimos n)
