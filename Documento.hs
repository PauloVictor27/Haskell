import Data.List
import Data.Char

linhas [] = []
linhas x = lines x

nlinhas [] = []
nlinhas x = zip [1..] (linhas x)

allwords [] = []
allwords ((x,y):xs) = (zip (repeat x) (words y))++(allwords(xs))
--allwords ((x,y):xs) = map (\w->(x,w))(words y)++allwords xs

documentacao [] = []
documentacao x = allwords(nlinhas(x))

sortls xs = sortBy cmpWord xs
 where cmpWord (_,a) (_,b) = compare a b 

almalgamate [] = []
almalgamate xxs@(x:xs) = (map fst (takeWhile eqw xxs), snd x):almalgamate(dropWhile eqw xs)
 where eqw (_,a) = map toUpper a == map toUpper (snd x)
 
makeindex txt = almalgamate(sortls(documentacao(txt)))

