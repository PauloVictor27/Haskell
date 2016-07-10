data Semana = Dom|Seg|Ter|Qua|Qui|Sex|Sab
data Arvore a = No a (Arvore a) (Arvore a) | Folha

class Map f where
 map' :: (a -> b) -> f a -> f b

class Len x where
 len :: x -> Int

------------------------------------------------------------------

instance Len [b] where
 len [] = 0
 len (x:xs) = 1 + len xs

instance Len (Arvore b) where
 len Folha = 0
 len (No _ esq dir) = 1 + len esq + len dir

instance Map [] where
 map' = map

{-
instance Map Arvore where
 map' Folha = Folha
 map' f (No x e d) = No f x (map' f e) (map' f d)
-}

inc xs = map' (+1) xs

instance Show Semana where
 show Dom = "Domingo"
 show Seg = "Segunda"
 show Ter = "Terca"
 show Qua = "Quarta"
 show Qui = "Quinta"
 show Sex = "Sexta"
 show Sab = "Sabado"

{-
instance (Show a) = Show (Arvore a) where
 show Folha = "Folha"
 show (No x esq dir) = "("++show esq++")"++show x++"("++show dir++")"
-}
