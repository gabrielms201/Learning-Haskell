------
-- 1. 
-- Escreva em Haskell o programa membro, que recebe um elemento e uma lista e verifica se o
-- elemento é membro da lista.
-- 
-- membro 5 [ 3 , 2 , 4 , 5 , 1 , 9 ]
-- > True
-- 
-- membro 6 [ 3 , 2 , 4 , 5 , 1 , 9 ]
-- > False

membro :: Eq t => t -> [t] -> Bool
membro _ [] = False
membro e (a:x)
    | e == a = True
    | otherwise = membro e x

------


membro l1 l2 = l1 `union` l2
