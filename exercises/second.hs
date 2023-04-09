-- Ricardo Gabriel Marques dos Santos Ruiz


{-- 
1 Escreva em haskell o onde, que recebe um elemento e uma lista e devolve a posição da primeira ocorrência deste elemento na lista.
Exemplo de entrada do "onde".
1 onde 2 [ 7 , 1 , 2 , 1 , 5 , 3 ]
Saida 3
 --}


onde elemento lista = onde' elemento lista 1
onde' _ [] _ = -1
onde' elemento (a:x) pos
  | elemento == a = pos
  | otherwise = onde' elemento x (pos + 1)

{--
Escreva o ateh, que recebe um elemento elt e uma lista l e 
devolve a lista dos primeiros elementos de l até o elemento elt, inclusive
--}

--ateh elt l = [x | x <- l, x <= elt]
ateh elt l = takeWhile (<= elt) l

-- Escreva o apos, que recebe um elemento elt e uma lista l e devolve a lista dos elementos de l
-- que estão após o elemento elt
apos elt l = drop 1 (dropWhile (/= elt) l)

-- Escreva o gera_n, que recebe um inteiro n e gera uma lista com a sequência dos naturais de
-- 1 até n.
gera_n 0 = []
gera_n n = gera_n (n-1) ++ [n]

-- Escreva o gera_m_mult, que recebe dois inteiros n e m e gera uma lista com a sequência dos
-- naturais de 1 até m, múltiplos de n
gera_m_mult n m = [x | x <- [1..m], mod x n == 0]

-- Escreva em Haskell o programa split, que recebe uma lista e divide esta lista em duas listas
-- de tamanho igual ou aproximadamente igual (obs: apesar de ser mais fácil, você não precisa
-- seguir exatamente o comportamento dos meus exemplos abaixo, ou seja, pode dividir a lista
-- de outro modo).
split xs = (take l xs, drop l xs)
    where l = length xs `div` 2

-- 
-- Sem usar como programa auxiliar o "tamanho da lista", escreva em Haskell o programa mtam,
-- que recebe duas listas e responde se elas tem o mesmo tamanho.
mtam :: [a] -> [b] -> Bool
mtam [] [] = True
mtam [] _ = False
mtam _ [] = False
mtam (_:x) (_:y) = mtam x y


-- Escreva em Haskell o programa tri, que recebe uma lista e triplica seus elementos (seguindo
-- os exemplos em Listing ):
tri :: [x] -> [x]
tri [] = []
tri (x:xs) = [x, x, x] ++ tri xs

-- Escreva em Haskell o programa subs, que recebe dois elementos (a e b) e uma lista lst, todos
-- do mesmo tipo, e substitui todas as ocorrências de a por b em ls
subs a b [] = []
subs a b (x:xs) = if a == x then b : subs a b xs else x : subs a b xs

-- Escreva a expressão lambda para o XOR (ou exclusivo).
-- lambda x, y: (x and not y) or (not x and y)