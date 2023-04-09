intere [] ls = []

intere (x:xs) ls
    | intera x ls = x : intere xs ls
    | otherwise = intere xs ls

intera e [] = False
intera e (a:x)
    | e == a = True
    | otherwise = intera e x