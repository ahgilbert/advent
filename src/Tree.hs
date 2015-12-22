module Tree where

data Tree a = T (a, [Tree a])

insert :: Tree a -> a -> Tree a
insert (T (k, [])) v = T (k, [(T (v,[]))])

main = print $ compare 'a' 'b'
