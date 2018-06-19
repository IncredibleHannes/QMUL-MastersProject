module Selection
    (
     J,
     argsup,
     arginf,
     bigotimes,
     otimes
    ) where

type J r x = (x -> r) -> x

argsup, arginf :: [x] -> J Int x
argsup    []  p = undefined
argsup (x:xs) p = f xs x (p x)
  where f    xs  a   1  = a
        f    []  a   r  = a
        f (x:xs) a (-1) = f xs x (p x)
        f    xs  a   0  = g xs
         where g (x:xs) = if p x == 1 then x else g xs
               g    []  = a

arginf xs p = argsup xs (\x -> - p x)

otimes :: J r x -> (x -> J r [x]) -> J r [x]
otimes e0 e1 p = a0 : a1
  where a0 = e0(\x0 -> overline (e1 x0) (\x1 -> p(x0:x1)))
        a1 = e1 a0 (\x1 -> p(a0 : x1))
        overline e p = p(e p)

bigotimes :: [[x] -> J r x] -> J r [x]
bigotimes [] = \p -> []
bigotimes (e : es) =
 e [] `otimes` (\x -> bigotimes[\xs->d(x:xs) | d <- es])
