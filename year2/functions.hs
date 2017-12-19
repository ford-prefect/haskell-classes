map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map f xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs) = if f x then x : filterRest else filterRest where filterRest = filter' f xs

foldl' :: (r -> a -> r) -> r -> [a] -> r
foldl' _ i []       = i
foldl' f i (x : xs) = foldl' f (f i x) xs

foldr' :: (a -> r -> r) -> r -> [a] -> r
foldr' _ i [] = i
foldr' f i (x : xs) = f x (foldr' f i xs)

run :: Integral a => [a] -> a
run = foldl' (+) 0 . map (\a -> a * a) . filter even

main :: IO ()
main = undefined
