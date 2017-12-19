data Tree = EmptyNode | Node Int Tree Tree deriving (Eq, Show)

inOrder :: Tree -> [Int]
inOrder EmptyNode = []
inOrder (Node v l r) =
    inOrder l ++ [v] ++ inOrder r

insert :: Tree -> Int -> Tree
insert EmptyNode i = Node i EmptyNode EmptyNode
insert node@(Node v l r) i
  | i == v    = node
  | i <  v    = Node v (insert l i) r
  | otherwise = Node v l (insert r i)

fromList :: [Int] -> Tree
fromList [] = EmptyNode
fromList (x : xs) = insert (fromList xs) x

-- insertTree :: Tree -> Tree -> Tree
-- insertTree EmptyNode from = from
-- insertTree to EmptyNode = to
-- insertTree to@(Node vt l r) from@(Node vf _ _)
--   | vt == vf = error "Duplicate!"
--   | vt <  vf = Node vt l (insertTree r from)
--   | vt >  vf = Node vt (insertTree l from) r
-- 
-- delete :: Tree -> Int -> Tree
-- delete EmptyNode i = EmptyNode
-- delete (Node v l r) i
--   | i == v = insertTree l r
--   | i <  v = Node v (delete l v) r
--   | i >  v = Node v l (delete r v)

find :: Tree -> Int -> Bool
find EmptyNode _ = False
find (Node v l r) i
  | i == v    = True
  | i <  v    = find l i
  | otherwise = find r i

main :: IO ()
main = undefined

-- quickCheck (\v -> (insert EmptyNode v) == (Node v EmptyNode EmptyNode))
