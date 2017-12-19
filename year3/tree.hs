module Tree where

data BST a = Node a (BST a) (BST a) | Empty deriving (Show)

find :: Ord a => BST a -> a -> Bool
find Empty x = False
find (Node v l r) x
  | x < v = find l x
  | x > v = find r x
  | otherwise = True

insert :: Ord a => BST a -> a -> BST a
insert Empty x = Node x Empty Empty
insert (Node v l r) x
  | x < v     = Node v (insert l x) r
  | x > v     = Node v l (insert r x)
  | otherwise = error "Tried to insert duplicate"

delete :: Ord a => BST a -> a -> BST a
delete Empty x = Empty
delete (Node v l r) x
  | x == v = subsumeTree r l
  | x < v  = Node v (delete l x) r
  | x > v  = Node v l (delete r x)
  where
    subsumeTree Empty sub = sub
    subsumeTree (Node v l r) sub = Node v (subsumeTree l sub) r

validate :: Ord a => BST a -> Bool
validate Empty = True
validate (Node v l r) = validate' v (<) l && validate' v (>) r
  where
    validate' _ _ Empty             = True
    validate' v cmp t@(Node v' _ _) = cmp v' v && validate t
