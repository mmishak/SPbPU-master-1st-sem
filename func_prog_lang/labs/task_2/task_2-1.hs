data BinaryTree = EmptyTree | Leaf Integer | Node Integer BinaryTree BinaryTree deriving (Show)

insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree x = Leaf x
insert (Leaf value) x | x < value = Node value (Leaf x) EmptyTree
                      | x > value = Node value EmptyTree (Leaf x)
                      | otherwise = Leaf value                   
insert (Node value left right) x | x < value = Node value (insert left x) right
                                 | x > value = Node value left (insert right x)
                                 | otherwise = Node value left right

remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree    _ = EmptyTree
remove (Leaf value) x = if value == x then EmptyTree 
                                      else Leaf value
remove (Node value left right) x | x < value = Node value (remove left x) right
                                 | x > value = Node value left (remove right x)
                                 | otherwise = concatTree left right
                                 where concatTree EmptyTree    tree = tree
                                       concatTree (Leaf v)     tree = Node v EmptyTree tree
                                       concatTree (Node v l r) tree = Node value l (concatTree r tree)

emptyTree :: BinaryTree
emptyTree = EmptyTree

containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree               _ = False
containsElement (Leaf value)            x = x == value
containsElement (Node value left right) x | x < value = containsElement left x
                                          | x > value = containsElement right x
                                          | otherwise = True

nearestGE :: BinaryTree -> Integer -> Integer
nearestGE EmptyTree x = undefined
nearestGE (Leaf value) x | x == value = value
                         | otherwise = undefined
nearestGE (Node value left@(EmptyTree) right) x | x <= value = value
                                                | otherwise = nearestGE right x
nearestGE (Node value left@(Leaf leftValue) right) x | x == value = value
                                                     | x < value = if x > leftValue then value
                                                                                    else nearestGE left x
                                                     | otherwise = nearestGE right x
nearestGE (Node value left@(Node leftValue leftLeft leftRight) right) x | x == value = value
                                                                        | x < value = if x > leftValue then value
                                                                                                       else nearestGE left x
                                                                        | otherwise = nearestGE right x


treeFromList :: [Integer] -> BinaryTree
treeFromList list = foldl insert EmptyTree list

listFromTree :: BinaryTree -> [Integer]
listFromTree EmptyTree               = []
listFromTree (Leaf value)            = [value]
listFromTree (Node value left right) = [value] ++ (listFromTree left) ++ (listFromTree right)