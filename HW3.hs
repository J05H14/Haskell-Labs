--1
data Student = Student{ firstName :: String
                      , lastName :: String
                      , major :: String
                      , age :: Int}deriving Show
--2
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

makeNode :: Student  -> Tree Student
makeNode x = Node x EmptyTree EmptyTree

treeInsert :: Student -> Tree Student -> Tree Student
treeInsert x EmptyTree = makeNode x
treeInsert x (Node a left right)
    | x { age } == a { age } = Node x left right
    | x { age } < a { age } = Node a (treeInsert x left) right
    | x { age } > a { age } = Node a left (treeInsert x right)
