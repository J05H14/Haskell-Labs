--1
data Student = Student{ firstName :: String
                      , lastName :: String
                      , major :: String
                      , age :: Int}deriving Show
--2
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)

makeNode :: Student  -> Tree Student
makeNode x = Node x EmptyTree EmptyTree

treeInsert :: Student -> Tree Student -> Tree Student
treeInsert x EmptyTree = makeNode x
treeInsert x (Node a left right)
    | getAge x == getAge a = Node x left right
    | getAge x < getAge a = Node a (treeInsert x left) right
    | getAge x > getAge a = Node a left (treeInsert x right)

getAge :: Student -> Int
getAge Student {age = a} = a

--3 csv file

--4

main = do
    path <- getArgs
    contents <- readFile path
    putStrLn "The Path is:"
    mapM putStrLn path
    putStrLn "The List is:"
    putStrLn contents
