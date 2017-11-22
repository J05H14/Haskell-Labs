import System.Environment
import System.Directory
import System.IO
import Data.List.Split

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

--a
main = do
    args <- getArgs
    contents <- readFile (head args)
    
    let list = splitOn "\n" contents
        searchType = head $ tail args
        searchVal = head $ tail $ tail args
        stuList = parseCsv list
        stuTree = makeStudentTree stuList
        answer = search (searchType) (searchVal) (stuTree)
    print answer

parseCsv :: [String] -> [Student]
parseCsv [] = []
parseCsv ["'"] = []
parseCsv (x:xs) = 
    let studentData = splitOn "," x
        student = makeStudent studentData
    in [student] ++ parseCsv xs

makeStudent :: [String] -> Student
makeStudent [fn,ln,maj,age] = Student fn ln maj (read age)

--b
makeStudentTree :: [Student] -> Tree Student
makeStudentTree [x] = treeInsert x EmptyTree
makeStudentTree (x:xs) = treeInsert x (makeStudentTree xs)

--c
treeElem :: Int -> Tree Student -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == getAge a = True
    | x < getAge a = treeElem x left
    | x > getAge a = treeElem x right

searchByAge :: Int -> Tree Student -> Bool
searchByAge age tree = treeElem age tree

--d
depthFirst :: Tree Student -> [Student]
depthFirst EmptyTree = []
depthFirst (Node a left right) = a : (depthFirst left) ++ (depthFirst right)

searchByName :: String -> Tree Student -> Bool
searchByName name tree =
    let list = depthFirst tree
    in findName name list

findName :: String -> [Student] -> Bool
findName name [] = False
findName name (x:xs)
    | getLastName x == name = True
    | otherwise = findName name xs

getLastName :: Student -> String
getLastName Student {lastName = l} = l

-- use c or d
search :: [Char] -> String -> Tree Student -> Bool
search "" "" EmptyTree = False
search sType sVal tree
    | sType == "search-age" = searchByAge value tree
    | sType == "search-name" = searchByName sVal tree
    where value = read sVal