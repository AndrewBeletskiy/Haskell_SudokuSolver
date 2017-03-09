import Data.List
import System.IO

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

replaceNM n m newVal (x:xs)
    | n == 0 = (replaceNth m newVal x) : xs
    | otherwise = x:replaceNM (n-1) m newVal xs

findPos :: [Char] -> Char -> Int
findPos [] _ = -1
findPos (x:xs) c
    | x == c = 0
    | otherwise = (let ind = findPos xs c 
                   in if (ind >= 0) 
                      then ind + 1
                      else -1)

split :: [Char] -> Char -> [[Char]]
split [] _ = []
split str c = f : split (tail l) c
    where
        pos = findPos str c
        (f,l) = (if (pos == -1)
                then (str, " ")
                else splitAt (findPos str c) str)

main = do
    contents <- readFile "in.txt"
    let numbersText = words contents
    let numbersList = map (\x -> (fromEnum (head x)) - 48) numbersText
    let numbersTable = getTable numbersList n n
    
    let foundedResult = findRes numbersTable (getEmptyIndexes numbersTable)
    let outContents = getContentsString (snd foundedResult)
    putStrLn outContents
    writeFile "out.txt" outContents
    getLine
    where n = 9

getContentsString :: [[[Int]]] -> String
getContentsString results = 
    let
        getContent' f = (foldl (\all elem -> all ++ (foldl (\cur e -> cur ++ (show e) ++ " ") [] elem) ++ "\n") [] f)
    in foldl (\cur elem -> getContent'(elem) ++ "\n\n" ++ cur) [] results
    
getTable :: [a] -> Int -> Int -> [[a]]
getTable [] _ _ = []
getTable lst 1 m = [take m lst]
getTable lst n m = take m lst : getTable (drop m lst)  (n-1) m

getEmptyIndexes :: [[Int]] -> [(Int, Int)]
getEmptyIndexes field = let n = 9 in [(i,j) | i <- [0..n-1], j <- [0..n-1], ((field !! i) !! j) == 0]

findRes :: [[Int]] -> [(Int, Int)] -> (Bool, [[[Int]]])
findRes f emptyIndexes= 
    let 
        canGo = testNonPossibleNumbers f emptyIndexes
        emptyCount = length emptyIndexes
    in if (emptyCount == 0)
        then (True, [f])
        else if (canGo) 
             then let (i,j) = head emptyIndexes
                      nums = findPossibleNumbers f i j
                      results = filter (\(r, f) -> r) (map (\n -> findRes (replaceNM i j n f) (tail emptyIndexes)) nums)
                      len = length results
                  in if (len > 0) then (True, foldl (\cur el -> cur ++ el) [] [x | (_,x) <- results])
                                  else (False, [[[3]]])
             else (False, [f, [[1]]])

isPossible :: [[Int]] -> Bool
isPossible field = 
    and [testRows field, testColumns field, testBlocks field]

countDigits :: [Int] -> [Int]
countDigits lst = [length $ filter (\y -> y == n) lst | n <-[0..9]]

testRows :: [[Int]] -> Bool
testRows lst = and [1 >= maximum (tail (countDigits x)) | x <- lst]

testColumns :: [[Int]] -> Bool
testColumns lst = 
    testRows (transp lst)

testBlocks :: [[Int]] -> Bool
testBlocks lst =
    testRows (getBlockLst lst)

testNonPossibleNumbers :: [[Int]] ->[(Int, Int)]-> Bool
testNonPossibleNumbers f empty= 
    all (>0) [length $ findPossibleNumbers f i j | (i,j) <- empty]

transp :: [[Int]] -> [[Int]]
transp lst = [[lst !! i !! j | i<-[0..n-1]] | j<-[0..m-1]]
    where 
        n = length lst
        m = length (head lst)

getBlockLst :: [[Int]] -> [[Int]]
getBlockLst lst = map getBlockValues [(bI, bJ) | bI <- [0..2], bJ <- [0..2]]
    where getBlockValues (bI,bJ) = [lst !! i !! j| i<-[bI * 3 .. bI * 3 + 2], j<-[bJ * 3 .. bJ * 3 + 2]]

findPossibleNumbers :: [[Int]] -> Int -> Int -> [Int]
findPossibleNumbers field i j = [x | x<-[1..9], not (isElemInRowColumnBlock x i j field)]

isElemInRowColumnBlock :: Int -> Int -> Int -> [[Int]] -> Bool
isElemInRowColumnBlock elem i j field = or [isElemInRow elem i field, isElemInColumn elem j field, isElemInBlock elem i j field]

isElemInRow :: Int -> Int -> [[Int]] -> Bool
isElemInRow e i field = e `elem` field !! i

isElemInColumn :: Int -> Int -> [[Int]] -> Bool
isElemInColumn e j field = e `elem` [x !! j | x <- field]

isElemInBlock :: Int -> Int -> Int -> [[Int]] -> Bool
isElemInBlock e i j field = let 
                              (blockI, blockJ) = findBlock i j
                            in e `elem` [field !! i !! j | i<-[blockI * 3 .. blockI * 3 + 2], j<-[blockJ * 3 .. blockJ * 3 + 2]]

findBlock :: Int -> Int -> (Int, Int)
findBlock i j
  | and [i>=0, i < 3, j >= 0, j < 3] = (0,0)
  | and [i>=0, i < 3, j >= 3, j < 6] = (0,1)
  | and [i>=0, i < 3, j >= 6, j < 9] = (0,2)
  | and [i>=3, i < 6, j >= 0, j < 3] = (1,0)
  | and [i>=3, i < 6, j >= 3, j < 6] = (1,1)
  | and [i>=3, i < 6, j >= 6, j < 9] = (1,2)
  | and [i>=6, i < 9, j >= 0, j < 3] = (2,0)
  | and [i>=6, i < 9, j >= 3, j < 6] = (2,1)
  | and [i>=6, i < 9, j >= 6, j < 9] = (2,2)
  | otherwise = (3,3)

