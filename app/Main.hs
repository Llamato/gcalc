import System.IO ( IO, hFlush, stdout, getLine)
import GHC.IO.Handle.Internals
import Text.Printf ( printf )
import Text.Read ( readMaybe )
import Data.Text.Lazy.Read
import System.Process
import Prelude hiding (subtract)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.IntMap.Merge.Lazy (merge)
import Data.Text (splitOn, pack, Text)
import Control.Exception (try)
import Data.List
import Data.Matrix


data StackSystem = StackSystem {
    stacks :: M.Map Double [Double],
    current :: Double
}


formatSysId :: Double -> Text
formatSysId sysid =
    if  floor sysid == ceiling sysid then
        head (splitOn (pack ".") (pack (show sysid)))
    else
        pack (show sysid)


getStack :: Double -> StackSystem -> [Double]
getStack stackId sys = fromMaybe [] (M.lookup stackId (stacks sys))


getCurrentStack :: StackSystem -> [Double]
getCurrentStack sys = fromMaybe [] (M.lookup (current sys) (stacks sys))


updateCurrentStack :: StackSystem -> [Double] -> StackSystem
updateCurrentStack sys newStack = sys { stacks = M.insert (current sys) newStack (stacks sys)}


switchStack :: StackSystem -> Double -> StackSystem
switchStack sys stackId =
  let currentContents = fromMaybe [] (M.lookup stackId (stacks sys))
      newStacks = M.insert stackId currentContents (stacks sys)
  in sys { stacks = newStacks, current = stackId }


mergeStacks :: StackSystem -> Double -> StackSystem
mergeStacks sys stackId = updateCurrentStack sys (getStack stackId sys ++ getCurrentStack sys)


stackAdd :: StackSystem -> Double -> StackSystem
stackAdd sys stackId = updateCurrentStack sys (zipWith (+) (getStack stackId sys) (getCurrentStack sys))


stackProduct :: StackSystem -> Double -> StackSystem
stackProduct sys stackId = updateCurrentStack sys (zipWith (*) (getStack stackId sys) (getCurrentStack sys))


stackDivision :: StackSystem -> Double -> StackSystem
stackDivision sys stackId = updateCurrentStack sys (zipWith (/) (getStack stackId sys) (getCurrentStack sys))


stackDotProduct :: StackSystem -> Double -> StackSystem
stackDotProduct sys stackId = updateCurrentStack sys [sum (getCurrentStack (stackProduct sys stackId))]


stackCrossProduct :: StackSystem -> Double -> StackSystem
stackCrossProduct sys stackId = updateCurrentStack sys (drop 3 sourceStack ++ [aVec!!1 * bVec!!2 - aVec!!2 * bVec!!1, aVec!!2 * bVec!!0 - aVec!!0 * bVec!!2, aVec!!0 * bVec!!1 - aVec!!1 * bVec!!0])
    where
        sourceStack = getStack stackId sys
        targetStack = getCurrentStack sys
        aVec = take 3 sourceStack
        bVec = take 3 targetStack


makeMatrixFromStackElements :: Int -> Int -> [Double] -> Matrix Double
makeMatrixFromStackElements columns rows stack = matrix columns rows $ \(r, c) -> stack!!((r-1)*columns+(c-1))


deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (front:rest)
   | i == 0    = rest
   | otherwise = front : deleteN (i-1) rest

removeMatrixRow :: Int -> Matrix a -> Matrix a
removeMatrixRow i mat = fromLists $ reverse $ foldl (\acc c -> if c == i then acc else toList (rowVector (getRow c mat)) : acc) [] [1.. nrows mat]


removeMatrixColumn :: Int -> Matrix a -> Matrix a
removeMatrixColumn j mat = fromLists $ map (deleteN (j-1)) (toLists mat)


matrixDeterminant :: Matrix Double -> Double
matrixDeterminant mat
  | ncols mat /= nrows mat = 0
  | ncols mat == 1 = sum mat
  | ncols mat == 2 = a*d-b*c
  | otherwise = foldl (\acc (i, j) -> if j == 1 then acc + (-1)^(i+j) * (mat ! (i, j)) * matrixDeterminant (removeMatrixRow i (removeMatrixColumn j mat)) else acc) 0 indices
    where
      a = getElem 1 1 mat
      b = getElem 2 1 mat
      c = getElem 1 2 mat
      d = getElem 2 2 mat
      indices = [(i, j) | i <- [1.. nrows mat], j <- [1.. ncols mat]]


putStackLn :: [Double] -> IO ()
putStackLn [] = return ()
putStackLn stack = mapM_ printItem (zip [0..length stack] stack)
    where
        printItem (i, x) = printf "%i: %f\n" i x


add, subtract, multiply, divide, swap, scale, power, sign, roll, duplicate, determinant :: [Double] -> [Double]
add         (x:y:xs) = (y+x) : xs
add         _       = []

subtract    (x:y:xs) = (y-x) : xs
subtract    _       = []

multiply    (x:y:xs) = (y*x) : xs
multiply    _       = []

divide      (x:y:xs) = (y/x) : xs
divide      _       = []

sign        (x:xs) = (x*(-1)) : xs
sign        _       = []

swap        (x:y:xs) = [y,x] ++ xs
swap        _       = []

roll        (x:xs) = xs ++ [x]
roll        _       = []

duplicate   (x:xs) = x : x : xs
duplicate   _       = []

power       (x:y:xs) = x**y : xs
power       _       = []

scale       (x:xs) = map  (x *) xs
scale       _       = []

determinant (x:xs) = matrixDeterminant (makeMatrixFromStackElements (round x) (round x) xs) : drop (round (x**2)) xs
determinant _       = []


processCmd :: [Double] -> String -> IO [Double]
processCmd stack cmd
        | cmd == "+" = return (add stack)
        | cmd == "-" = return (subtract stack)
        | cmd == "*" = return (multiply stack)
        | cmd == "/" = return (divide stack)
        | cmd == "n" = return (sign stack)
        | cmd == "swp" = return (swap stack)
        | cmd == "rol" = return (roll stack)
        | cmd == "dup" = return (duplicate stack)
        | cmd == "pow" = return (power stack)
        | cmd == "product" = return [product stack]
        | cmd == "scale" = return (scale stack)
        | cmd == "det" = return (determinant stack)
        | cmd == "sum" = return [sum stack]
        | cmd == "d" = return (drop 1 stack)
        | cmd == "cls" = return []
        | cmd == "p" = do
            putStackLn (reverse stack)
            return stack
        | otherwise = do
            return stack


processSysCmd :: StackSystem -> String -> IO StackSystem
processSysCmd sys cmd
    | cmd == "switch" = return (switchStack currentSystem stackId)
    | cmd == "merge" = return (mergeStacks currentSystem stackId)
    | cmd == "sadd" = return (stackAdd currentSystem stackId)
    | cmd == "sproduct" = return (stackProduct currentSystem stackId)
    | cmd == "sdiv" = return (stackDivision currentSystem stackId)
    | cmd == "sdot" = return (stackDotProduct currentSystem stackId)
    | cmd == "scross" = return (stackCrossProduct currentSystem stackId)
    | otherwise = do
        newStack <- processCmd currentStack cmd
        return (updateCurrentStack sys newStack)
    where
        currentStack = getCurrentStack sys
        currentSystem = updateCurrentStack sys (drop 1 currentStack)
        stackId = head currentStack


normalizeInput :: String -> String
normalizeInput istr
    | null istr = istr
    | head sstr == '.' = '0' : sstr
    | otherwise = sstr
    where
        sstr = replace ',' '.' istr
        replace old new _ = map (\c -> if c == old then new else c) istr


ipo :: StackSystem -> IO ()
ipo sys = do
        let stack = getCurrentStack sys
        printf "%s:%i:" (formatSysId (current sys)) (length stack)
        hFlush stdout
        istr <- getLine
        case readMaybe (normalizeInput istr) of
            Just num -> do
                ipo (updateCurrentStack sys (num : stack))
            Nothing -> do
                newStackSystem <- processSysCmd sys istr
                ipo newStackSystem


main :: IO ()
main = ipo StackSystem {
            stacks = M.singleton 0 [],
            current = 0
        }
