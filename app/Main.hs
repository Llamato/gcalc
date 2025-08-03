import System.IO ( IO, hFlush, stdout, getLine)
import GHC.IO.Handle.Internals
import Text.Printf ( printf )
import Text.Read ( readMaybe )
import Data.Text.Lazy.Read
import System.Process
import Prelude hiding (subtract)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.IntMap.Merge.Lazy (merge)
import Data.Text (splitOn, pack, Text)
import Control.Exception (try)
import Data.List
import Data.Maybe (listToMaybe)

data StackSystem = StackSystem {
    stacks :: M.Map Double [Double],
    current :: Double
}


formatSysId :: Double -> Text
formatSysId id =
    if floor id == ceiling id then
        head (splitOn (pack ".") (pack (show id)))
    else
        pack (show id)


push :: [Double] -> Double -> [Double]
push stack value = value : stack


pop :: [Double] -> Double
pop (value: stack) = value


getStack :: Double -> StackSystem -> [Double]
getStack id sys = fromMaybe [] (M.lookup id (stacks sys))


getCurrentStack :: StackSystem -> [Double]
getCurrentStack sys = fromMaybe [] (M.lookup (current sys) (stacks sys))


updateCurrentStack :: StackSystem -> [Double] -> StackSystem
updateCurrentStack sys newStack = sys { stacks = M.insert (current sys) newStack (stacks sys)}


switchStack :: StackSystem -> Double -> StackSystem
switchStack sys id =
  let currentContents = fromMaybe [] (M.lookup id (stacks sys))
      newStacks = M.insert id currentContents (stacks sys)
  in sys { stacks = newStacks, current = id }


mergeStacks :: StackSystem -> Double -> StackSystem
mergeStacks sys id = updateCurrentStack sys (getStack id sys ++ getCurrentStack sys)


stackAdd :: StackSystem -> Double -> StackSystem
stackAdd sys id = updateCurrentStack sys (zipWith (+) (getStack id sys) (getCurrentStack sys))


stackProduct :: StackSystem -> Double -> StackSystem
stackProduct sys id = updateCurrentStack sys (zipWith (*) (getStack id sys) (getCurrentStack sys))


stackDivision :: StackSystem -> Double -> StackSystem
stackDivision sys id = updateCurrentStack sys (zipWith (/) (getStack id sys) (getCurrentStack sys))


stackDotProduct :: StackSystem -> Double -> StackSystem
stackDotProduct sys id = updateCurrentStack sys [sum (getCurrentStack (stackProduct sys id))]


stackCrossProduct :: StackSystem -> Double -> StackSystem
stackCrossProduct sys id = updateCurrentStack sys (drop 3 sourceStack ++ [aVec!!1 * bVec!!2 - aVec!!2 * bVec!!1, aVec!!2 * bVec!!0 - aVec!!0 * bVec!!2, aVec!!0 * bVec!!1 - aVec!!1 * bVec!!0])
    where
        sourceStack = getStack id sys
        targetStack = getCurrentStack sys
        aVec = take 3 sourceStack
        bVec = take 3 targetStack


putStackLn :: [Double] -> IO ()
putStackLn [] = return ()
putStackLn stack = mapM_ printItem (zip [0..length stack] stack)
    where
        printItem (i, x) = printf "%i: %f\n" i x


add, subtract, multiply, divide, swap, scale, power, sign, roll, duplicate :: [Double] -> [Double]
add         (x:y:xs) = (y+x) : xs
subtract    (x:y:xs) = (y-x) : xs
multiply    (x:y:xs) = (y*x) : xs
divide      (x:y:xs) = (y/x) : xs
sign        (x:xs) = (x*(-1)) : xs
swap        (x:y:xs) = [y,x] ++ xs
roll        (x:xs) = xs ++ [x]
duplicate   (x:xs) = x : x : xs
scale       (x:xs) = map  (x *) xs
power       (x:y:xs) = x**y : xs


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
        | cmd == "scale" = return (scale stack)
        | cmd == "product" = return [product stack]
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
    | head str == '.' = '0' : str
    | otherwise = str
    where
        str = replace ',' '.' istr
        replace old new str = map (\c -> if c == old then new else c) istr


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
