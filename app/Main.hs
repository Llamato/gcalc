import System.IO ( hFlush, stdout)
import Text.Printf ( printf )
import Text.Read ( readMaybe )
import Prelude hiding (subtract, error)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, takeWhile, length, splitOn)
import Data.Matrix
import System.Environment (getArgs)
import Control.Monad (forM_)

data StackSystem = StackSystem {
    stacks :: M.Map Double [Double],
    current :: Double
}

data CalcError =
    NotEnoughElements String Int Int
  | DivisionByZero
  | NegativeSqrt
  | ZeroRoot
  | EmptyStack
  | UnknownCommand String
  deriving (Show)

formatSysId :: Double -> Text
formatSysId sysid = Data.Text.takeWhile (/= '.') (pack $ show sysid)


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
removeMatrixRow i mat = fromLists . reverse $ foldl (\acc c -> if c == i then acc else toList (rowVector (getRow c mat)) : acc) [] [1.. nrows mat]


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

decimalPlaces :: Double -> Int
decimalPlaces x = Data.Text.length . last $ splitOn (pack ".") (pack $ show x)

hcf :: Double -> Double -> Double
hcf a b = let 
    scaleFactor = 10 ^ (max (decimalPlaces a) (decimalPlaces b))
    scaledA = round (a * scaleFactor)
    scaledB = round (b * scaleFactor)
    in fromIntegral ((Prelude.gcd scaledA scaledB) :: Integer) / scaleFactor

putStackLn :: [Double] -> IO ()
putStackLn [] = return ()
putStackLn stack = mapM_ printItem (zip [0.. Prelude.length stack] stack)
    where
        printItem (i, x) = printf "%i: %f\n" i x


add, subtract, multiply, divide, sign, swap, roll, scale, power, oroot, sroot, gcd, duplicate, determinant :: [Double] -> Either CalcError [Double]
add         (x:y:xs) = Right ((y+x) : xs)
add         xs       = Right xs

subtract    (x:y:xs) = Right ((y-x) : xs)
subtract    xs       = Right xs

multiply    (x:y:xs) = Right ((y*x) : xs)
multiply    xs       = Right xs

divide      (x:y:xs) = Right ((y/x) : xs)
divide      (0:_)    = Left DivisionByZero
divide      xs       = Right xs

sign        (x:xs) = Right ((x*(-1)) : xs)
sign        xs       = Right xs

swap        (x:y:xs) = Right ([y,x] ++ xs)
swap        xs       = Right xs

roll        (x:xs) = Right (xs ++ [x])
roll        xs       = Right xs

duplicate   (x:xs) = Right (x : x : xs)
duplicate   xs       = Right xs

power       (x:y:xs) = Right (x**y : xs)
power       xs       = Right xs

oroot       (x:y:xs) = Right (y**(1/x) : xs)
oroot       xs       = Right xs

sroot       (x:xs)   = Right (sqrt x : xs)
sroot       xs       = Right xs

gcd         (x:y:xs) = Right (hcf x y : xs)
gcd         (xs)     = Right xs

scale       (x:xs)   = Right (map  (x *) xs)
scale       xs       = Right xs

determinant (x:xs)   = Right (matrixDeterminant (makeMatrixFromStackElements (round x) (round x) xs) : drop (round (x**2)) xs)
determinant xs       = Right xs


processCmd :: [Double] -> String -> IO [Double]
processCmd stack cmd
    | cmd == "p" = do
        putStackLn (reverse stack)
        return stack
    | otherwise =
        case runCmd stack cmd of
            Left error -> do
                putStrLn $ "Error: " ++ show error
                return stack
            Right newStack -> return newStack


runCmd :: [Double] -> String -> Either CalcError [Double]
runCmd stack cmd = case cmd of
        "+" -> add stack
        "-" -> subtract stack
        "*" -> multiply stack
        "/" -> divide stack
        "n" -> sign stack
        "swp" -> swap stack
        "rol" -> roll stack
        "dup" -> duplicate stack
        "pow" -> power stack
        "sqrt" -> sroot stack
        "gcd" -> Main.gcd stack
        "rt" -> oroot stack
        "product" -> Right [product stack]
        "scale" -> scale stack
        "det" -> determinant stack
        "sum" -> Right [sum stack]
        "d" -> Right (drop 1 stack)
        "cls" -> return []
        "" -> Right stack
        unknown -> Left (UnknownCommand unknown)



processSysCmd :: StackSystem -> String -> IO (Either CalcError StackSystem)
processSysCmd sys cmd = let currentStack = getCurrentStack sys in
            if not (null currentStack) then
                let
                    currentSystem = updateCurrentStack sys (drop 1 currentStack)
                    stackId = head currentStack
                in
                case cmd of
                    "switch" -> return $ Right (switchStack currentSystem stackId)
                    "merge" -> return $ Right (mergeStacks currentSystem stackId)
                    "sadd" -> return $ Right (stackAdd currentSystem stackId)
                    "sproduct" -> return $ Right (stackProduct currentSystem stackId)
                    "sdiv" -> return $ Right (stackDivision currentSystem stackId)
                    "sdot" -> return $ Right (stackDotProduct currentSystem stackId)
                    "scross" -> return $ Right (stackCrossProduct currentSystem stackId)
                    _ -> do
                        newStack <- processCmd currentStack cmd
                        return $ Right (updateCurrentStack sys newStack)
            else
                return $ Left (NotEnoughElements cmd 2 1)


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
        printf "%s:%i:" (formatSysId (current sys)) (Prelude.length stack)
        hFlush stdout
        istr <- getLine
        case readMaybe (normalizeInput istr) of
            Just num -> do
                ipo (updateCurrentStack sys (num : stack))
            Nothing -> do
                newStackSystem <- processSysCmd sys istr
                case newStackSystem of
                    Left error -> if null istr then ipo sys
                        else do
                            putStrLn $ "Error: " ++ show error
                            ipo sys
                    Right newsys -> ipo newsys 


parseCommandLineArgument :: String -> IO ()
parseCommandLineArgument arg
    | arg == "--help" = do
        putStrLn "help"
    | otherwise = do
        putStr "Unknown argument "
        putStrLn arg

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg] -> parseCommandLineArgument arg
        [] -> ipo StackSystem { stacks = M.singleton 0 [], current = 0 }
        _ -> forM_ args parseCommandLineArgument
