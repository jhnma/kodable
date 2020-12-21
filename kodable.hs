import Prelude hiding (Left, Right)
import Split
import Data.List
import Data.Set hiding (map,filter,take,drop,null,foldr)
import Data.Maybe
import Text.Printf
import System.Directory

-- Reference table for conversion between MapElem and String
table :: [(MapElem, String)]
table = [(Ball, "@"), (Path, "-"), (Grass, "*"), (PinkPath, "p"), (OrangePath, "o"), (YellowPath, "y"), (Bonus, "b"), (Target, "t"), (Error, "err")]

-- Data definiton for Map Elements (MapElem)
data MapElem = Ball | Path | Grass | PinkPath | OrangePath | YellowPath | Bonus | Target | Error deriving (Eq,Ord)
instance Show MapElem where
    show = showMapElem

-- Function for showing MapElem
showMapElem :: MapElem -> String
showMapElem x = case elemIndex x (map fst table) of
        Just a  -> snd $ table!!a
        Nothing -> ""

-- Definition for Instructions to Move
data Instruction = Left | Right | Up | Down | Cond MapElem Instruction | Loop Int Instruction Instruction | Function Instruction Instruction Instruction | ErrIns deriving (Eq,Ord)
instance Show Instruction where
    show = showIns

-- Function for showing a single Instruction
showIns :: Instruction -> String
showIns i = case i of
    Main.Left         -> "Left"
    Main.Right        -> "Right"
    Up                -> "Up"
    Down              -> "Down"
    Cond a i          -> "Cond{" ++ showMapElem a ++ "}{" ++ show i ++ "}"
    Loop a i1 i2      -> "Loop{" ++ show a ++ "}{" ++ show i1 ++ "," ++ show i2 ++ "}"
    Function i1 i2 i3 -> "Function with " ++ show i1 ++ " " ++ show i2 ++ " " ++ show i3

-- Function for showing a list of Instructions
showInss :: [Instruction] -> String
showInss [] = ""
showInss (x:xs) = showIns x ++ " " ++ showInss xs

-- Definition of the game board (Gamemap)
newtype Gamemap = MapOf [[MapElem]]
instance Show Gamemap where
    show = showGameMap

-- Function for showing the Gamemap
showGameMap :: Gamemap -> String
showGameMap x = case x of
    MapOf []     -> ""
    MapOf [t]    -> show' t
    MapOf (t:ts) -> show' t ++ "\n" ++ show (MapOf ts)
    where show' [] = ""
          show' [r] = showMapElem r
          show' (r:rs) = showMapElem r ++ " " ++ show' rs

-- Loading the game
load :: String -> IO Gamemap
load "random" = return random
load fname    = do x <- readFile fname
                   putStrLn "Read map successfully!"
                   if tail x == " " then
                       return $ MapOf $ map (map parseMapElem . splitOn " ") (splitOn "\n" x)
                   else return $ MapOf $ map (map parseMapElem . removeTail . splitOn " ") (splitOn "\n" x)
                   where removeTail :: [a] -> [a]
                         removeTail [] = []
                         removeTail [_] = []
                         removeTail xs = init xs

-- Randomly generates a game map
random :: Gamemap
random = undefined
{-
  1. First randomly generate size of grid, then the starting point
  2. Then generate random one-step instructions to move along, until it reaches rightmost column
-}

-- Parses Map Elements from String
parseMapElem :: [Char] -> MapElem
parseMapElem x = case elemIndex x (map snd table) of
    Just a -> fst $ table!!a
    Nothing -> Error

-- Searches the Element in gamemap and returns a list of coordinates
findElem :: MapElem -> Gamemap -> [(Int,Int)]
findElem me (MapOf gm) = findElem0 me (MapOf gm) 0 where
    findElem0 :: MapElem -> Gamemap -> Int -> [(Int,Int)]
    findElem0 me (MapOf [])     x = []
    findElem0 me (MapOf (r:rs)) x = case elemIndices me r of
        []  -> findElem0 me (MapOf rs) (x+1)
        m   -> map (\y -> (x,y)) m ++ findElem0 me (MapOf rs) (x+1)

-- Verifies if the gamemap has a solution
hasSolution :: Gamemap -> Bool
hasSolution gm = case findPath gm of
    [] -> False
    _ -> True

type PointState = ((Int,Int), Bool, Set MapElem, Set (Int,Int), [Instruction])
-- Type for checking solution of the problem, in the form of (Coord, Started, VisitedColors, VisitedCheckpts, Instructions)

funcOptimizer :: [Instruction] -> [Instruction]
funcOptimizer inss = case inss of
    []     -> []
    [x]    -> [x]
    (x:xs) ->
        if length xs >= 2 && ([x] ++ [head xs] ++ [xs!!1]) == inFunc
            then Function x (head xs) (xs!!1) : funcOptimizer (drop 2 xs)
        else x : funcOptimizer xs
    where inFunc = mostCommon (all3Substrings inss)
          all3Substrings :: [a] -> [[a]]
          all3Substrings [] = []
          all3Substrings (x:xs) =
              if length xs >= 2 then
                  ([x] ++ [head xs] ++ [xs!!1]) : all3Substrings xs
              else []
          mostCommon :: Ord a => [a] -> a
          mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

-- Finds the optimal path of the map
findPath :: Gamemap -> [Instruction]
findPath (MapOf gm) = finder (toPtState $ findElem Ball (MapOf gm)) (toPtState $ findElem Ball (MapOf gm)) (head $ findElem Target (MapOf gm)) gm where
    toPtState :: [(Int,Int)] -> [PointState]
    toPtState []     = []
    toPtState (x:xs) = (x,False,empty,empty,[]) : toPtState xs

    getCoord :: PointState -> (Int,Int)
    getCoord (x,_,_,_,_) = x

    getStarted :: PointState -> Bool
    getStarted (_,x,_,_,_) = x

    getVisitedColors :: PointState -> Set MapElem
    getVisitedColors (_,_,x,_,_) = x

    getVisitedCheckpts :: PointState -> Set (Int,Int)
    getVisitedCheckpts (_,_,_,x,_) = x

    getInstructions :: PointState -> [Instruction]
    getInstructions (_,_,_,_,x) = x

    finder :: [PointState] -> [PointState] -> (Int,Int) -> [[MapElem]] -> [Instruction]
    finder points visited end gamemap
        | (end, False, fromList $ findElem Bonus (MapOf gm)) `elem` map (\x -> (getCoord x, getStarted x, getVisitedCheckpts x)) points =
            if not $ null (elemIndices (end,False,fromList $ findElem Bonus (MapOf gm)) (map (\x -> (getCoord x, getStarted x, getVisitedCheckpts x)) points)) then
                getInstructions $ points !! head (elemIndices (end,False,fromList $ findElem Bonus (MapOf gm)) (map (\x -> (getCoord x, getStarted x, getVisitedCheckpts x)) points))
            else []
        | null points = []
        | otherwise   = finder (genNewPts points) (visited ++ genNewPts points) end gamemap where

        genNewPts :: [PointState] -> [PointState]
        genNewPts []     = []
        genNewPts (x:xs) = feasiblePaths x ++ genNewPts xs

        feasiblePaths :: PointState -> [PointState]
        feasiblePaths x = filter validMove $ next x gamemap
        -- feasiblePaths x = next x gamemap

        validMove :: PointState -> Bool
        -- validMove pt = validPos (MapOf gm) (getCoord pt)
        validMove pt =
            if null (getInstructions pt) then True
            else (getCoord pt, getStarted pt, getVisitedCheckpts pt, last $ getInstructions pt) `notElem` map (\x -> (getCoord x, getStarted x, getVisitedCheckpts x, tryLast $ getInstructions x)) visited
        -- && not (getStarted pt)

        tryLast :: [Instruction] -> Instruction
        tryLast [] = ErrIns
        tryLast [x] = x
        tryLast (x:xs) = tryLast xs

    next :: PointState -> [[MapElem]] -> [PointState]
    next state map =
        if getStarted state then
            if nextCoord == coord then nextStates ++ [(coord, False, empty, getVisitedCheckpts state, getInstructions state)]
            else nextStates ++ [(nextCoord, True, checkColor coord map (getVisitedColors state), checkBonus coord map (getVisitedCheckpts state), getInstructions state)] -- add checking bonus
        else nextStatesStopped

        where coord = getCoord state
              currInstruction = last $ getInstructions state
              nextCoord = runIns (MapOf gm) coord 0 currInstruction
              baseInstructions = [Left, Right, Up, Down]
              getMapElem map (x,y) = if validPos (MapOf map) (x,y) then map!!x!!y else Error
              feasibleIns = filter (\x -> runIns (MapOf gm) coord 0 x /= coord) baseInstructions

              nextStates :: [PointState]
              nextStates =
                  if isCond coord map && ((map `getMapElem` coord) `notElem` getVisitedColors state) then
                      Prelude.map (\x -> (runIns (MapOf gm) coord 0 x, True, empty, getVisitedCheckpts state, getInstructions state ++ [Cond (map `getMapElem` coord) x])) feasibleIns
                  else []
                          
              nextStatesStopped :: [PointState]
              nextStatesStopped = Prelude.map (\x -> (runIns (MapOf map) coord 0 x, True, empty, getVisitedCheckpts state, getInstructions state ++ [x])) feasibleIns

              checkColor :: (Int,Int) -> [[MapElem]] -> Set MapElem -> Set MapElem
              checkColor c map colors = case map `getMapElem` c of
                  YellowPath -> Data.Set.insert YellowPath colors
                  OrangePath -> Data.Set.insert OrangePath colors
                  PinkPath   -> Data.Set.insert PinkPath colors
                  _          -> colors
              
              checkBonus :: (Int,Int) -> [[MapElem]] -> Set (Int,Int) -> Set (Int,Int)
              checkBonus c map visitedCP = if map `getMapElem` c == Bonus then Data.Set.insert c visitedCP else visitedCP

              isCond :: (Int,Int) -> [[MapElem]] -> Bool
              isCond coord map = case map `getMapElem` coord of
                  PinkPath -> True
                  YellowPath -> True
                  OrangePath -> True
                  _ -> False

-- Playing with no predefined function
play' :: IO [Instruction]
play' = do putStr "  >> First direction: "
           inp <- getLine
           if inp == "" then
               do putStrLn "     Cannot have no directions at all! Please try again."
                  play'
           else do i <- parseIns inp
                   if i == ErrIns then
                        do putStrLn "     Invalid instruction! This instruction will not be counted. Try again!"
                           play'
                   else do ins <- playSnd [i]
                           putStrLn $ showInss ins
                           return ins
    where
        playSnd :: [Instruction] -> IO [Instruction]
        playSnd ins = do putStr "  >> Next  direction: "
                         inp <- getLine
                         if inp == "" then return ins    -- eval the input, start executing the commands (have to take care of condition interrupts), determine if the target is reached
                         else do i <- parseIns inp
                                 if i == ErrIns then
                                     do putStrLn "     Invalid instruction! This instruction will not be counted. Try again!"
                                        playSnd ins
                                 else playSnd (ins ++ [i])
        
        -- Improvement: Change to error checking the format of input for Cond, Loop and Function as well
        parseIns :: String -> IO Instruction
        parseIns s
            | s == "Left" = return Main.Left
            | s == "Right" = return Main.Right
            | s == "Up" = return Up
            | s == "Down" = return Down
            | substring 0 4 s == "Cond" = do i <- parseIns (substring 8 (length s - 1) s)
                                             return $ Cond (parseMapElem [s!!5]) i
            | substring 0 4 s == "Loop" = do i <- parseIns (substring 8 (find' ',' s) s)
                                             j <- parseIns (substring (find' ',' s + 1) (length s - 1) s)
                                             return $ Loop (read [s!!5]) i j
            | substring 0 8 s == "Function" = return ErrIns
            | otherwise = return ErrIns

            where find' :: Char -> [Char] -> Int
                  find' s l = case elemIndex s l of
                      Just x  -> x
                      Nothing -> -1
                  substring :: Int -> Int -> String -> String
                  substring i j s = drop i (take j s)

-- Playing with predefined function
play :: Instruction -> Instruction -> Instruction -> IO [Instruction]
play a b c = do putStr "  >> First direction: "
                inp <- getLine
                if inp == "" then
                    do putStrLn "     Cannot have no directions at all! Please try again.\n"
                       play a b c
                else do i <- parseIns inp a b c
                        if i == ErrIns then
                            do putStrLn "     Invalid instruction! This instruction will not be counted. Try again!\n"
                               play a b c
                        else do ins <- playSnd a b c [i]
                                putStrLn $ showInss ins
                                return ins
    where
        playSnd :: Instruction -> Instruction -> Instruction -> [Instruction] -> IO [Instruction]
        playSnd a b c ins = do putStr "  >> Next  direction: "
                               inp <- getLine
                               if inp == "" then return ins    -- eval the input, start executing the commands (have to take care of condition interrupts), determine if the target is reached
                               else do i <- parseIns inp a b c
                                       if i == ErrIns then
                                           do putStrLn "     Invalid instruction! This instruction will not be counted. Try again!\n"
                                              playSnd a b c ins
                                       else playSnd a b c (ins ++ [i])

        -- Improvement: Change to error checking the format of input for Cond, Loop and Function as well
        parseIns :: String -> Instruction -> Instruction -> Instruction -> IO Instruction
        parseIns s a b c
            | s == "Left" = return Main.Left
            | s == "Right" = return Main.Right
            | s == "Up" = return Up
            | s == "Down" = return Down
            | substring 0 4 s == "Cond" = do i <- parseIns (substring 8 (length s - 1) s) a b c
                                             return $ Cond (parseMapElem [s!!5]) i
            | substring 0 4 s == "Loop" = do i <- parseIns (substring 8 (find' ',' s) s) a b c
                                             j <- parseIns (substring (find' ',' s + 1) (length s - 1) s) a b c
                                             return $ Loop (read [s!!5]) i j
            | substring 0 8 s == "Function" = return $ Function a b c
            | otherwise = return ErrIns

            where find' :: Char -> [Char] -> Int
                  find' s l = case elemIndex s l of
                      Just x  -> x
                      Nothing -> -1
                  substring :: Int -> Int -> String -> String
                  substring i j s = drop i (take j s)

-- Runs a single instruction based on the coordinates of the ball and the map. Advance if the instruction is valid and returns the same coordinates otherwise.
runIns :: Gamemap -> (Int,Int) -> Int -> Instruction -> (Int,Int)
runIns gm         (x,y) s Left                = if validPos gm (x,y-1) then (x,y-1) else (x,y)
runIns gm         (x,y) s Right               = if validPos gm (x,y+1) then (x,y+1) else (x,y)
runIns gm         (x,y) s Up                  = if validPos gm (x-1,y) then (x-1,y) else (x,y)
runIns gm         (x,y) s Down                = if validPos gm (x+1,y) then (x+1,y) else (x,y)
runIns (MapOf me) (x,y) s (Cond a i)          = if me!!x!!y == a then runIns (MapOf me) (x,y) s i else (x,y)
runIns gm         (x,y) s (Loop a i1 i2)      = if a > 0 then runIns gm (fst (runInss [i1,i2] gm (x,y) s)) s (Loop (a-1) i1 i2) else (x,y)
runIns gm         (x,y) s (Function i1 i2 i3) = fst (runInss [i1,i2,i3] gm (x,y) s)

-- Determines if the given position of the map is valid
validPos :: Gamemap -> (Int, Int) -> Bool
validPos (MapOf m) (x,y)  = (x >= 0 && x < length m) && (y >= 0 && y < length (head m)) && (m `at` (x,y) /= Grass)

-- Accesses the gamemap given the coordinates
at :: [[a]] -> (Int,Int) -> a
m `at` (a,b) = m!!a!!b

-- Runs a list of instructions on a gamemap, given an initial starting point, returns the resulting coordinates and the number of bonuses left to take.
-- Todo: Given map and set of instructions, move the ball acc. to instructions and print every move of the ball
runInss :: [Instruction] -> Gamemap -> (Int, Int) -> Int -> ((Int, Int),Int)
runInss []     (MapOf gm) (x,y) s = ((x,y),s)
runInss (t:ts) (MapOf gm) (x,y) s = 
    if tmp == (x,y) then
        runInss ts (MapOf gm) tmp s
    else case gm `at` tmp of
        Path -> runInss (t:ts) (MapOf gm) tmp s
        Ball -> runInss (t:ts) (MapOf gm) tmp s
        Target -> runInss (t:ts) (MapOf gm) tmp s
        Bonus -> runInss (t:ts) (MapOf gm) tmp (s-1)
        Grass -> runInss ts (MapOf gm) (x,y) s
        _ -> case head ts of
            Cond me i -> if me == gm `at` tmp then runInss (i:tail ts) (MapOf gm) tmp s else runInss (t:ts) (MapOf gm) tmp s
            _ -> runInss (t:ts)  (MapOf gm) tmp s
    where tmp = runIns (MapOf gm) (x,y) s t

runInssIO :: [Instruction] -> Gamemap -> (Int, Int) -> Int -> Int -> IO ((Int, Int),Int)
runInssIO []     (MapOf gm) (x,y) s c = return ((x,y),s)
runInssIO (t:ts) (MapOf gm) (x,y) s c =
    do putStrLn (mutated gm 0 ++ if gm `at` tmp == Bonus then "\nOne bonus point collected!" else "")
       if tmp == (x,y) then
           if c > 0 then runInssIO ts (MapOf gm) tmp s 0
           else putStrLn (printf "Cannot go %s! The game will terminate. Try again!\n" (showIns t)) >> return ((-1,-1),-1)
       else case gm `at` tmp of
          Path -> runInssIO (t:ts) (MapOf gm) tmp s (c+1)
          Ball -> runInssIO (t:ts) (MapOf gm) tmp s (c+1)
          Target -> runInssIO (t:ts) (MapOf gm) tmp s (c+1)
          Bonus -> runInssIO (t:ts) (MapOf gm) tmp (s-1) (c+1)
          Grass -> runInssIO ts (MapOf gm) (x,y) s 0
          _ -> case head ts of
              Cond me i -> if me == gm `at` tmp then runInssIO (i:tail ts) (MapOf gm) tmp s 0 else runInssIO (t:ts) (MapOf gm) tmp s c
              _ -> runInssIO (t:ts) (MapOf gm) tmp s (c+1)
    where tmp = runIns (MapOf gm) (x,y) s t
          mutated :: [[MapElem]] -> Int -> String
          mutated me s = case me of
              []     -> ""
              (t:ts) -> if s == fst tmp then showSpecial t 0 ++ "\n" ++ mutated ts (s+1) else show' t ++ "\n" ++ mutated ts (s+1)
              where show' [] = ""
                    show' [r] = showMapElem' r
                    show' (r:rs) = showMapElem' r ++ " " ++ show' rs
                    showSpecial r y =
                        case r of []  -> ""
                                  [e] -> if y == snd tmp then "@" else showMapElem' e
                                  (e:es) -> (if y == snd tmp then "@" else showMapElem' e) ++ " " ++ showSpecial es (y+1)

          showMapElem' :: MapElem -> String
          showMapElem' x = if x == Ball then "-" else
              case elemIndex x (map fst table) of
                Just a  -> snd $ table!!a
                Nothing -> ""

kodable :: IO ()
kodable = do
    -- check feasibility of map
    -- display result of feasibility, if infeasible, terminate
    command <- getCmd
    case head command of
        "load" -> do b <- hasFile (command!!1)
                     if not b then putStrLn "No such file! Try again." >> kodable
                     else do
                         (MapOf gm) <- load (command!!1)
                         if hasSolution (MapOf gm) then do putStrLn "Map has a solution!\nInitial:"
                                                           print (MapOf gm)
                                                           if not (noError gm) then putStrLn "Warning: The loaded file has error characters." >> kodableLoaded (MapOf gm)
                                                           else kodableLoaded (MapOf gm)
                         else putStrLn "Map is unsolvable! Please load another file." >> kodable
        _      -> do putStrLn "Cannot run other commands before loading the map. Try again!"
                     kodable
    where kodableLoaded :: Gamemap -> IO ()
          kodableLoaded (MapOf gm) = do
              command <- getCmd
              case head command of
                  "load" -> do b <- hasFile (command!!1)
                               if not b then putStrLn "No such file! Try again." >> kodable
                               else do
                                   gm <- load (command!!1)
                                   if hasSolution gm then putStrLn "Map has a solution!\nInitial:" >> print gm >> kodableLoaded gm
                                   else putStrLn "Map is unsolvable! Please load another file." >> kodable
                  "play" ->
                      if length command == 1 then
                          do ins <- play'
                             result <- runInssIO ins (MapOf gm) (head $ findElem Ball (MapOf gm)) 3 0
                             if fst result == (-1,-1) then kodableLoaded(MapOf gm)
                             else 
                                if fst result == head (findElem Target (MapOf gm)) then
                                    if snd result == 0 then
                                        putStrLn "Congratulations! You reached the target with all 3 bonuses passed."
                                    else do
                                        putStrLn $ printf "You reached the target, but you have only taken %d bonuses. Try again!" (3-snd result)
                                        kodableLoaded (MapOf gm)
                                else
                                    putStrLn "Your instructions didn't reach the target. Try again!"

                      else
                          do ins <- play (toIns $ command!!1) (toIns $ command!!2) (toIns $ command!!3)
                             result <- runInssIO ins (MapOf gm) (head $ findElem Ball (MapOf gm)) 3 0
                             if fst result == (-1,-1) then kodableLoaded(MapOf gm)
                             else 
                                if fst result == head (findElem Target (MapOf gm)) then
                                    if snd result == 0 then
                                        putStrLn "Congratulations! You reached the target with all 3 bonuses passed."
                                    else do
                                        putStrLn $ printf "You reached the target, but you have only taken %d bonuses. Try again!" (3-snd result)
                                        kodableLoaded (MapOf gm)
                                else
                                    putStrLn "Your instructions didn't reach the target. Try again!"
                  "hint" -> do putStrLn $ printf "The first instruction is %s." (showIns $ head sol)
                               putStrLn $ printf "The last instruction is %s." (showIns $ last sol)
                               putStrLn $ printf "The minimal number of instructions needed is %d.\n" (length sol)
                               kodableLoaded (MapOf gm)
                               where sol = findPath (MapOf gm)
                  "soln" -> do print $ findPath (MapOf gm)
 
          toIns :: String -> Instruction
          toIns "Left"  = Left
          toIns "Right" = Right
          toIns "Up"    = Up
          toIns "Down"  = Down

          hasFile :: String -> IO Bool
          hasFile fname = do pwd <- getCurrentDirectory
                             doesFileExist $ pwd ++ "\\" ++ fname

          getCmd :: IO [String]
          getCmd = do
              putStr "> "
              command <- getLine
              let cmdList = words command
              if null cmdList then getCmd
              else case head cmdList of
                  "load" -> if length cmdList == 2 then return ["load", tail $ init (cmdList!!1)] else putStrLn "Invalid input! Try again." >> getCmd
                  "play" -> if (length cmdList == 1) || (length cmdList == 4 && isDirection (cmdList!!1) && isDirection (cmdList!!2) && isDirection (cmdList!!3)) then return cmdList
                          else putStrLn "Invalid input! Try again." >> getCmd
                  "hint" -> return ["hint"]
                  "soln" -> return ["soln"]
                  _ -> putStrLn "Invalid input! Try again." >> getCmd
              where isDirection :: String -> Bool
                    isDirection s = (s == "Left") || (s == "Right") || (s == "Up") || (s == "Down")
          
          noError :: [[MapElem]] -> Bool
          noError gamemap = null $ findElem Error (MapOf gamemap)