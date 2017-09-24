import Data.List

type State = Int

data Cell = Zero | One deriving (Show, Eq)

data Move = L | R deriving (Show, Eq)

data Tape = Tape [Cell] Cell [Cell] deriving Show

data Transition = Transition {
   input  :: (Cell, State)
  ,twrite :: Cell
  ,tmove  :: Move
  ,tstate :: State
                             } deriving Show

data Machine = Machine {
   state       :: State
  ,tape        :: Tape
  ,transitions :: [Transition]
  ,step        :: Int
  ,mid         :: Int
                       } deriving Show

findTransition :: (Cell, State) -> [Transition] -> Transition
findTransition (c, s) (x:xs)
  | (c, s) == input x = x
  | otherwise         = findTransition (c, s) xs

runStep :: Machine -> Machine
runStep m = if (state m) == 0 then m else nm
  where nm  = Machine ns nt trs nn (mid m)
        trs = transitions m
        ns  = tstate tr
        nt  = shiftTape (writeTape (tape m) (twrite tr)) (tmove tr)
        nn  = (step m) + 1
        tr  = findTransition (currentCell (tape m), state m) (transitions m)

runNSteps :: Int -> Machine -> Machine
runNSteps n m | n > 0     = runNSteps (n-1) (runStep m)
              | otherwise = m

currentCell :: Tape -> Cell
currentCell (Tape _ c _) = c

writeTape :: Tape -> Cell -> Tape
writeTape (Tape l _ r) c = Tape l c r

shiftTape :: Tape -> Move -> Tape
shiftTape (Tape l c r) m = case m of
  L -> Tape l' n r'
    where l' = if null l then [] else init l
          r' = if null r then [c] else c:r
          n  = if null l then Zero else last l
  R -> Tape l' n r'
    where l' = if null l then [c] else l++[c]
          r' = if null r then [] else tail r
          n  = if null r then Zero else head r

makeTransition :: (Cell, State, Cell, Move, State) -> Transition
makeTransition (a, b, c, d, e) = Transition (a,b) c d e

cells = [Zero, One]
moves = [L, R]
initialTape = Tape [] Zero []

nStateTransitions :: State -> [[Transition]]
nStateTransitions n = seq
  -- start from State 1 on [1..n] to exclude starting halt state (State 0)
  where combs = pure (,,,,) <*> cells <*> [1..n] <*> cells <*> moves <*> [0..n]
        trans = [makeTransition i | i <- combs]
        group = groupBy (\t1 t2 -> input t1 == input t2) trans
        seq   = sequence group

longestCellType :: Tape -> Cell -> Int
longestCellType (Tape l c r) cell = length . filter (\cc -> cc == cell) $ l++c:r

buildMachine :: (Int, (State, [Transition])) -> Machine
buildMachine (index, (st, trs)) = Machine st initialTape trs 0 index

generateNStateMachines :: State -> [Machine]
generateNStateMachines n = machines
  where stateNTrans = pure (,) <*> [1..n] <*> (nStateTransitions n)
        indexedST   = zip [0..] stateNTrans
        machines    = [buildMachine a | a <- indexedST]

runMachinesNSteps :: [Machine] -> Int -> [Machine]
runMachinesNSteps ms n = [runNSteps n m | m <- ms]

onlyHalted :: [Machine] -> [Machine]
onlyHalted ms = filter (\m -> (state m == 0)) ms

findLongestOnes :: [Machine] -> Machine
findLongestOnes ms = foldr cond (head ms) (tail ms)
  where cond = (\m1 m2 -> if (longestCellType (tape m1) One) > (longestCellType (tape m2) One) then m1 else m2)

-- find the first halting two state machine that produces the longest string on Ones on the tape after ten steps
m = findLongestOnes $ onlyHalted $ runMachinesNSteps (generateNStateMachines 2) 10

main:: IO ()
-- prints out 4
main = print $ longestCellType (tape m) One
