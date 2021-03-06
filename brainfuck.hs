import Data.Char
import Data.List
import Data.Array.IArray
import qualified Test.QuickCheck as QC
import System.IO


zeroMemory = 0 : zeroMemory

data State = State {
    memory :: [Int],
    dataPtr :: Int,
    codePtr :: Int,
    codes :: String,  
    output :: String,
    loopStartCount :: Int,
    loopEndCount :: Int
}

instance Show State where
    show (State memory dataPtr codePtr codes output lsc lec) = 
        "State ( memory [ " ++( show (take 30 memory) ) ++ ", " ++ 
            "dataPtr: " ++ (show dataPtr) ++ ", " ++
            "codePtr: " ++ (show codePtr) ++ ", " ++
            "currentCode: " ++ (if ( (length codes) <= codePtr) then "!EOF!" else (show (codes!!codePtr))) ++ ", " ++
            "currentMemory: " ++ (show (memory!!dataPtr)) ++ ", " ++
            "loopStartCount: " ++ (show lsc) ++ ", " ++
            "loopEndCount: " ++ (show lec) ++ ", " ++
            "output: " ++ (show output) ++ ", " ++
        " ] ) " 

newState codes = State zeroMemory 0 0 codes [] 0 0
helloWorld = newState "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
sierpinski = newState "[-]>++++[<++++++++>-]>++++++++[>++++<-]>>++>>>+>>>+<<<<<<<<<<[-[->+<]>[-<+>>>.<<]>>>[[->++++++++[>++++<-]>.<<[->+<]+>[->++++++++++<<+>]>.[-]>]]+<<<[-[->+<]+>[-<+>>>-[->+<]++>[-<->]<<<]<<<<]++++++++++.+++.[-]<]+++++"
twentySix = newState "+++++[>+++++<-]>+"
hello = newState "++++++++[>++++++++<-]>++++++++.---.+++++++..+++."
nested = newState "+[[[++++>+[-]]]]"

parserTest  :: Bool
parserTest = outputMustBe helloWorld "Hello World!\n" &&
    outputMustBe hello "HELLO" &&
    (memory (run twentySix))!!1 == 26


outputMustBe :: State -> String -> Bool
outputMustBe state desiredOutput = ((output (run state))) == desiredOutput


stepMemory :: State -> Int -> State
stepMemory state stepSize = step (state{ dataPtr = ((dataPtr state) + stepSize) })

stepMemoryRight state = stepMemory state 1
stepMemoryLeft state = stepMemory state (-1)

incMemory state = step (state { memory = modify (memory state) (dataPtr state) (+1)  })
decMemory state = step (state { memory = modify (memory state) (dataPtr state) (+(-1))  })

printToOutput state = step ( state { output = ( (output state) ++ ([chr ( (memory state)!!( dataPtr state)) ] ))} )

jumpTo :: Int -> State -> State
jumpTo nextCodePtr state = state { codePtr = nextCodePtr } 

step :: State -> State
step state = state{codePtr = ( (codePtr state) + 1)}

-- if the byte at the data pointer is zero, jump it forward to the command after the matching ]
jumpRightIfNeeded :: State -> State
jumpRightIfNeeded state = if (isZero (dataPtr state) (memory state)) then 
        jumpTo (findLoopEnd (codes state) (codePtr state)) state
    else
        step state

-- if the byte at the data pointer is nonzero, jump it back to the command after the matching [ command
jumpLeftIfNeeded :: State -> State
jumpLeftIfNeeded state = if (not (isZero (dataPtr state) (memory state))) then 
        jumpTo (findLoopStart (codes state) (codePtr state)) state
    else
        step (loopEnded state)

currentAction :: State -> (State -> State)
currentAction (State memory dataPtr codePtr codes output loopStartCount loopEndCount) = case codes!!codePtr of 
        '>' -> stepMemoryRight
        '<' -> stepMemoryLeft
        '+' -> incMemory
        '-'  -> decMemory
        '.' -> printToOutput
        '[' -> \x -> loopStarted (jumpRightIfNeeded x)
        ']' -> jumpLeftIfNeeded
        ' ' -> step


loopStarted :: State -> State
loopStarted state = state{loopStartCount = (loopStartCount state)+1}

loopEnded :: State -> State
loopEnded state = state{loopEndCount = (loopEndCount state)+1}


stepState :: State -> State
stepState state = (currentAction state state)

run :: State -> State
run state =  if (not (finished state)) then
        run (stepState state)
    else
        state



finished :: State -> Bool
finished state = (codePtr state) >= length (codes state)
       
isZero :: Int -> [Int]  -> Bool 
isZero index list = list!!index == 0


setMemValue :: [Int] -> Int -> Int -> [Int]
setMemValue memory dataPtr value = replace dataPtr ( mod value 255 ) memory

modify :: [Int] -> Int -> (Int -> Int) -> [Int] 
modify memory dataPtr apply = 
    replace dataPtr (mod (apply (memory!!dataPtr)) 255 ) memory

replace :: Int -> Int -> [Int] ->  [Int]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

--findBackwardsStart :: [Int] -> Int -> Int
--findBackwardsStart :: memory currentPos = 1

findLoopStart :: [Char] -> Int -> Int
findLoopStart codes current =
    countUntilLoopsClosed codes (current - 1) 0 0 (-1)

findLoopEnd :: [Char] -> Int -> Int
findLoopEnd codes current = 
    countUntilLoopsClosed codes (current + 1) 0 0 (1)

loopMarker direction = if (direction < 0) then '[' else ']'

-- count bakcwards or forward while for current loop start/end point
countUntilLoopsClosed :: [Char] -> Int -> Int -> Int -> Int -> Int
countUntilLoopsClosed codes current opened closed direction = if (codes!!current == (loopMarker direction) && (opened - closed) == 0) then
        current
    else (
        case codes!!current of 
            ']' -> countUntilLoopsClosed codes (current + direction) (opened) (closed + 1) direction
            '[' -> countUntilLoopsClosed codes (current + direction) (opened + 1) (closed) direction
            otherwise -> countUntilLoopsClosed codes (current + direction) (opened) (closed) direction
        )

testCountBackwards :: Bool
testCountBackwards =
    ((countUntilLoopsClosed "[]" 0 0 0 (-1) ) == 0) &&
    ((countUntilLoopsClosed "[a]" 1 0 0 (-1) ) == 0) &&
    ((countUntilLoopsClosed ">>[abc]<<" 5 0 0 (-1)) == 2) &&
    ((countUntilLoopsClosed "abc[a[bcd]e[f]g]9" 14 0 0 (-1)) == 3)


testLoopStartFind :: Bool 
testLoopStartFind =
    (findLoopStart "a[b[c[d[e]f]g]h]i" 15) == 1 &&
    (findLoopStart "[[]]a[[]]" 8) == 5 && 
    (findLoopStart "[]" 1) == 0 &&
    (findLoopStart "[[]]" 3) == 0 && 
    True

testLoopEndFind :: Bool
testLoopEndFind = 
    (findLoopEnd "a[b[c[d[e]f]g]h]i" 1) == 15 && True

writeResultToFile state fileName = do
    outh <- openFile fileName WriteMode
    let res = run state
    hPutStrLn outh (output res)
