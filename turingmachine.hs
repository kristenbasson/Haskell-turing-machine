-- Kristen Basson BSSKRI003
-- Part C

data State = S0 | S1 | S2 | S3 | S4 | SH deriving (Show, Eq)

-- All possible state transitions
stateTransition :: State -> Char -> (Char, Char, State)
stateTransition S0 '1' = ('1', 'R', S0)
stateTransition S0 '-' = ('-', 'R', S1)
stateTransition S1 '#' = ('#', 'R', S1)
stateTransition S1 '$' = ('$', 'H', SH)
stateTransition S1 '1' = ('#', 'L', S2)
stateTransition S2 '-' = ('-', 'L', S3)
stateTransition S2 '#' = ('#', 'L', S2)
stateTransition S3 '#' = ('#', 'L', S3)
stateTransition S3 '1' = ('#', 'R', S4)
stateTransition S4 '#' = ('#', 'R', S4)
stateTransition S4 '-' = ('-', 'R', S1)

-- Used to keep track of the Turing machine
type TuringConfig =
  ( State, -- State
    [Char], -- Tape
    Int -- Head of the Turing machine (current position on the tape)
  )

-- helper function to replace the headPos with the # in the move function
replace :: [a] -> Int -> a -> [a]
replace list index newValue =
  let (firstPart, _:restOfSecondPart) = splitAt index list
  in firstPart ++ [newValue] ++ restOfSecondPart

move :: TuringConfig -> TuringConfig
move (state, tape, headPos) =
  (stateNew,
   replace tape headPos outputSymbol,
   headPos + moveDir)
  where
    (outputSymbol, direction, stateNew) = stateTransition state (tape !! headPos)
    moveDir = if direction == 'R' then 1 else -1  -- move right or left

-- Execute the Turing machine
execute :: TuringConfig -> IO (String, State)
execute (state, tape, headPos) = do
  putStrLn tape  -- printing the tape each time before moving right or left
  if state == SH
    then do
      putStrLn "Halt" 
      return (tape, state)
    else execute (move (state, tape, headPos))

-- Calculate the answer from the execute function that returns the (tape, state)
calculateAnswer :: [Char] -> Int
calculateAnswer tape = length (takeWhile (== '1') tape)

-- 
subtract' :: [Char] -> IO String
subtract' ns = do
  (finalTape, _) <- execute (S0, ns, 0)
  return finalTape

-- Takes input numbers a and b from the user
input :: IO [Char]
input = do
  putStrLn "Enter the first number (a): "
  a <- readLn  -- read the first number a from the user
  putStrLn "Enter the second number (b): "
  b <- readLn  -- read the second number b from the user

  if a <= b
    then do
      putStrLn "First number must be greater than second. Please try again."
      input  -- recursively call input to re-enter values if entered value is incorrect
    else
      return (convert a b)

-- Changes the input numbers from the user to 1's
convert :: Int -> Int -> [Char]
convert a b = replicate a '1' ++ "-" ++ replicate b '1' ++ "$"

main :: IO ()
main = do
  inputString <- input  -- get the input from the user
  finalTape <- subtract' inputString -- call function subtract'
  let answer = calculateAnswer finalTape
  putStrLn $ "Answer: " ++ show answer
