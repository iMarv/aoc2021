testinput :: [Command]
testinput = [(F, 5), (D, 5), (F, 8), (U, 3), (D, 8), (F, 2)]

data Action = F | U | D deriving (Enum, Eq)

type Command = (Action, Int)

commandToInt :: Command -> Int
commandToInt (F, num) = num
commandToInt (U, num) = num * (-1)
commandToInt (D, num) = num

sumDepth :: [Command] -> Int
sumDepth cmds = sum nums
  where
    cmds' = filter (\a -> fst a /= F) cmds
    nums = map commandToInt cmds'

sumForwards :: [Command] -> Int
sumForwards cmds = sum fwds
  where
    fwds = map snd (filter (\a -> fst a == F) cmds)

solveFor1 :: [Command] -> Int
solveFor1 cmds = sumForwards cmds * sumDepth cmds

aim :: [Command] -> Int -> Int -> Int -> Int
aim [] _ p d = p * d
aim ((U, num) : xs) aim' depth pos = aim xs (aim' - num) depth pos
aim ((D, num) : xs) aim' depth pos = aim xs (aim' + num) depth pos
aim ((F, num) : xs) aim' depth pos = aim xs aim' (depth + (aim' * num)) (pos + num)

solveFor2 :: [Command] -> Int
solveFor2 vals = aim vals 0 0 0

readAction :: String -> Action
readAction "forward" = F
readAction "up" = U
readAction "down" = D
readAction _ = error "Unknown action"

readCmd :: String -> Command
readCmd str = (action, num)
  where
    num = (read :: String -> Int) (dropWhile (/= ' ') str)
    action = readAction (takeWhile (/= ' ') str)

solve = do
  content <- readFile "input.txt"
  let vals = map readCmd (lines content)
  print "1"
  print (solveFor1 vals)
  print "2"
  print (solveFor2 vals)
