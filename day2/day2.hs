testinput :: [Command]
testinput = [(Forward, 5), (Down, 5), (Forward, 8), (Up, 3), (Down, 8), (Forward, 2)]

data Action = Forward | Up | Down deriving (Enum, Eq)

type Command = (Action, Int)

commandToInt :: Command -> Int
commandToInt (Forward, num) = num
commandToInt (Up, num) = num * (-1)
commandToInt (Down, num) = num

sumDepth :: [Command] -> Int
sumDepth cmds = sum nums
  where
    cmds' = filter (\a -> fst a /= Forward) cmds
    nums = map commandToInt cmds'

sumForwards :: [Command] -> Int
sumForwards cmds = sum fwds
  where
    fwds = map snd (filter (\a -> fst a == Forward) cmds)

solveFor1 :: [Command] -> Int
solveFor1 cmds = sumForwards cmds * sumDepth cmds

readAction :: String -> Action
readAction "forward" = Forward
readAction "up" = Up
readAction "down" = Down
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
