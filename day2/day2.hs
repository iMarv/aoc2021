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

aimDepthPos :: [Command] -> Int -> Int -> Int -> Int
aimDepthPos cmds aim depth pos
  | null cmds = pos * depth
  | isUp = recur (aim - x) depth pos
  | isDown = recur (aim + x) depth pos
  | isFwd = recur aim (depth + (aim * x)) (pos + x)
  | otherwise = error "Ooops!"
  where
    cmd = head cmds
    act = fst cmd
    x = snd cmd
    isUp = act == Up
    isDown = act == Down
    isFwd = act == Forward
    recur = aimDepthPos (tail cmds)

solveFor2 :: [Command] -> Int
solveFor2 vals = aimDepthPos vals 0 0 0

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
  print "2"
  print (solveFor2 vals)
