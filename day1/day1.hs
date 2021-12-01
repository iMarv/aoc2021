testinput :: [Int]
testinput = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

isIncrease :: Int -> [Int] -> Bool
isIncrease window l = sum firstWindow < sum secondWindow
  where
    firstWindow = take window l
    secondWindow = take window (drop window l)

countInc :: Int -> [Int] -> Int -> Int
countInc window remaining incs
  | notEnoughRemaining = incs
  | isIncrease window remaining = recur incs + 1
  | otherwise = recur incs
  where
    notEnoughRemaining = length remaining < window + 1
    recur = countInc window (tail remaining)

solve_1 = do
  content <- readFile "input.txt"
  let vals = map (read :: String -> Int) $ lines content
  print "1:"
  print (countInc 1 vals 0)
  print "2:"
  print (countInc 3 vals 0)
