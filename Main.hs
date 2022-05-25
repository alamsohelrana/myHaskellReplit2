import Data.List -- (\\)  -- (\\) is set-difference for unordered lists

addMe :: Integer -> Integer -> Integer
addMe x y = x + y

firstOrEmpty lst = if not (null lst) then head lst else "empty"

myHead (x : xs) = x

myTail (x : xs) = xs

addAnA [] = []
addAnA (x : xs) = (if x /= "SuperMan" then (++) "a " x else ("No! It's " ++) x) : addAnA xs

myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f newInit xs
  where
    newInit = f init x

myFoldr f init [] = init
myFoldr f init (x : xs) = f x rightResult
  where
    rightResult = myFoldr f init xs

harmonic x = foldl (+) 0 (map (\x -> 1 / x) (take x [1 ..]))

primesTo m = sieve [2 .. m]
  where
    sieve (x : xs) = x : sieve (xs \\ [x, x + x .. m])
    sieve [] = []

getMax :: [Integer] -> Integer
getMax [x] = x
getMax (x : xs) = if x >= newMax then x else newMax
  where
    newMax = getMax xs

getMin :: [Integer] -> Integer
getMin [x] = x
getMin (x : xs) = if x <= newMin then x else newMin
  where
    newMin = getMin xs

basicSort :: [Integer] -> [Integer]
basicSort [x] = [x]
basicSort xs =
  let y = getMin xs
      ys = filter (\x -> x /= y) xs
   in y : basicSort ys

main :: IO ()
main = do
  putStr "Sum of x + y = "
  print (myHead "abc")
  print (myTail "def")

  print (map (take 4) ["pumpkin", "pie", "peanut butter"])
  print ((\x -> (if x /= "SuperMan" then ("a " ++) x else ("No! It's " ++) x)) "Dog")
  print ((\x -> (^ 2) x) 23)

  print (addAnA ["dog", "plane", "boat", "SuperMan"])

  print (myFoldl (+) 0 [1, 2, 3, 4])

  putStr "135! = "
  print (myFoldl (*) 1 (take 135 [1 ..]))

  putStrLn ""
  print (harmonic 555)

  print (primesTo 100)

  putStrLn ""
  print (getMax [121, 23, 33456, 4])

  putStrLn ""
  print (basicSort (primesTo 100))