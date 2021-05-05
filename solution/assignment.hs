import Data.Char(intToDigit)

-- The factorial of N (written N!) is N*(N-1)*(N-2)...*1, so 1! is 1, 
-- and 3! is 1*2*3 = 6. Don't worry about 0! for now.
--
-- Write a recusive algorithm that returns the factorial of
-- a positive integer

factorial :: (Eq p, Num p) => p -> p
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- Write the recursive function `toDigits` that takes an 
-- integer and returns its string representation (you can't
-- use `show` :)
--
-- Helpful built in functions include `intToDigit` which
-- takes an integer between 0 and 9 and returns the corresponding 
-- character; and the function `divmod`
--
--   (truncatedResult, remainder) = divMod integer divisor
--
--   truncatedResult * divisor + remainder == integer
--
-- You may also need list concatenation, `++`.

toDigits :: Int -> [Char]
toDigits n 
    | n < 10    = [ intToDigit n ]
    | otherwise = toDigits rest ++ toDigits lowOrder 
    where
        ( rest, lowOrder ) = divMod n 10



-- Write a recursive function that counts the number of times
-- a given value occurs in a list

count :: (Eq a, Num p) => a -> [a] -> p
count val [] = 0 
count val (x:xs) =
    n + count val xs
    where 
        n = if val == x then 1 else 0


-- Given a list containing an even number of integers, 
-- determine if the sum of the numbers at even indices
-- equals the sum of the numbers at odd indices. It 

evensEqualOdds :: [ Integer ] -> Bool
evensEqualOdds list =
    evens == odds 
    where
        ( evens, odds ) = getSums list

getSums :: [ Integer ] -> ( Integer, Integer)
getSums [] = ( 0, 0)
getSums (x1 : x2 : xs) =
    ( t1 + x1, t2 + x2)
    where 
        ( t1, t2 ) = getSums xs

-- the zipper function should take two equal length lists and
-- combine corresponding elements from each into a tuple,
-- so 
--          zipper [1, 2]  [ "cat", "dog"]
-- will be
--          [ (1, "cat"), (2, "dog") ]
--
-- Implement `zipper` as a recursive function

zipper :: [a] -> [b] -> [(a, b)]
zipper [] [] = []
zipper (l1:l1s) (l2:l2s) =
    (l1, l2) : zipper l1s l2s

-----------------------------------
-- don't change below this point --

test :: (Eq a, Show a) => [Char] -> a -> a -> IO ()
test testNo got expected
  | got == expected =
      print (testNo ++ " OK   got expected value " ++ show expected)
  | otherwise =
      print (testNo ++ " FAIL expected " ++ show expected ++ " but got " ++ show got)


main = do
  test "x1" (factorial 1)    1
  test "x2" (factorial 2)    2
  test "x3" (factorial 5)  120
  test "x4" (factorial 10) 3628800

  test "y1" (toDigits 0)    "0"
  test "y2" (toDigits 7)    "7"
  test "y3" (toDigits 42)  "42"
  test "y4" (toDigits 987654321)  "987654321"

 
  test "1a" (count 'a' "hello") 0
  test "1b" (count 'h' "hello") 1
  test "1c" (count 'o' "hello") 1
  test "1d" (count 'l' "hello") 2

  test "4a" (evensEqualOdds [])             True
  test "4b" (evensEqualOdds [99, 99])       True
  test "4c" (evensEqualOdds [99, 1, 1, 99]) True
  test "4d" (evensEqualOdds [3, 4])         False
  test "4e" (evensEqualOdds [-1, -2, 4, 5]) True
  test "4f" (evensEqualOdds [1, 1, 2, 2, 3, 3, 4, 4]) True

  test "5a" (zipper [1,2] ["cat", "dog"]) [ (1, "cat"), (2, "dog") ]
  test "5b" (zipper "Hello" "World") [ ('H','W'), ('e','o'), ('l','r'), ('l','l'), ('o','d') ]