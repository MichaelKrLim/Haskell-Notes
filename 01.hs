{-
    Types:
        Int - limited memory (max/minBound)
        Integer - arbitrary memory
        Double - Double-precision floating point numbers
        Bool
        Char
        String

    note backtics make a function name into an infix operator.

    Haskell does not do implicit conversion. We have fromIntegral (Int or Integer to any other numeric type) & round, floor, and ceiling to convert floating-point numbers to Int or Integer 

    '/' divides floating-point numbers only. We need `div` for Integer division.
-}

-- Definition of Functions:

sum_to :: Integer -> Integer
sum_to 0 = 0
sum_to n = n + sum_to(n-1)

-- Guards:
{-
functionName arguments
    | condition1 = result1
    | condition2 = result2
    | condition3 = result3
    | otherwise  = defaultResult
-}

describeNumber :: Int -> String
describeNumber n
    | n < 0     = "Negative"
    | n == 0    = "Zero"
    | n > 0     = "Positive"

-- Pairs:

pair :: (Int, Char)
pair = (3, 'x')

-- Pairs can be accepted as their values (sum_pair(pair) = sum_pair(x,y))
sum_pair :: (Int, Int) -> Int
sum_pair(x, y) = x+y 

-- Lists:

list :: [Integer]
list = [1,2 .. 100]

-- Lists can be added with ++
-- Lists can be constructed with cons ':'
ex21 = [2,3,4] == 2 : 3 : 4 : []

-- Pattern matching also works with lists:
list_length :: [Integer] -> Integer
list_length []     = 0
list_length (x:xs) = 1 + list_length xs

sum_every_two :: [Integer] -> [Integer]
sum_every_two []         = []
sum_every_two (x:[])     = [x]
sum_every_two (x:(y:zs)) = (x + y) : sum_every_two zs