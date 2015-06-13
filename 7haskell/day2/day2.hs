module Main where
    import Data.Char

    -- Write a sort that takes a list and returns a sorted list

    sort [] = []
    sort (h:t) = sort(filter (<= h) t) ++ [h] ++ sort(filter (> h) t)

    -- Write a sort that takes a list and a function and sorts the
    -- list by the given function

    sortBy [] f = []
    sortBy (h:t) f = leftSort ++ [h] ++ rightSort
        where
            leftSort  = sortBy [x | x <- t, f x h] f
            rightSort = sortBy [x | x <- t, not(f x h)] f

    -- Write a function to convert a string to a number where the 
    -- string is of the form:
    --          "$2,345,678.99"
    -- and can possibly have leading 0s

    -- Skipping h the head and operating solely on the tail assumes
    -- the input is correct, i.e. always starts with "$", then we filter
    -- the ',' and '.'s out, and use foldl to build the total value * 100
    -- then divide the result by 100
    stringToDollar (h:t) = 
        fromIntegral(
            foldl 
                (\acc x -> acc * 10 + digitToInt x) 
                0
                [c | c <- t, c /= '.', c /= ',']
        ) / 100

    -- Write a function that takes an argument x and returns a lazy
    -- sequence that has every third number, starting with x
    -- Write a similar function that takes y and returns every fifth
    -- number
    -- Combine the two through composition to return every eighth
    -- number beginning with x + y

    everyThird :: Integer -> [Integer]
    everyThird x = x:(everyThird (x + 3))

    everyFifth :: Integer -> [Integer]
    everyFifth y = y:(everyFifth (y + 5))

    -- Not sure how to combine these functions with composition, for 
    -- composition, the return type of one function must match the input
    -- type of the other, which we can't do here unless I've misunderstood
    -- the everyFifth function
    everyEighth x y = zipWith (+) (everyThird x) (everyFifth y)

    -- Use a partially applied functino to define a function that will
    -- return half of a number and another that will append \n to the 
    -- end of any string

    half :: Fractional a => a -> a
    half = (/2)

    addNewline :: [Char] -> [Char]
    addNewline = (++"\n")