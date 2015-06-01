module Main where

    -- How many different ways can you find to write allEven
    -- where allEven takes a list of Integers and returns a list
    -- of only the even integers in the list

    -- The original example:
    allEven :: [Integer] -> [Integer]
    allEven [] = []
    allEven (h:t) = if even h then h:allEven t else allEven t

    -- With a list comprehension
    allEvenComp :: [Integer] -> [Integer]
    allEvenComp l = [x | x <- l, even x]

    -- Using guards
    allEvenGuard :: [Integer] -> [Integer]
    allEvenGuard (h:t)
        | even h = h : allEven t
        | odd h  = allEven t
        | otherwise = []


    -- Write a function that takes a list and returns the same 
    -- list in reverse
    reverse2 [] = []
    reverse2 l = last l : reverse2 (take (length l - 1) l)


    -- Write a function that builds two-tuples with all possible
    -- of two of the colors black, white, blue, yellow, and red
    -- We can use essentially the same solution as away party pairs
    -- found in the book
    pairs :: [[Char]] -> [([Char], [Char])]
    pairs l = [(a, b) | a <- l, b <- l, a < b]

    colorPairs :: [([Char], [Char])]
    colorPairs = pairs ["black", "white", "blue", "yellow", "red"]


    -- Write a list comprehension to build a childhood multiplication
    -- table
    mTables :: [(Integer, Integer, Integer)]
    mTables = [(a, b, a * b) | a <- [1..12], b <- [1..12]]


    -- Solve the map-coloring problem 
    -- Given the 5 southeastern states and their adjacencies
    -- assign each a color so that no two adjacent states are
    -- colored the same
    -- In the context of the problem as it was posed, we are 
    -- given 3 colors (red, blue, green), and have to come up with
    -- a valid assignment

    -- This is too hardcoded for my taste, but will return every
    -- valid coloring for this specific problem
    -- The returned tuples will be the colorings for:
    --   [ (tennessee, mississippi, alabama, georgia, flordia), .. ]
    mapColor :: [[Char]] -> [([Char], [Char], [Char], [Char], [Char])]
    mapColor colors = [(a, b, c, d, e) | a <- colors, b <- colors, c <- colors,
                        d <- colors, e <- colors, a /= b, a /= c, a /= d, b /= c,
                        c /= d, c /= e, d /= e]