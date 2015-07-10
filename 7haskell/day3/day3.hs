module Main where

    -- Write a function that looks up a hash table value that 
    -- uses the Maybe type
    keylookup :: Eq a => a -> [(a, b)] -> Maybe b
    keylookup k [] = Nothing
    keylookup k ((hk, hv):t)
        | k == hk = Just hv
        | otherwise = keylookup k t

    -- Write a hash that stores other hashes, several levels
    -- deep, and use Maybe as a Monad to retrieve an element
    -- for a hash several levels deep

    testlookup :: Maybe [Char]
    testlookup = 
        keylookup 10 h >>= keylookup 3 >>= keylookup 8
        where h = [(21,[(17,[( 4,"Hello"),(10,"World!"  )])]),
                   (10,[( 3,[(12,"Hello"),( 8,"Haskell!")])])]

    -- Represent a maze in Haskell with a Maze and Node type,
    -- and a function to return a Node given it's coordinates
    -- A Node should have a list of exits to other Nodes

    type Coords = (Int, Int)
    data Node = Exits [Coords] deriving (Show)
    data Maze = Maze [[Node]]

    -- Given a maze and coordinates, return the corresponding Node
    -- from the maze, or an empty Node with no exits on failure
    getNode :: Maze -> Coords -> Node
    getNode (Maze []) _ = Exits []
    getNode (Maze nodes) (i,j) 
        | i >= (length nodes) = Exits []
        | j >= (length (nodes !! 0)) = Exits []
        | otherwise = nodes !! i !! j

    -- Our very very simple test maze:
    --        +-+-+
    -- enter >  |  > exit
    --        + + +
    --        |   |
    --        +---+

    testGetNode :: [Node]
    testGetNode = 
        [getNode m (1,0), getNode m (2,0), getNode (Maze []) (0,0)]
        where m = Maze [[(Exits [(0,1)]), (Exits [(0,0),(1,1)])],
                        [(Exits [(1,1)]), (Exits [(1,0),(0,1)])]]


    -- Use a List Monad to solve the maze
    solveMaze :: Maze -> Coords -> Coords -> [[Coords]]
    solveMaze m start end = 
        mazeLoop m start end []

    -- Use a recursive function that uses Monad syntax to solve
    -- the maze. If the end is reached return the path, if the 
    -- path has double back on itself return an empty list.
    -- Given: The maze, the coordinates of the current node, 
    -- the current path, and the coordinates of the exit
    -- Return: List of all paths to the exit
    mazeLoop :: Maze -> Coords -> Coords -> [Coords] -> [[Coords]]
    mazeLoop m (ci, cj) (ei, ej) path
        | (ci, cj) == (ei, ej) = [path ++ [(ei, ej)]]
        | any (== (ci, cj)) path = []
        | otherwise = do
            (adji, adjj) <- exits
            p <- (mazeLoop m (adji, adjj) (ei, ej) (path ++ [(ci, cj)]))
            return p
        where (Exits exits) = (getNode m (ci,cj))


    -- Again, our very very simple test maze:
    --        +-+-+
    -- enter >  |  > exit
    --        + + +
    --        |   |
    --        +---+
    testSolve :: [[Coords]]
    testSolve = 
        solveMaze m (0,0) (1,0)
        where m = Maze [[(Exits [(0,1)]), (Exits [(0,0),(1,1)])],
                        [(Exits [(1,1)]), (Exits [(1,0),(0,1)])]]

    -- A slightly more complex maze with two paths to the exit and 
    -- one dead end:
    --        +-+-+-+
    --        |     |
    --        + +-+ +
    -- enter >  | |  > exit
    --        + + + +
    --        |     |
    --        +-+-+-+
    testSolve2 :: [[Coords]]
    testSolve2 = 
        solveMaze m (0,1) (2,1)
        where m = Maze [[(Exits [(1,0),(0,1)]), (Exits [(0,0),(0,2)]), (Exits [(0,1),(1,2)])],
                        [(Exits [(0,0),(2,0)]), (Exits [(1,2)]), (Exits [(0,2),(1,1),(2,2)])],
                        [(Exits [(1,0),(2,1)]), (Exits [(2,0),(2,2)]), (Exits [(2,1),(1,2)])]]