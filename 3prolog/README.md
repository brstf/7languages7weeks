Picross Solver
--------------

General MxN picross solver. Arguments are a list of column constraints, a list of row contraints, and the board itself.  For example, for a 5x5 picross board like the following:

    
          2   4   4   4   2
        +---+---+---+---+---+
    1 1 |   |   |   |   |   |
        +---+---+---+---+---+
      5 |   |   |   |   |   |
        +---+---+---+---+---+
      5 |   |   |   |   |   |
        +---+---+---+---+---+
      3 |   |   |   |   |   |
        +---+---+---+---+---+
      1 |   |   |   |   |   |
        +---+---+---+---+---+


We can input the constraints to this puzzle by:

    | ?- picross([[2],[4],[4],[4],[2]],[[1,1],[5],[5],[3],[1]],Board).
    
      X   X
    X X X X X
    X X X X X
      X X X
        X

It also handles general MxN puzzles, like the following:


    | ?- picross([[2],[2],[2],[2],[2]],[[1,1,1],[5],[1,1]],Board).
    
    X   X   X 
    X X X X X 
      X   X   


I'm not positive that this is well optimized, but it works on all test cases I've tried. It runs fairly slowly on large sized puzzles, but it solves smaller ones in a reasonable amount of time:

    | ?- picross([[1],[2,1],[5,1],[4],[2,3],[4,1],[2,4],[1,2]],[[2],[1,4],[2,2],[3,2],[2,2],[4,2],[4],[3]],Board).

              X X   
        X   X X X X 
      X X   X X     
    X X X     X X   
        X X     X X 
      X X X X   X X 
          X X X X   
        X X X       

