object Board {
    def getChar(id : Int) : Char = {
        if( id == -1 ) {
            return 'O'
        } else if ( id == 0 ) {
            return '.'
        } else {
            return 'X'
        }
    }
}

class Board {
    var board = Array.ofDim[Int](9)

    def getSumRow(index: Int) : Int = {
        var sum = 0
        for ( i <- (3 * index) until (3 * index) + 3 ) {
            sum += board(i)
        }
        return sum
    }

    def getSumCol(index: Int) : Int = {
        var sum = 0
        for ( i <- index until 9 by 3 ) {
            sum += board(i)
        }
        return sum
    }

    def getSumForwardDiagonal() : Int = {
        var sum = 0
        for ( i <- 0 until 9 by 4 ) {
            sum += board(i)
        }
        return sum
    }

    def getSumBackwardDiagonal() : Int = {
        var sum = 0
        for ( i <- 2 until 7 by 2 ) {
            sum += board(i)
        }
        return sum
    }

    def getWinner() : Int = {
        var score = Array.ofDim[Int](8)
        for(i <- 0 until 3) {
            score(i) = getSumRow(i)
            score(i + 3) = getSumCol(i)
        }
        score(6) = getSumForwardDiagonal()
        score(7) = getSumBackwardDiagonal()

        if ( score.contains(3) ) {
            return 1
        } else if ( score.contains(-3) ) {
            return -1
        } else {
            return 0
        }
    }

    override def toString() : String  = {
        var s : String = ""
        for ( i <- 1 to board.length ) {
            s += Board.getChar(board(i - 1)) + " "
            if (i % 3 == 0) s += "\n"
        }
        return s
    }
}

class TicTacToe { 
    var b = new Board()
    gameLoop()

    def get(row : Int, col : Int ) : Int = {
        return b.board(row * 3 + col)
    }

    def set(row : Int, col : Int, id : Int) {
        b.board(row * 3 + col) = id
    }

    def setX(row : Int, col : Int) {
        set(row, col, 1)
    }

    def setO(row : Int, col : Int) {
        set(row, col, -1)
    }

    def gameLoop() {
        var winner = 0
        var player = 1
        while(winner == 0) {
            println(b.toString())
            print("[" + Board.getChar(player) + "] Enter row,col: ")
            var input = readLine
            var coords = input.split(",")
            if ( coords.length != 2 ) {
                println("Coordinates must be of the form r,c where r = [0-2], c = [0-2]")
            } else {
                // Currently no input validating 
                val row = coords(0).toInt
                val col = coords(1).toInt

                if( get(row, col) != 0 ) {
                    println("You can only make a play in an empty square!")
                } else {
                    if( player == 1 ) {
                        setX(row, col)
                    } else {
                        setO(row, col)
                    }
                    player = -player
                    winner = b.getWinner()
                }
            }
        }
        print(b.toString())
        println("Player [" + Board.getChar(winner) + "] wins!")
    }
}

var t = new TicTacToe()