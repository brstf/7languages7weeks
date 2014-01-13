// 5 - Prototype for a 2d array - matrix
Matrix := List clone

// Add dim initialization method to allocate a list of
// y lists that are x elements long
Matrix dim := method(x, y,
    // Create y lists
    for(j,1,y,
        innerList := list();
        for(i,1,x,
            innerList append(nil)
        );
        append(innerList)
    )
)

// Set list element (x,y) to value
Matrix set := method(x, y, value, 
    at(y) atPut(x, value)
)

// Get the list element at (x,y)
Matrix get := method(x, y, 
    at(y) at(x)
)

// 6 - Bonus transpose method
Matrix transpose := method(
    for(i, 0, self size - 1, 
        for(j, 0, i - 1, 
            tmp := self get(i,j)
            self set(i, j, self get(j, i))
            self set(j, i, tmp)
        )
    )
)

// 7 - Write / Read matrix to / from file
Matrix writeTo := method(fileName,
    f := File with(fileName);
    f remove;
    f openForUpdating;
    self foreach(l, l foreach(el, f write(el .. " ")); f write("\n"));
    f close
)

Matrix readFrom := method(fileName, 
    f := File with(fileName);
    f openForReading;
    j := 0;
    while(j < self size,
        self atPut(j, f readLine split);
        j = j + 1
    )
)

// Convenience print method
Matrix printMat := method(
    self foreach(l, l foreach(el, el print; " " print) println)
)

"Create and initialize matrix:" println
matrix := Matrix clone
matrix dim(3,3)
matrix set(0,0,1)
matrix set(0,1,2)
matrix set(0,2,3)
matrix set(1,0,4)
matrix set(1,1,5)
matrix set(1,2,6)
matrix set(2,0,7)
matrix set(2,1,8)
matrix set(2,2,9)
matrix printMat
"" println

"Transposed matrix:" println
matrix transpose
matrix printMat
 
matrix writeTo("3x3.mat")

"" println
"Read in this matrix:" println
newMat := Matrix clone
newMat dim(3,3)
newMat readFrom("3x3.mat")
newMat printMat