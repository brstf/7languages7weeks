// Create a list syntax that uses brackets

// Add squareBrackets to Object to create lists with brackets
Object squareBrackets := method(
    call message arguments
)

// Add squareBrackets to List to access elements of the list
List squareBrackets := method(arg,
    doMessage(self at(arg))
)

// Create a list with brackets
l := [1,2,3,4,5]
writeln(l)

// Test accessing elements of the list
writeln(l[1])
writeln(l[2])
writeln(l[1] + l[2])
writeln(l[3,4])