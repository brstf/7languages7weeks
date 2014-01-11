// Day 1 of Io: Object, messages, prototypes, slots
// Testing different singletons for truth value
if(1) then("1 is true." println) else("1 is false." println)
if(0) then("0 is true." println) else("0 is false." println)
if("") then("\"\" is true." println) else("\"\" is false." println)
if(nil) then("nil is true." println) else("nil is false." println)
"" println

// Get what slots a prototype supports
Vehicle := Object clone
Vehicle description := "Something to take you places"
Car := Vehicle clone
Car description = "Something to take you places with wheels"
ferrari := Car clone
"Valid slot names for the prototype of ferrari:" println
ferrari proto slotNames println
"" println

// ::= vs := vs =
Car speed ::= 5
Car numWheels := 4
Car description = "Vehicle with 4 wheels and a motor"
Car slotNames println
"" println

// Execute code in slot given its name
Car printDescription := method(Car description println)
Car getSlot("printDescription") call