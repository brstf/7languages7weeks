// Enhance the XML program to handle attributes

Builder := Object clone
Builder indent := 0
Builder args := nil

// Curly brackets method to construct a map from the given attributes

Builder curlyBrackets := method(
    attrs := Map clone

    call message arguments foreach(arg,
        pair := arg asString split(":")
        attrs atPut(pair at(0) asMutable strip, pair at(1) asMutable strip)
    )
)

Builder forward := method(
    // Get the list of arguments we can modify
    self args = call message arguments

    self indent repeat(write("  "))
    write("<", call message name)

    // If the first argument is a map, print attributes
    // Testing if the message is a curlyBrackets message does not seem 
    // like an optimal way to do this
    if(args at(0) name == "curlyBrackets",
        m := doMessage(args at(0))
        m keys foreach(key,
            write(" ")
            write(key asMutable removePrefix("\"") removeSuffix("\""))
            write("=", m at(key))
        )
        args removeAt(0)
    )
    writeln(">")

    self indent = self indent + 1

    call message arguments foreach(arg,
        content := self doMessage(arg);
        if(content type == "Sequence", 
            self indent repeat(
                write("  ")); 
                writeln(content)
            )
        )

    // Decrease indent after processing each argument
    self indent = self indent - 1

    self indent repeat(write("  "))
    writeln("</", call message name, ">"))

// Now we use the BUilder as normal
Builder body(
    hr(
        b("Programming Languages")
    ),
    ul(
        li({"author" : "Steve Dekorte", "character":"Ferris Bueller"}, "Io"),
        li({"author" : "Matz", "character" : "Mary Poppins"}, "Ruby")
    )
)
