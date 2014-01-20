// Enhance the XML program to add spaces to show indentation structure
Builder := Object clone

// Simple solution, add an indent level to the builder
Builder indent := 0

Builder forward := method(
    // Before every write statement, print spaces for the
    // indent level
    self indent repeat(write("  "))
    writeln("<", call message name, ">")

    // Increase indent before calling each argument
    self indent = self indent + 1

    call message arguments foreach(
        arg,
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
Builder body(hr(b("Programming Languages")), ul(li("Io"), li("Lua"), li("JavaScript")))