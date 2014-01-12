// Fibonacci sequence using a for loop
fib := method(f,
    f1 := 0;
    f2 := 1;
    for(i,2,f,
        f1 = f2 + f1;
        tmp := f2;
        f2 = f1;
        f1 = tmp
    );
    f2
)

"Using A For Loop:" println
fib(1) println
fib(2) println
fib(4) println
fib(8) println

// Fibonacci sequence using recursion
fibr := method(f1, f2, f, d, 
    if(d <= f, 
        fibr(f2, f1 + f2, f, d+1),
    f2)
)

fib := method(f, fibr(0,1,f,2))

"Using Recursion:" println
fib(1) println
fib(2) println
fib(4) println
fib(8) println