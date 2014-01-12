// Overload / operator so x / 0 = 0
oldDiv := Number getSlot("/")
Number / := method(x, if(x == 0, 0, self oldDiv(x)))

(3 / 2) println
(10 / 5) println
(10 / 0) println