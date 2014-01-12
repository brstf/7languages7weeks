// Method to compute the sum of a 2d list
twodsum := method(ol, s := 0; ol foreach(il, s = s + il sum))
list2d := list(list(1,2,3), list(4,5,6), list(7,8,9))
twodsum(list2d) println