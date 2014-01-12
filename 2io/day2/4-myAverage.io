// List myAverage without exception raising
List myAverage := method(self sum / self size)

l := list(1,2,3,4)
l myAverage println

// List myAverage with exceptions for non-Numbers
List myAverage := method(
    self foreach(el, 
        if(el proto != Number, 
            Exception raise("Error: Non-Number in the list.")
        )
    );
    self sum / self size
)

l myAverage println
l = list(1,2,3,"four")
l myAverage println