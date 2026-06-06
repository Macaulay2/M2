document {
    Key => {
	 instances,
	(instances, Type),
    },
     Usage => "instances X",
     Inputs => { "X" },
     Outputs => {{"a hash table listing global symbols whose values are instances of type ", TT "X"}},
     EXAMPLE lines ///
     20!
     instances ZZ
     ///
     }

