--- status: done
--- author(s): Dan
--- notes: 

document {
     Key => {
	 between,
	(between, Thing, VisibleList),
     },
     Headline => "insert something between elements of a list",
     Usage => "between(m,v)",
     "Inserts ", TT "m", " between each pair of elements of the list or sequence ", TT "v", ", returning a list.",
     EXAMPLE {
	  "between(55,{a,b,c,d})",
	  ///concatenate between(",", (1..10) / toString)///
	  }
     }
