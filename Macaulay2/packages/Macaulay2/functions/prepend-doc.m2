--- status: done
--- author(s): dan
--- notes: 

document { 
     Key => prepend,
     Headline => "add an element to the beginning of a list",
     }

document { 
     Key => (prepend,Thing,BasicList),
     Usage => "prepend(x,v)",
     Inputs => { "x", "v" },
     Outputs => {{" yields the list obtained by prepending " , TT "x", " to the list ", TT "v"}},
     "The result will have the same class.",
     EXAMPLE {
	  "prepend(z,(a,b,c))",
	  "prepend(z,{a,b,c})",
	  },
     SeeAlso => {"append", "join"}
     }
