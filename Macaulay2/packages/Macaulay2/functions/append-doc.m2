--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => append,
     Headline => "append an element to a list",
     }

document { 
     Key => (append,BasicList,Thing),
     Usage => "append(v,x)",
     Inputs => { "v", "x" },
     Outputs => {{" yields the list obtained by appending ", TT "x", " to the list ", TT "v"}},
     "The result will have the same class.",
     EXAMPLE {
	  "append((a,b,c),z)",
	  "append({a,b,c},z)",
	  },
     SeeAlso =>{ "prepend", "join"}
     }

