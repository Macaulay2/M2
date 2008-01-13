--- status: DRAFT
--- author(s): from before, MES
--- notes: 

document { 
     Key => {member,
	  (member,Thing,Set),
	  (member,Thing,VisibleList)},
     Headline => "test membership in a list or set",
     Usage => "member(e,x)",
     Inputs => {
	  "e" => Thing,
	  "x" => {ofClass {List, Sequence, Set}}
	  },
     Outputs => {
	  Boolean => "whether e is in the list, sequence, or set x"
	  },
     EXAMPLE {
      	  "member(c,{a,b,c,d,e})",
      	  "member(f,(a,b,c,d,e))",
	  "member(3,set{1,2,5,6})"
	  },
     SeeAlso => {positions, Set}
     }
