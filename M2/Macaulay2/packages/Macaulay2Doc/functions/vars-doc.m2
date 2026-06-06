--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => vars,
     Headline => "variables",
     }

document { 
     Key => {(vars,Ring)},
     Headline => "row matrix of the variables",     
     Usage => "vars R",
     Inputs => {
	  "R"
	  },
     Outputs => {
	  Matrix =>  { "a matrix with one row whose entries are the variables of the ring ", TT "R"}
	  },
     EXAMPLE {
	  "S = QQ[w,x,y,z];",
	  "vars S",
	  "ideal vars S",
	  "coker vars S",
	  "res coker vars S",
	  "R = S/(x^2-w*y, x*y-w*z, x*z-y^2);",
          "vars R",
	  "use S;",
	  "Q = S/(x^2-w*y, z);",
	  "vars S"
	  },
     }

document { 
     Key => {(vars, Monoid)},
     Headline => "list of variables in a monoid",
     Usage => "vars M",
     Inputs => { "R" },
     Outputs => { List => { "a list of the generators of ", TT "M"} },
     "The result is the same as that provided by ", TO (generators, Monoid), ".",
     EXAMPLE lines ///
     	  M = monoid[x,y,z]
	  vars M
     ///
     }
document { 
     Key => {(vars,List), (vars,Sequence), (vars,ZZ)},
     Headline => "a sequence of variables",
     Usage => "vars L",
     Inputs => {
	  "L" => {", integer, or ", TO2("Sequence", "sequence"), " of integers"}
	  },
     Outputs => {
	  Sequence => 
	  {"which consists of ", TO2("Symbol","symbols"), 
	       " which can be used as indeterminates"}
	  },
     "The ", TO2("Symbol","symbols"), " returned are single letters, or the letter ", 
     TT "x", " or ", TT "X", " followed by some digits.  ",   
     "There is no limit on the size or sign of the integers in ", TT "L", ".",
     EXAMPLE {
	  "vars{0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25}",
	  "vars(26..51)",
	  "vars 52",
	  "vars(-9..-1)",
	  "vars(3 .. 9, 33 .. 35, 1000 .. 1002, -100 .. -98)"
	  },
     SeeAlso => {".."}
     }

