--- status: DRAFT
--- author(s): M. Stillman
--- notes: only added example

document { 
     Key => {ceiling},
     Headline => "ceiling function",
     Usage => "ceiling x",
     Inputs => { "x" => RR },
     Outputs => { { "the smallest integer greater than or equal to the number ", TT "x" } },
     EXAMPLE {
	  "ceiling(-3.4)"
	  },
     SeeAlso => {floor}
     }
