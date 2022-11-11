--- status: DRAFT
--- author(s): MES
--- notes: just added example

document {
     Key => {floor},
     Headline => "floor function",
     SeeAlso => {ceiling}
     }
     
document { 
     Key => {(floor,Number), (floor, Constant)},
     Headline => "floor function",
     Usage => "floor x",
     Inputs => { "x" },
     Outputs => { { "the greatest integer less than or equal to the number ", TT "x", ".  The
	       imaginary part of a complex number is ignored." } },
     EXAMPLE {
	  "floor (7.234232131231*10^6)"
	  },
     SeeAlso => {(ceiling, Number), (truncate, Number)}
     }

