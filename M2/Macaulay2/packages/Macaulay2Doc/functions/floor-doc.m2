--- status: DRAFT
--- author(s): MES
--- notes: just added example

document { 
     Key => {floor,(floor,Number)},
     Headline => "floor function",
     Usage => "floor x",
     Inputs => { "x" => Number },
     Outputs => { { "the greatest integer less than or equal to the number ", TT "x", ".  The
	       imaginary part of a complex number is ignored." } },
     EXAMPLE {
	  "floor (7.234232131231*10^6)"
	  },
     SeeAlso => {ceiling}
     }

