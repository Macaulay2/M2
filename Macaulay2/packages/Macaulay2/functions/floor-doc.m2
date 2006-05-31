--- status: DRAFT
--- author(s): MES
--- notes: just added example

document { 
     Key => {floor,(floor,RR)},
     Headline => "floor function",
     Usage => "floor x",
     Inputs => { "x" => RR },
     Outputs => { { "the largest integer less than or equal to the number ", TT "x" } },
     EXAMPLE {
	  "floor (7.234232131231*10^6)"
	  },
     SeeAlso => {ceiling}
     }

