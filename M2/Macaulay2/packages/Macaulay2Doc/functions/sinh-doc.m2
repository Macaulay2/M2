--- status: DRAFT
--- author(s): L. Gold
--- notes: include example?

document { 
     Key => {sinh,(sinh,CC),(sinh,ZZ),(sinh,RR),(sinh, QQ),(sinh, RRi)},
     Headline => "compute the hyperbolic sine",
     Usage => "sinh x\nsinh I",
     Inputs => {
	  "x" => RR,
    "I" => RRi
	  },
     Outputs => {
	  RR => { "the hyperbolic sine of ", TT "x" },
    RRi => { "an interval containing the hyerbolic sines of the points of ", TT "I" }
     }
--     EXAMPLE {
--	  }
} 
