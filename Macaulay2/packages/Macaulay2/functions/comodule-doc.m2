--- status: DRAFT
--- author(s): MES, taken from Dan's
--- notes: 

document { 
     Key => {comodule,
	  (comodule, Ideal),
	  (comodule, Module)},
     Headline => "submodule to quotient module",
     Usage => "comodule M",
     Inputs => {
	  "M" => Module => {"or an ", TO2(Ideal,"ideal")}
	  },
     Outputs => {
	  Module => {"If M is given as a submodule ", TEX "I \\subset N", 
	       "where N is either a free module or a quotient, then
	       the module N/I is returned."},
	  },
     EXAMPLE {
	  "R = QQ[a..d];",
	  "I = ideal(a,b,c,d^3);",
	  "comodule I"
	  },
     SeeAlso => {image, cokernel, kernel}
     }
