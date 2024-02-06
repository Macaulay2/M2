--- status: DRAFT
--- author(s): MES, taken from Dan's
--- notes: 

document { 
     Key => {comodule,
	  (comodule, Ideal),
	  (comodule, Module),
	  (quotient, Ideal),
	  (quotient, Module)
	  },
     Headline => "submodule to quotient module",
     Usage => "comodule M\nquotient M",
     Inputs => {
	  "M" => Module => {"or an ", TO2(Ideal,"ideal")}
	  },
     Outputs => {
	  Module => {"the quotient module ", TT "N/M", ", if ", TT "M", " is given as a submodule $M \\subset N$",
	       ", where ", TT "N", " is either a free module or a quotient of a free module."},
	  },
     EXAMPLE {
	  "R = QQ[a..d];",
	  "I = ideal(a,b,c,d^3);",
	  "comodule I"
	  },
     SeeAlso => {image, cokernel, kernel}
     }
