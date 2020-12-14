document {
     Key => baseRings,
     Headline => "store the list of base rings of a ring",
     TT "baseRings", " -- a symbol used as a key in a ring ", TT "R", " under which is
     stored a list of base rings for ", TT "R", ".",
     PARA{
     	  "A base ring ", TT "A", " of ", TT "R", " is one of the rings involved in the
     	  construction of ", TT "R", "."
	  },
     PARA{ "The base rings are presented in chronological order." },
     SeeAlso => { baseRing }
     }

document {
     Key => {baseRing,(baseRing, Ring)},
     Headline => "produce the ring from which a ring was formed",
     Usage => "baseRing R",
     Inputs => { "R" => Ring },
     Outputs => { Ring => "the ring from which ", TT "R", " was formed" },
     PARA {
	  "The base ring of a ring ", TT "R", " is the ring from which ", TT "R", " was formed.
	  For example, if ", TT "R", " is a quotient ring of the form ", TT "S/I", ", 
	  or if ", TT "R", " is a fraction ring of the form ", TT "frac S", ", 
	  or if ", TT "R", " is a polynomial ring over ", TT "S", ",
	  then the base ring is ", TT "S", "."
	  },
     EXAMPLE lines ///
     baseRing QQ
     R = QQ[x,y]
     S = R / (x^2 + y^3 - 1)
     T = frac S
     baseRing T
     baseRing S
     baseRing R
     ///,
     PARA {
	  "The entire chain of base rings can be obtained under the key ", TO "baseRings", "."
	  },
     EXAMPLE "T.baseRings",
     SeeAlso => { baseRings }
     }
