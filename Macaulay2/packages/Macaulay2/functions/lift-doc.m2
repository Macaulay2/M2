--- status: DRAFT
--- author(s): MES
--- notes: BUG to fix

undocumented {
	  (lift,Ideal,ZZ),
	  (lift,ZZ,Ring),
	  (lift,QQ,Ring),
	  (lift,ZZ,ZZ),
	  (lift,QQ,ZZ),
	  (lift,QQ,QQ),
	  (lift,Matrix,ZZ),
	  (lift,RingElement,ZZ),
	  (lift,RingElement,QQ)}

document { 
     Key => {lift,
	  (lift,Ideal,Ring),
	  (lift,Matrix,Ring),
	  (lift,RingElement,Ring)},
     Headline => "lift to another ring",
     Usage => "lift(f,R)",
     Inputs => {
	  "f" => {"a ", TO2(RingElement,"ring element"), ", ",
	       TO2(Ideal, "ideal"), ", or ", 
	       TO2(Matrix, "matrix")},
	  "R" => Ring => ""
	  },
     Outputs => {
	  {"a ", TO2(RingElement,"ring element"), ", ",
	       TO2(Ideal, "ideal"), ", or ", 
	       TO2(Matrix, "matrix"), 
	       ", over the ring ", TT "R"}
	  },
     "The ring ", TT "R", " should be one of the base rings 
     associated with the ring of ", TT "f",".  An error is raised if
     ", TT "f", " cannot be lifted to ", TT "R", ".",
     PARA,
     "The first example is lifting from the fraction field of R to R.",
     EXAMPLE {
	  "lift(4/2,ZZ)",
	  "R = ZZ[x];",
	  "f = ((x+1)^3*(x+4))/((x+4)*(x+1))",
     	  "lift(f,R)"
	  },
     PARA,
     "Another use of lift is to take polynomials in a quotient ring
     and lift them to the polynomial ring.",
     EXAMPLE {
	  "A = QQ[a..d];",
	  "B = A/(a^2-b,c^2-d-a-3);",
	  "f = c^5",
	  "lift(f,A)",
	  "jf = jacobian ideal f",
	  "lift(jf,A)"
	  },
     PARA,
     "Elements may be lifted to any base ring, if such a lift exists.
     For example,",
     EXAMPLE {
	  "use B;",
	  "g = (a^2+2*a-3)-(a+1)^2",
	  "lift(g,A)",
	  "lift(g,QQ)",
	  "lift(lift(g,QQ),ZZ)"
	  },
     SeeAlso => {baseRings,liftable,promote}
     }

TEST ///
A = QQ[a..d]
f = (a+1)^2-a^2-2*a
lift(f,ZZ)  -- BUG
lift(lift(f,QQ),ZZ)
///
