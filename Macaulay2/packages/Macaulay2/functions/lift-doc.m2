--- status: DRAFT
--- author(s): MES
--- notes: BUG to fix

undocumented {
     (lift, CC, QQ),
     (lift, CC, RR),
     (lift, CC, ZZ),
     (lift, Ideal, QQ),
     (lift, Ideal, ZZ),
     (lift,Matrix,CC,QQ),
     (lift,Matrix,CC,RR),
     (lift,Matrix,CC,ZZ),
     (lift,Matrix,Number),
     (lift,Matrix,QQ,QQ),
     (lift,Matrix,QQ,ZZ),
     (lift,Matrix,RR,QQ),
     (lift,Matrix,RR,ZZ),
     (lift,Matrix,ZZ,ZZ),
     (lift, QQ, QQ),
     (lift, QQ, ZZ),
     (lift, RR, QQ),
     (lift, RR, ZZ),
     (lift, ZZ, ZZ)
     }

document { 
     Key => {lift,
	  (lift,Ideal,RingElement),
	  (lift,Matrix,RingElement)},
     Headline => "lift to another ring",
     Usage => "lift(f,R)",
     Inputs => {
	  "f" => {"a ", TO2(RingElement,"ring element"), ", ",
	       TO2(Ideal, "ideal"), ", or ", 
	       TO2(Matrix, "matrix")},
	  "R" => Ring,
	  Verify => Boolean => {"whether to give an error message if lifting is not possible, or, alternatively, to return ", TO "null"}
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
     PARA{},
     "The first example is lifting from the fraction field of R to R.",
     EXAMPLE {
	  "lift(4/2,ZZ)",
	  "R = ZZ[x];",
	  "f = ((x+1)^3*(x+4))/((x+4)*(x+1))",
     	  "lift(f,R)"
	  },
     PARA{},
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
     PARA{},
     "Elements may be lifted to any base ring, if such a lift exists.
     For example,",
     EXAMPLE {
	  "use B;",
	  "g = (a^2+2*a-3)-(a+1)^2",
	  "lift(g,A)",
	  "lift(g,QQ)",
	  "lift(lift(g,QQ),ZZ)"
	  },
     "The functions ", TT "lift", " and ", TO "substitute", " are useful to move numbers from one kind of
     coefficient ring to another.",
     EXAMPLE lines ///
	  lift(3.0,ZZ)
	  lift(3.0,QQ)
	  ///,
     PARA{
	  "A continued fraction method is used to lift a real number to a rational number.",
	  },
     EXAMPLE lines ///
	  12/127.
	  lift(oo,QQ)
          ///,
     SeeAlso => {baseRings,liftable
	  -- ,promote
	  }
     }

TEST ///
A = QQ[a..d]
f = (a+1)^2-a^2-2*a
lift(f,ZZ)
lift(lift(f,QQ),ZZ)

lift(0.0 * ii + 3.0, RR)
a = lift(1.34242,RR)
b = promote(a,RR)
///
