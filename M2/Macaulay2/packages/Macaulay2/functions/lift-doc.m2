--- status: DRAFT
--- author(s): MES
--- notes: BUG to fix

undocumented {(lift,Matrix,CC,CC), 
     (lift,Matrix,Number), 
     (lift,Matrix,ZZ,ZZ), (lift,Matrix,QQ,ZZ), (lift,Matrix,RRR,ZZ), 
     (lift,Matrix,QQ,QQ), (lift,Matrix,RRR,QQ), (lift,Matrix,CCC,ZZ),
     (lift,Matrix,RRR,RRR), (lift,Matrix,RR,ZZ), (lift,Matrix,CCC,QQ), (lift,Matrix,CC,ZZ), (lift,Matrix,RR,QQ), 
     (lift,Matrix,CCC,RRR), (lift,Matrix,RRR,RR), (lift,Matrix,CC,QQ),
     (lift,Matrix,CCC,CCC), (lift,Matrix,CCC,RR), (lift,Matrix,RR,RR), (lift,Matrix,CCC,CC), (lift,Matrix,CC,RR),
     (lift, ZZ, ZZ),
     (lift, RRR, ZZ),
     (lift, RRR, QQ),
     (lift, RR, ZZ),
     (lift, RRR, RRR),
     (lift, RR, QQ),
     (lift, RRR, RR),
     -- (lift, RR, RRR),
     (lift, RR, RR),
     (lift, QQ, ZZ),
     (lift, QQ, QQ),
     (lift, CCC, ZZ),
     (lift, CCC, QQ),
     (lift, CC, ZZ),
     (lift, CCC, RRR),
     (lift, CC, QQ),
     (lift, CCC, CCC),
     (lift, CCC, RR),
     (lift, CCC, CC),
     (lift, CC, RR),
     (lift, CC, CC),
     (lift, Ideal, ZZ),
     (lift, Ideal, QQ)
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
	  "R" => Ring
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
          substitute(3,RR)
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
     EXAMPLE lines ///
	  setPrecision 15
          z = lift(.2341124,RRR)
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
a = lift(1.34242,RRR)
b = promote(a,RR)
///
