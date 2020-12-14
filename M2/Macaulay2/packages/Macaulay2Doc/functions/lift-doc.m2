--- status: DRAFT
--- author(s): MES, DRG
--- notes: BUG to fix

-*
-- TODO
lift(Matrix,type of CC_*,type of CC_*)
lift(Matrix,type of RR_*,type of RR_*)
lift(Module,type of InexactNumber')
lift(Module,type of InexactNumber)
lift(Module,type of Number)
lift(Module,type of RingElement)
lift(MutableMatrix,type of InexactNumber')
lift(MutableMatrix,type of InexactNumber)
lift(MutableMatrix,type of Number)
lift(MutableMatrix,type of RingElement)
*-

document { 
     Key => {lift,
	  (lift,Ideal,RingElement),[lift,Verify],
	  (lift,Matrix,RingElement),
	  (lift, CC, QQ),
	  (lift, CC, RR_*),
	  (lift, CC, ZZ),
	  (lift, Ideal, QQ),
	  (lift, Ideal, ZZ),
	  (lift,Matrix,CC_*,QQ),
	  (lift,Matrix,CC_*,RR_*),
	  (lift,Matrix,CC_*,ZZ),
	  (lift,Matrix,Number),
	  (lift,Matrix,QQ,QQ),
	  (lift,Matrix,QQ,ZZ),
	  (lift,Matrix,RR_*,QQ),
	  (lift,Matrix,RR_*,ZZ),
	  (lift,Matrix,ZZ,ZZ),
	  (lift, QQ, QQ),
	  (lift, QQ, ZZ),
	  (lift, RR, QQ),
	  (lift, RR, ZZ),
	  (lift, ZZ, ZZ),
	  (symbol ^, RingElement, Ring),
	  (symbol ^, Number, Ring),
	  (symbol ^, RingElement, RingFamily),
	  (symbol ^, Number, RingFamily),
	  (symbol ^, Constant, Ring),
	  (symbol ^, Constant, RingFamily)
	  },
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
	       TO2(Matrix, "matrix"), ", over the ring ", TT "R"}
	  },
     PARA {
	  "(Disambiguation: for division of matrices, which is thought of as lifting
	  one homomorphism over another, see instead ", TO (symbol //,Matrix, Matrix), ".  For lifting a map between modules to a map 
	  between their free resolutions, see ", TO extend, ".)"
	  },
     PARA {
	  "The ring ", TT "R", " should be one of the base rings 
	  associated with the ring of ", TT "f",".  An error is raised if
	  ", TT "f", " cannot be lifted to ", TT "R", "."
	  },
     PARA{ "The first example is lifting from the fraction field of R to R." },
     EXAMPLE lines ///
     lift(4/2,ZZ)
     R = ZZ[x];
     f = ((x+1)^3*(x+4))/((x+4)*(x+1))
     lift(f,R)
     ///,
     PARA{
	  "Another use of lift is to take polynomials in a quotient ring
	  and lift them to the polynomial ring.",
	  },
     EXAMPLE lines ///
     A = QQ[a..d];
     B = A/(a^2-b,c^2-d-a-3);
     f = c^5
     lift(f,A)
     jf = jacobian ideal f
     lift(jf,A)
     ///,
     PARA{ "Elements may be lifted to any base ring, if such a lift exists." },
     EXAMPLE lines ///
     use B;
     g = (a^2+2*a-3)-(a+1)^2
     lift(g,A)
     lift(g,QQ)
     lift(lift(g,QQ),ZZ)
     ///,
     PARA {
	  "The functions ", TT "lift", " and ", TO "substitute", " are useful to move numbers from one kind of
	  coefficient ring to another."
	  },
     EXAMPLE lines ///
     lift(3.0,ZZ)
     lift(3.0,QQ)
     ///,
     PARA{
	  "A continued fraction method is used to lift a real number to a rational number, whereas
	  ", TO "promote", " uses the internal binary representation.",
	  },
     EXAMPLE lines ///
     lift(123/2341.,QQ)
     promote(123/2341.,QQ)
     factor oo
     ///,
     PARA { "For numbers and ring elements, an alternate syntax with ", TO "^", " is available,
	  analogous to the use of ", TO "_", " for ", TO "promote", "." },
     EXAMPLE lines ///
     .0001^QQ
     .0001_QQ
     ///,
     SeeAlso => {baseRings,liftable,promote}
     }

TEST ///
A = QQ[a..d]
f = (a+1)^2-a^2-2*a
lift(f,ZZ)
lift(lift(f,QQ),ZZ)

lift(0.0 * ii + 3.0, RR)
///
