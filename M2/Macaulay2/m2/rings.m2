--		Copyright 1994 by Daniel R. Grayson

Ring _ String := (x,s) -> x#s		  -- gets variable from its name

random Ring := (R) -> (
     if R.?random then R.random()
     else error "no method found for item of class Ring"
     )

ZZ _ Ring := (i,R) -> (
     if i === 1 then R#1
     else if i === 0 then R#0
     else i * R#1
     )

poincare Ring := R -> poincare R^1

dim Ring := R -> (
     if R.?dim
     then R.dim
     else error("dimension of ring ", name R, " unknown"))

char Ring := R -> (
     if R.?char then R.char 
     else error("characteristic of ", name R, " unknown"))

generators Ring := R -> {}

vars Ring := R -> (
     g := generators R;
     if R.?vars then R.vars else R.vars =
     map(R^1,,{g}))

numgens Ring := R -> #generators R

document { quote Engine,
     TT "Engine", " -- a key for rings which yields the value ", TT "true", " if this
     ring is supported by the ", TO "engine", "."
     }

ring Thing := x -> (
     if x.?ring then x.ring 
     else if instance(class x,Ring) then class x
     else error "no ring")
ring Type := T -> if T.?ring then T.ring else error "no ring"

document { quote ring,
     TT "ring x", " -- yields the ring associated with ", TT "x", ".",
     BR,
     NOINDENT,
     TT "ring x", " -- yields the ambient ring of a ring element ", TT "x", ".",
     BR,
     NOINDENT,
     TT "ring M", " -- yields the base ring of a module ", TT "M", ".",
     BR,
     NOINDENT,
     TT "ring C", " -- yields the base ring of a chain complex ", TT "C", ".",
     BR,
     NOINDENT,
     TT "ring I", " -- yields the ambient ring of an ideal ", TT "I", ".",
     BR,
     NOINDENT,
     TT "ring p", " -- yields the base ring of a module homomorphism ", TT "p", ".",
     BR,
     NOINDENT,
     TT "ring f", " -- yields the base ring of a map of chain complexes ", TT "f", ".",
     BR,
     NOINDENT,
     TT "ring X", " -- yields the coordinate ring of an affine variety ", TT "X", ".",
     BR,
     NOINDENT,
     TT "ring Z", " -- yields the homogeneous coordinate ring of a projective variety ", TT "Z", ".",
     SEEALSO "Ring"
     }

ambient Ring := R -> error "no ambient ring present"
coefficientRing Ring := R -> error "no coefficient ring present"

document { quote coefficientRing,
     TT "coefficientRing R", " -- yields the coefficient ring of the ring ", TT "R", ".",
     PARA,
     "If ", TT "R", " is a polynomial ring, then the coefficient ring is
     the base ring from which the coefficients are drawn.  If ", TT "R", " is
     constructed from a polynomial ring as a quotient ring or a fraction ring
     or a sequence of such operatinos, then the original coefficient ring
     is returned.",
     EXAMPLE {
	  "coefficientRing(ZZ/101[a][b])",
      	  "ultimate(coefficientRing,ZZ/101[a][b])"
	  },
     }

isCommutative = method()
isCommutative Ring := R -> R.isCommutative
document { quote isCommutative,
     TT "isCommutative R", " -- tells whether the ring R is commutative."
     }

ZZ.isCommutative = true
QQ.isCommutative = true
RR.isCommutative = true

isRing = method()
isRing Thing := R -> false
isRing Ring := R -> true

document { quote isRing,
     TT "isRing x", " -- determines whether x is a ring."
     }

isHomogeneous Ring := R -> (
     R.?isHomogeneous and R.isHomogeneous
     or
     degreeLength R == 0 
     )
