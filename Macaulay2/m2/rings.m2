--		Copyright 1994 by Daniel R. Grayson

Ring _ String := (x,s) -> x#s		  -- gets variable from its name

ZZ _ Ring := (i,R) -> (
     if i === 1 then R#1
     else if i === 0 then R#0
     else i * R#1
     )

use Ring := R -> (
     if not R.?use then error("no 'use' method for ", name R);
     R.use R; 
     R)

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
     TT "ring x", " -- yields the ring associated with x.",
     PARA,
     "ring x -- yields the ambient ring of a ring element x",
     PARA,
     "ring M -- yields the base ring of a module M.",
     PARA,
     "ring C -- yields the base ring of a chain complex C.",
     PARA,
     "ring I -- yields the ambient ring of an ideal I.",
     PARA,
     "ring p -- yields the base ring of a module homomorphism p.",
     PARA,
     "ring f -- yields the base ring of a map of chain complexes f.",
     PARA,
     "See also ", TO "Ring", "."
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
     EXAMPLE "coefficientRing(ZZ/101[a][b])",
     EXAMPLE "ultimate(coefficientRing,ZZ/101[a][b])"
     }

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
