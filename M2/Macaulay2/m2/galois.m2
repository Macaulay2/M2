--		Copyright 1996 by Daniel R. Grayson

GaloisField = new Type of EngineRing

ambient GaloisField := R -> R.baseRings#-1
coefficientRing GaloisField := R -> coefficientRing R.baseRings#-1

document { quote PrimitiveElement,
     TT "PrimitiveElement => g", " -- an option used with ", TO "GF", ".",
     PARA,
     "The value can be a ring element providing a primitive element, or the
     symbol ", TO "FindOne", " (the default) which specifies that
     ", TO "GF", " should search for a primitive element."
     }

document { quote FindOne,
     TT "FindOne", " -- a value for the option ", TO "PrimitiveElement", "
     to ", TO "GF", " which specifies that ", TO "GF", " should search 
     for a primitive element."
     }

document { quote Variable,
     TT "Variable => x", " -- an option used with ", TO "GF", "."
     }

document { quote GaloisField,
     TT "GaloisField", " -- the class of all Galois fields.",
     PARA,
     "A Galois field is a finite field implemented in the ", TO "engine", ",
     and created with ", TO "GF", ".",
     "Keys used:",
     MENU {
	  TO "order"
	  }
     }


GF = method (
     Options => { 
	  PrimitiveElement => FindOne,
	  Variable => null
	  }
     )

document { quote GF,
     TT "GF R", " -- make a Galois field from a quotient ring R which happens
     to be isomorphic to a finite field.",
     BR,NOINDENT,
     TT "GF(p,n)", " -- make a Galois field with ", TT "p^n", " elements, where 
     ", TT "p", " is a prime.",
     BR,NOINDENT,
     TT "GF(q)", " -- make a Galois field with ", TT "q", " elements, where 
     ", TT "q", " is a power of a prime.",
     PARA,
     "Options:",
     MENU {
	  (TO "PrimitiveElement", " => g -- provide a primitive element"),
     	  (TO "Variable", " => x -- provide a variable name")
	  },
     PARA,
     SEEALSO ("GaloisField")
     }

lastp := 2

TEST "
R=ZZ/2[t]
assert isPrime (t^2+t+1)
assert (not isPrime (t^2+1))
"

unpack := (S,cont) -> (			  -- a quotient ring
     if class S =!= QuotientRing
     then error("expected ",name S," to be a quotient ring");
     R := ultimate(ambient, S);			  -- original poly ring
     if R === ZZ then (
	  if isField S then (
	       R = S[local t];
	       S = R/t;
	       )
     	  else error("expected ",name S," to be a field")
	  );
     if class R =!= PolynomialRing
     or numgens R =!= 1
     then error("expected ",name R," to be a polynomial ring in one variable");
     if degreeLength R =!= 1
     then error("expected ",name R," to be a simply graded polynomial ring");
     p := char R;
     if coefficientRing R =!= ZZ/p or (p =!= lastp and not isPrime p)
     then error("expected ",name R," to be a polynomial ring over a finite prime field");
     lastp = p;
     I := presentation S;			  -- a Matrix, sigh
     if rank source I =!= 1
     then error("expected ",name S," to be a quotient ring by a principal ideal");
     f := I_(0,0);
     n := first degree f;
     cont(R,p,n,f))

isPrimitive = (g) -> (
     g != 0 and
     unpack(ring g,
	  (R,p,n,f) -> (
     	       q := p^n;
     	       all(factor (q-1), v -> 1 != g ^ ((q-1)//v#0))
     	       )))

document { quote isPrimitive,
     TT "isPrimitive g", " -- determines whether ", TT "g", " is a primitive
     element of a finite field.",
     PARA,
     EXAMPLE "R = ZZ/5[t]/(t^2+t+1)",
     EXAMPLE "isPrimitive t",
     EXAMPLE "isPrimitive (t-1)"
     }

TEST "
R = ZZ/5[t]/(t^2+t+1)
use R
assert (not isPrimitive t)
assert isPrimitive (t-1)
assert (not isPrimitive 0_R)
"

GF(ZZ,ZZ) := (p,n,options) -> (
     x := if options.Variable === null then local x else baseName options.Variable;
     R := ZZ/p[x,MonomialSize => 16];
     t := R_0;
     while ( f := t^n + sum(n, i-> random p * t^i); not isPrime f) do ();
     GF(R/f,options,Variable => x)
     )

GF(ZZ) := (q,options) -> (
     factors := factor q;
     if #factors =!= 1 or factors#0#0 === -1
     then error "expected a power of a prime";
     GF(factors#0#0,factors#0#1,options))

GF(Ring) := (S,options) -> unpack(S, (R,p,n,f) -> (
     if not isPrime f
     then error("expected ",name S," to be a quotient ring by an irreducible polynomial");
     g := options.PrimitiveElement;
     if g === FindOne then (
	  t := S_0;
	  if isPrimitive t 
	  then g = t
	  else while ( 
	       g = sum(n, i -> random p * t^i); 
	       not isPrimitive g
	       ) do ();
	  );
     xx := options.Variable;
     if xx =!= null
     then xx = baseName xx
     else (
	  if g =!= null and g == S_0 
	  then xx = S.syms#0
	  else xx = local x;
	  );
     if g === null
     then (
     	  sendgg(ggPush S, ggGF);
     	  F := new GaloisField from newHandle();
     	  F.ConvertToExpression = R.ConvertToExpression;
	  )
     else (
	  d := p^n-1;
	  if ring g != S
	  then error "expected primitive element in the right ring";
	  if not isPrimitive g
	  then error "expected ring element to be primitive";
     	  sendgg (ggPush g, ggGF);
     	  F = new GaloisField from newHandle();
	  F.ConvertToExpression = ConvertApply(
	       i -> (
		    if i === 0 then expression 0 
		    else if i === d then expression 1
		    else (expression xx)^i
		    ), 
	       ConvertInteger);
	  name F := h -> name expression h;
	  net F := h -> net expression h;
	  );
     expression F := t -> convert(
	  F.ConvertToExpression, sendgg(ggPush t, ggtonet)
	  );
     F.degreeLength = degreeLength R;
     F.char = p;
     F.frac = F;
     F.syms = {xx};
     F.generators = {F_0};
     baseName F := y -> (
	  if F_0 == y then xx else error "expected a generator"
	  );
     F.order = p^n;
     F.baseRings = append(S.baseRings,S);
     F / F := (f,g) -> f // g;		  -- it is a field
     use F;
     F))

document { quote order,
     TT "order", " -- used as a key inside finite fields under which is
     stored the number of elements in the field.",
     PARA,
     SEEALSO "GaloisField"
     }

random GaloisField := F -> (
     i := random F.order;
     if i === 0 then 0_F else F.generators#0^i
     )

TEST "
L = ZZ/5[t]
M = L/(t^2+t+1)
use M
G = GF(M,Variable => v,PrimitiveElement => t-1)
assert( lift(v,M) + 1 == lift (v+1,M) )
assert( lift(v^6,M) == (lift(v,M))^6 )
assert( lift(v^7,M) == (lift(v,M))^7 )
"

dim GaloisField := R -> 0

isField GaloisField := F -> true
isField QuotientRing := R -> ambient R === ZZ and isPrime char R
isField Ring := R -> R === QQ

document { quote isField,
     TT "isField R", " -- tells whether a ring is a field.",
     PARA,
     "No computation is done -- the question is whether the ring was
     constructed as a field."
     }

isAffineRing = method()
isAffineRing Ring := R -> isField R
isAffineRing PolynomialRing := R -> (
     not (options R)#SkewCommutative and not (options R)#Inverses and
     isAffineRing coefficientRing R
     )
isAffineRing QuotientRing := R -> isField R or isAffineRing ambient R

document { quote isAffineRing,
     TT "isAffineRing R", " -- tells whether a ring is an affine ring.",
     PARA,
     "An affine ring is a quotient of a polynomial ring over a field."
     }
