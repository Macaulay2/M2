---------------------
--- Preliminaries  --
---------------------
getIndex = (R,x) -> (
     M := try monoid R else error "expected a polynomial ring or quotient of one";
     if class x =!= R then error "expected an element of the ring";
     x = try baseName x else error "expected a variable of the ring";
     M.index#x)

getIndices = (R,v) -> unique apply(v, a -> getIndex(R,a))

degree (RingElement, RingElement) := (f,x) -> (
     v := getIndex(ring f,x);
     mm := coefficients({v},f);
     (max degrees source mm#0)_0)

------------------------------
-- Elimination of variables --
------------------------------

eliminate = method()

inversePermutation = (p) -> (
     q := new MutableList from toList(0..#p-1);
     scan(0..#p-1, i -> q#(p#i) = i);
     toList q)

eliminate (Ideal, List) := (I,v) -> (
     R := ring I;
     varlist := getIndices(ring I,v);
     others := toList(set(0..numgens R-1) - set varlist);
     perm := join(varlist,others);
     invperm := inversePermutation perm;
     degs := (monoid R).Options.Degrees;
     degs = apply(perm, i -> degs_i);
     R1 := (coefficientRing R)[Variables=>numgens R,MonomialOrder=>ProductOrder{#varlist,#others},
	  Degrees=>degs, MonomialSize=>16];
     toR1 := map(R1,R,apply(invperm,i->R1_i));
     toR := map(R,R1,apply(perm,i->R_i));
     J := toR1 I;
     if isHomogeneous I then
         (cokernel generators J).poincare = poincare cokernel generators I;
     ideal mingens ideal toR selectInSubring(1,generators gb J)
     )

eliminate (Ideal, RingElement) := (I,v) -> eliminate(I,{v})

document { eliminate,
     EXAMPLE {
	  ///needs "eliminate.m2"///,
	  "R = ZZ/101[x,a,b,c,d,Degrees=>{1,1,2,1,2}];",
	  "f = x^2+a*x+b",
	  "g = x^2+c*x+d",
	  "time eliminate(ideal(f,g),x)",
	  "time ideal resultant(f,g,x)",
	  "sylvesterMatrix(f,g,x)",
	  "discriminant(f,x)"
	  },
     SEEALSO "sylvesterMatrix"
     }

-----------------------------------------------
-- Sylvester matrix, resultant, discriminant --
-----------------------------------------------

sylvesterMatrix = method()
sylvesterMatrix(RingElement,RingElement,RingElement) := (f,g,x) -> (
     R := ring f;
     if R =!= ring g then error "expected same ring";
     v := getIndex(R,x);  -- just to check if x is a variable in the polyring R.
     if f == 0 or g == 0 
     then map(R^1,R^1,0)
     else (
       degf := degree(f,x);
       degg := degree(g,x);
       if degf === 0 or degg === 0 
       then matrix {{1_R}}
       else (
         x1 := matrix{{1,x}};
         xfg := transpose symmetricPower(degf + degg - 1, x1);
         xf := symmetricPower(degf-1, x1);
         xg := symmetricPower(degg-1, x1);
	 m := contract(xfg, (f ** xg) | (g ** xf));
         substitute(m, x=>0)))
     )

resultant = method()
resultant(RingElement, RingElement, RingElement) := (f,g,x) -> 
     det sylvesterMatrix(f,g,x)

discriminant = method()
discriminant(RingElement, RingElement) := (f,x) -> resultant(f, diff(x,f), x)

document { resultant,
     TT "resultant(f:RingElement,g:RingElement,x:RingElement) --> RingElement",
     BR,NOINDENT, "  -- yields the Sylvester resultant of f and g with respect to the variable x.",
     PARA,
"The elements 'f' and 'g' should be polynomials in the same ring, and 'x' should be
a variable in that ring.  The result is the determinant of the Sylvester matrix, 
", TT "sylvesterMatrix(f,g,x)", ".  The resultant of f and its derivative with respect to x is the
discriminant, ", TT "discriminant(f,x).",
     EXAMPLE {
	  ///needs "eliminate.m2"///,
	  "R = ZZ[x,a,b,c,d];",
	  "f = x^2+a*x+b",
	  "g = x^2+c*x+d",
	  "resultant(f,g,x)",
	  "sylvesterMatrix(f,g,x)",
	  "discriminant(f,x)"
	  },
     SEEALSO {"sylvesterMatrix", "discriminant", "eliminate"}
     }


TEST ///
needs "eliminate.m2"
R = ZZ/101[a..d]
time I = monomialCurveIdeal(R,{1,3,4})
time eliminate(I,{b})

R = ZZ[a,b,c,d,e]
f1 = a^4 + b*a + c
degree(f1,a)
f2 = a^2 + d*a + e
time sylvesterMatrix(f1,f2,a)
time resultant(f1,f2,a)
time discriminant(f1,a)
f3 = 1_R
time resultant(f1,f3,a)

R = ZZ/32003[a,b,c,d,e]
f1 = a^4 + b*a + c
f2 = a^2 + d*a + e
time resultant(f1,f2,a)
time eliminate(ideal(f1,f2),a)
///
