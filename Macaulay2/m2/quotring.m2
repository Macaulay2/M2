--		Copyright 1996 by Daniel R. Grayson and Michael E. Stillman


QuotientRing = new Type of EngineRing

document { quote QuotientRing,
     TT "QuotientRing", " -- the class of all quotient rings.",
     PARA,
     SEEALSO {quote /, Ring, Ideal}
     }

isQuotientRing = method()
isQuotientRing Ring := R -> false
isQuotientRing QuotientRing := R -> true
coefficientRing QuotientRing := R -> coefficientRing R.baseRings#-1
options QuotientRing := R -> options R.baseRings#-1

isQuotientOf = method()
isQuotientOf(Ring,Ring) := (S,R) -> S === R
isQuotientOf(QuotientRing,Ring) := (S,R) -> (
     S === R or isQuotientOf(S.baseRings#-1,R)
     )
document { quote isQuotientOf,
     TT "isQuotientOf(S,R)", " -- tells whether S is a quotient ring of R."
     }

document { quote isQuotientRing,
     TT "isQuotientRing R", " -- tells whether R is provided as a quotient
     ring."
     }

TEST "
assert( ZZ/2 === ZZ/(4,6) )
R = ZZ/101[t]
"

degreeLength QuotientRing := S -> degreeLength S.baseRings#-1
vars QuotientRing := S -> (
     if S.?vars 
     then S.vars 
     else S.vars = matrix table (1, numgens S, (i,j) -> S_j)
     )
numgens QuotientRing := S -> numgens S.baseRings#-1

pretty := relns -> (
     s := toSequence flatten entries relns;
     if #s === 1 then s = first s;
     s)

name QuotientRing := S -> (
     concatenate( name last S.baseRings, "/", name pretty S.relations)
     )

random QuotientRing := S -> (
     if S.baseRings === {ZZ} then (random char S)_S
     else notImplemented())

expression QuotientRing := S -> (
     if S.?name then S.name
     else new Divide from { 
	  expression last S.baseRings,  
	  expression pretty S.relations 
	  }
     )
net QuotientRing := S -> net expression S
dim QuotientRing := S -> if S.?dim then S.dim else S.dim = dim S^1

ambient PolynomialRing := R -> R
ambient QuotientRing := R -> R.baseRings#-1

isHomogeneous QuotientRing := R -> (
     if R.?isHomogeneous then R.isHomogeneous 
     else R.isHomogeneous = (
	  degreeLength R == 0
	  or
	  isHomogeneous ambient R and isHomogeneous R.relations
	  )
     )

Ring / Module := (R,I) -> (
     if ambient I != R^1 or I.?relations
     then error ("expected ", name I, " to be an ideal of ", name R);
     R / ideal I)

savedQuotients := new MutableHashTable

Ring / Ideal := (R,I) -> if I == 0 then R else (
     if R === ZZ then (
	  gensI := I.generators ** R;
	  gensgbI := generators gb gensI;
	  n := if gensgbI == 0 then 0 else gensgbI_(0,0);
	  if n < 0 then n = -n;
	  if n === 0 then ZZ
	  else if savedQuotients#?n 
	  then savedQuotients#n
	  else (
	       if n > 32767 then error "large characteristics not implemented yet";
	       if n > 1 and not isPrime n
	       then error "ZZ/n not implemented yet for composite n";
	       G := degreesMonoid 0;
	       sendgg(ggPush n, ggPush G, ggcharp);
	       S := new QuotientRing from newHandle();
	       S.relations = gensI;
	       S.ConvertToExpression = R.ConvertToExpression;
	       S.isCommutative = R.isCommutative;
	       S.presentation = gensgbI;
	       S.char = n;
	       if n === 1 then S.dim = -1 else if n === 0 then S.dim = 1 else S.dim = 0;
	       S.ConvertToExpression = ConvertApply(expression, ConvertInteger);
	       expression S := x -> convert(
		    S.ConvertToExpression, sendgg(ggPush x, ggtonet)
		    );
	       S.frac = S;		  -- ZZ/n with n PRIME!
	       S.baseRings = {ZZ};
	       savedQuotients#n = S;
	       S))
     else (
	  error "can't form quotient of this ring"
	  )
     )

document { (quote /, Ring, Ideal),
     TT "R/I", " -- form a quotient ring.",
     PARA,
     "Here ", TT "I", " may be: an element of ", TT "R", "; a sequence of elements of
     ", TT "R", "; or a submodule of ", TT "R^1", ".",
     PARA,
     "The names of the variables are assigned values in the new quotient ring
     by automatically running ", TT "use R", ", unless R has a name,
     or one of the rings R is a quotient ring of has a name.",
     PARA,
     "Quotient rings are bulky objects, because they contain a Groebner basis
     for their ideals, so only quotients of ", TT "ZZ", " are remembered
     forever.  Typically the ring created by ", TT "R/I", " will
     be a brand new ring, and its elements will be incompatible with the
     elements of previously created quotient rings for the same ideal.",
     PARA,
     EXAMPLE {
	  "ZZ/2 === ZZ/(4,6)",
      	  "R = ZZ/101[t]",
      	  "R/t === R/t",
	  },
     PARA,
     SEEALSO {"QuotientRing", "use"}
     }

predecessors := method()
predecessors Ring := R -> {R}
predecessors QuotientRing := R -> append(predecessors last R.baseRings, R)

EngineRing / Ideal := (R,I) -> if I == 0 then R else (
     -- recall that ZZ in NOT an engine ring.
     A := R;
     while class A === QuotientRing do A = A.baseRings#-1;
     gensI := I.generators ** R;
     gensgbI := generators gb gensI;
     sendgg(ggPush gensgbI, ggqring);
     S := new QuotientRing from newHandle();
     S.relations = gensI;
     S.ConvertToExpression = R.ConvertToExpression;
     S.isCommutative = R.isCommutative;
     S.baseRings = append(R.baseRings,R);
     if R.?generatorSymbols then S.generatorSymbols = R.generatorSymbols;
     if R.?generatorExpressions then (
	  S.generatorExpressions = R.generatorExpressions;
	  scan(R.generatorExpressions, x -> (
		    a := if class x === Symbol then string x else name x;
		    S#a = promote(R#a,S);
		    ));
	  );
     S.use = x -> (
	  try monoid S;
	  if S.?monoid then (
	       M := S.monoid;
	       M + M := (m,n) -> S#1 * m + S#1 * n;
	       M - M := (m,n) -> S#1 * m - S#1 * n;
	       - M := m -> (- S#1) * m;
	       scan(S.baseRings, A -> (
		    A + M := (i,m) -> promote(i, S) + m;
		    M + A := (m,i) -> m + promote(i, S);
		    A - M := (i,m) -> promote(i, S) - m;
		    M - A := (m,i) -> m - promote(i, S);
		    A * M := (i,m) -> promote(i, S) * m;
		    M * A := (m,i) -> m * promote(i, S);
		    ));
	       );
	  );
     S)

Ring / ZZ := Ring / RingElement := Ring / List := Ring / Sequence := (R,f) -> R / ideal f

presentation QuotientRing := R -> (
     if R.?presentation then R.presentation else R.presentation = (
	  sendgg (ggPush R, gggetideal);
	  getMatrix ultimate(ambient,R)
	  )
     )

presentation PolynomialRing := R -> map(R^1,R^0,0)

presentation(QuotientRing,QuotientRing) := 
presentation(PolynomialRing,QuotientRing) := 
presentation(QuotientRing,PolynomialRing) := 
presentation(PolynomialRing,PolynomialRing) := (R,S) -> (
     if not isQuotientOf(S,R) then error "expected ring and a quotient ring of it";
     v := map(R^1,R^0,0);
     while S =!= R do (
	  v = v | lift(S.relations,R);
	  S = S.baseRings#-1;
	  );
     v)

TEST "
R = ZZ/101[a..d]
f=1+a+b+c+d
assert(size f == 5)
S = R/a
assert(size promote(f,S) == 4)
"

codim PolynomialRing := R -> 0
codim QuotientRing := (R) -> codim cokernel presentation R

hilbertSeries QuotientRing := options -> (S) -> (
     hilbertSeries(coker presentation S,options)
     )

monoid QuotientRing := (S) -> (
     if S.?monoid then S.monoid
     else (
	  R := ultimate(ambient,S);
	  S.monoid = monoid R
	  )
     )

degreesRing QuotientRing := (S) -> (
     if S.?degreesRing then S.degreesRing 
     else (
	  R := ultimate(ambient,S);
	  S.degreesRing = degreesRing R
	  )
     )

QuotientRing_String := (S,s) -> (
     if S#?s then S#s
     else (
	  R := ultimate(ambient, S);
	  S#s = promote(R_s, S)
	  )
     )

generators QuotientRing := (S) -> (
     if S.?generators 
     then S.generators
     else S.generators = (
	  A := ultimate(ambient,S);
	  S.generators = apply(generators A, m -> promote(m,S))))

char QuotientRing := (S) -> (
     if S.?char then S.char
     else S.char = (
	  R := ultimate(ambient,S);
	  -- eventually we'll have to compute it correctly
	  char R))
