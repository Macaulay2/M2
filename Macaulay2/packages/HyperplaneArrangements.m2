-- Feb 11 2007: beginnings of hyperplane arrangements package
-- with Greg Smith

newPackage(
     "HyperplaneArrangements",
     Version => "0.2",
     Date => "24 May 2007",
     Authors => {
	  {Name => "Graham Denham", HomePage => "http://www.math.uwo.ca/~gdenham/"},
	  {Name => "Gregory G. Smith", Email => "ggsmith@mast.queensu.ca", HomePage => "http://www.mast.queensu.ca/~ggsmith"}
	  },
     Headline => "hyperplane arrangements",
     DebuggingMode => true
     )

export {Arrangement, arrangement, -- compress, trim, coefficients,
--     euler, poincare, cone, rank, ring, matrix,
     deletion, orlikSolomon, HypAtInfinity, typeA, typeB, typeD, graphic, 
     Flat, flat, flats, tolist, closure, meet, vee, subArrangement, 
     restriction, arrangementSum, EPY, der, crit, omega, HS}

Arrangement = new Type of HashTable
Arrangement.synonym = "hyperplane arrangement"
Arrangement.GlobalAssignHook = globalAssignFunction
Arrangement.GlobalReleaseHook = globalReleaseFunction
Arrangement.AfterPrint = A -> (
     << endl;
     << concatenate(interpreterDepth:"o") << lineNumber << " : Hyperplane Arrangement "
     << endl;
     )
net Arrangement := A -> if ReverseDictionary#?A then toString ReverseDictionary#A else net expression A
expression Arrangement := A -> new RowExpression from { A.hyperplanes }
describe Arrangement := A -> net A.hyperplanes

arrangement = method(TypicalValue => Arrangement)
arrangement (List,Ring) := Arrangement => (L,R) -> (
     if #L > 0 and ring L#0 != R then (
	  f := map(R, ring L#0);
	  A := L / f)
     else A = L;
     new Arrangement from {
	  symbol ring => R,
	  symbol hyperplanes => A,
	  symbol cache => new CacheTable
	  })
arrangement List := Arrangement => L -> arrangement(L, ring L#0)
arrangement (Arrangement,Ring) := Arrangement => (A,R) -> arrangement(A.hyperplanes,R)

ring Arrangement := Ring => A -> A.ring

tolist = method(TypicalValue=>List);  -- prefer to overload toList; see join
tolist Arrangement := List => A -> A.hyperplanes

matrix Arrangement := Matrix => options -> A -> matrix {A.hyperplanes}

coefficients Arrangement := Matrix => options -> A -> (
     if (A.hyperplanes == {}) then 0 else jacobian matrix A)

rank Arrangement := A -> 
     if (A.hyperplanes == {}) then 0 else rank coefficients A

trim Arrangement := Arrangement => options -> A -> 
     if (A.hyperplanes == {}) then A else (
     	  seen := new MutableHashTable;
     	  L := select(A.hyperplanes, h -> if seen#?ih or h == 0 
	       then false else seen#h = true);
	  arrangement(L, ring A))

compress Arrangement := Arrangement => A -> 
     if (A.hyperplanes == {}) then A else (
	  L := select(A.hyperplanes, h -> h != 0);
	  arrangement(L, ring A))

-- equality testing

Arrangement == Arrangement := (A,B) -> (
     (A.hyperplanes == B.hyperplanes) and (A.ring == B.ring))

-- deletion; restriction is a special case of res. to a flat, so comes 
-- later

deletion = method(TypicalValue => Arrangement)
deletion (Arrangement,RingElement) := Arrangement => (A,h) -> (
     select(A.hyperplanes,i->(i != h)));

cone (Arrangement,RingElement) := Arrangement => (A,h) -> (
     arrangement ((apply(A.hyperplanes,i->homogenize(i,h))) | {h}));

cone (Arrangement,Symbol) := Arrangement => (A,h) -> (
     R := ring A;
     S := (coefficientRing R)[h];
     T := tensor(R,S,Degrees=>toList ((numgens(R)+numgens(S)):1));
     f := map(T,S);
     cone (arrangement(A,T),f S_0));

partial := m -> (
     E := ring m;
     sum first entries compress diff(vars E,m));

monomialSubIdeal := I -> (  -- note: add options (See SP's code)
     R := ring I;
     K := I;
     J := ideal(1_R);
     while (not isMonomialIdeal K) do (
	  J = ideal leadTerm gens gb K;
	  K = intersect(I,J));
     ideal mingens K);

-- the orlikSolomon method expects a central arrangement.  
--
-- It returns an ideal I with OS = E/I, where E is the ring of I
-- and OS is the (central) Orlik-Solomon algebra.
--
-- the same ideal defines the cohomology ring of the projective 
-- complement, but in a subalgebra of E.
--
-- Since we can't construct this in M2, the option Projective 
-- returns a larger ideal I' so that E/I' is the cohomology ring
-- of the projective complement, written in coordinates that put
-- a hyperplane H_j at infinity.
--
-- not clear this is the best...

orlikSolomon = method(TypicalValue => Ideal, 
                      Options => {Projective => false, HypAtInfinity => 0});

orlikSolomon (Arrangement,Ring) := Ideal => o -> (A,E) -> (
     n := #A.hyperplanes;
     e := symbol e;
     Ep := coefficientRing(ring A)[e_1..e_n,SkewCommutative=>true];
     C := substitute(syz coefficients A,Ep);
     M := monomialSubIdeal( ideal( (vars Ep) * C));
     f := map(E,Ep,vars E);
     I := ideal append( apply(flatten entries gens f M, r -> partial r),0_E);
     if o.Projective then trim I+ideal(E_(o.HypAtInfinity)) else trim I);

-- remark, above, characteristic of E does not need to match A.

orlikSolomon (Arrangement,Symbol) := Ideal => o -> (A,e) -> (
     n := #A.hyperplanes;
     E := coefficientRing(ring A)[e_1..e_n,SkewCommutative=>true];
     orlikSolomon(A,E,o));

orlikSolomon (Arrangement) := Ideal => o -> (A) -> (
     e := symbol e;
     orlikSolomon(A,e,o));

--  can't forward options, since existing method doesn't have options.
poincare (Arrangement) := RingElement => A -> (
     I := orlikSolomon A;
     numerator reduceHilbert hilbertSeries ((ring I)/I));

-- Euler characteristic of (proj) complement

euler (Arrangement) := ZZ => A -> (
     I := orlikSolomon(A,Projective=>true);
     f := numerator reduceHilbert hilbertSeries ((ring I)/I);
     sub(f,{(ring f)_0 => -1}));

-- euler(Flat) coming later

-- some constructions of Coxeter type
-- adjusted to allow choice of coefficient ring: following the defaults,
-- the OS algebra would be constructed over ZZ, which is too slow.
-- default coefficients are now QQ, but this can be adjusted.

typeA = method(TypicalValue => Arrangement)
typeA (ZZ,PolynomialRing) := Arrangement => (n,R) -> (
     arrangement flatten apply(n,i->apply(toList(i+1..n),j->R_i-R_j)));

typeA (ZZ,Ring) := Arrangement => (n,k) -> (
     x := symbol x;
     R := k[x_1..x_(n+1)];
     typeA(n,R));

typeA (ZZ) := Arrangement => n -> typeA(n,QQ);

typeD = method(TypicalValue => Arrangement)
typeD (ZZ,PolynomialRing) := Arrangement => (n,R) -> (
     arrangement flatten apply(n-1,i->(
	         flatten apply(toList(i+1..n-1),(j->{R_i-R_j,R_i+R_j})))));

typeD (ZZ,Ring) := Arrangement => (n,k) -> (
     x := symbol x;
     R := k[x_1..x_n];
     typeD(n,R));

typeD (ZZ) := Arrangement => n -> typeD(n,QQ);

typeB = method(TypicalValue => Arrangement)
typeB (ZZ,PolynomialRing) := Arrangement => (n,R) -> (
     arrangement ( (tolist(typeD(n,R))) |apply(n,i->R_i)));

typeB (ZZ,Ring) := Arrangement => (n,k) -> (
     x := symbol x;
     R := k[x_1..x_n];
     typeB(n,R));

typeB (ZZ) := Arrangement => n -> typeB(n,QQ);

-- construct a graphic arrangement, from a graph given by a list
-- of edges.  Assume vertices are integers 1..n

graphic = method(TypicalValue => Arrangement)
graphic (List,PolynomialRing) := Arrangement => (G,R) -> 
     arrangement (G/(e->(R_(e_1-1)-R_(e_0-1))))
     
graphic (List,Ring) := Arrangement => (G,k) -> (
     n := max flatten G;
     x := symbol x;
     R := k[x_1..x_n];
     graphic(G,R));

graphic (List) := Arrangement => G -> (
     graphic(G,QQ));

-- intersection lattice and flats:

Flat = new Type of HashTable
Flat.synonym = "intersection of hyperplane(s)"
Flat.AfterPrint = F -> (
     << endl;
     <<  concatenate(interpreterDepth:"o") << lineNumber << " : Flat of " << F.arrangement
     << endl;
     )

net Flat := F -> net F.flat
expression Flat := (F) -> new Holder from { F.flat }
 
flat = method(TypicalValue => Flat)

flat (Arrangement,List) := Flat => (A,F) ->
new Flat from {
     symbol flat => F,
     symbol arrangement => A,
     symbol cache => new CacheTable
}

arrangement Flat := Arrangement => F -> (   -- get arrangement of a flat F
     F.arrangement);

euler (Flat) := ZZ => F -> (
     euler subArrangement F);  -- aka beta invariant

tolist Flat := List => F -> (
     F.flat);

closure = method(TypicalValue => Flat)  
closure (Arrangement,Ideal) := Flat => (A,I) -> (
     flat(A,positions(A.hyperplanes,h -> h % gb I == 0)));

closure (Arrangement,List) := Flat => (A,S) -> (
     closure (A,ideal (A.hyperplanes_S|{0_(ring A)})));   -- ugly hack for empty list

closure (Arrangement,Arrangement) := Flat => (A,B) -> (
     closure (A,ideal B));

-- lattice operations  

meet = method(TypicalValue => Flat)
meet (Flat,Flat) := Flat => (F,G) -> (
     A := arrangement F;
     if (A =!= arrangement G) then error "need the same arrangement"; 
     flat(A,select((tolist F),i->member(i,tolist G))));

-- join would be better; see tolist

vee = method(TypicalValue => Flat)
vee (Flat,Flat) := Flat => (F,G) -> (
     A := arrangement F;
     if (A =!= arrangement G) then error "need the same arrangement"; 
     closure(A,(tolist F)|(tolist G)));

Flat | Flat := Flat => vee  
Flat ^ Flat := Flat => meet  -- ooh, cool.  But note L_1^L_2 isn't L_1^(L_2) !

subArrangement = method(TypicalValue => Arrangement)
subArrangement (Flat) := Arrangement => (F) -> (
     A := arrangement F;
     arrangement(A.hyperplanes_(tolist F)));

-- the next version is redundant, but I'm putting it in 
-- in case users want to use the usual notation

subArrangement (Arrangement,Flat) := Arrangement => (A,F) -> (
     if (A =!= arrangement F) then error "not a flat of the arrangement";
     subArrangement F);

Arrangement _ Flat := Arrangement => subArrangement

-- restriction will return a (i) multiarrangement with (ii) natural
-- coordinate ring; maybe not what everyone expects
-- empty flat needs special treatment; better style possible here, Greg?

restriction = method(TypicalValue => Arrangement)
restriction (Flat) := Arrangement => (F) -> (
     A := arrangement F;
     R := ring A;
     compress arrangement(A,R/(ideal ((tolist A)_(tolist F)|{0_R}))));

restriction (Arrangement,Flat) := Arrangement => (A,F) -> (
     if (A =!= arrangement F) then error "not a flat of the arrangement";
     restriction F);

Arrangement ^ Flat := Arrangement => restriction

-- in the sense of matroid contraction
-- needs some error checking?

restriction (Arrangement,RingElement) := Arrangement => (A,h) -> (
     arrangement(A,(ring A)/(ideal h)));

restriction (Arrangement,Ideal) := Arrangement => (A,I) -> (
     arrangement(A,(ring A)/I));

rank (Flat) := ZZ => F -> (rank subArrangement F);

flats = method(TypicalValue => List)
flats (ZZ,Arrangement) := List => (j,A) -> (
     I := orlikSolomon A;
     OS := (ring I)/I;
     L := flatten entries basis(j,OS);
     unique(L/indices/(S->closure(A,S))));

flats (Arrangement) := List => A -> (
     apply(rank A,j->flats(j,A)));

-- direct sum of two arrangements  ( can't overload "directSum" or "tensor")

arrangementSum = method(TypicalValue => Arrangement)
arrangementSum (Arrangement, Arrangement) := Arrangement => (A,B) -> (
     R := ring A; S := ring B;
     RS := tensor(R,S,Degrees => toList ((numgens(R)+numgens(S)):1));
     f := map(RS,R); g := map(RS,S);
     (A.hyperplanes/f)|(B.hyperplanes/g));

-- should have change of rings here too, but e.g. ZZ[t]**QQ not implemented

Arrangement ** Arrangement := Arrangement => arrangementSum

symExt= (m,R) ->(
     if (not(isPolynomialRing(R))) then error "expected a polynomial ring or an exterior algebra";
     if (numgens R != numgens ring m) then error "the given ring has a wrong number of variables";
     ev := map(R,ring m,vars R);
     mt := transpose jacobian m;
     jn := gens kernel mt;
     q  := vars(ring m)**id_(target m);
     n  := ev(q*jn))

-- EPY module, formerly called FA

EPY = method(TypicalValue => Module);

EPY (Ideal,Ring) := Module => (j, R) -> (
     modT := (ring j)^1/(j*(ring j^1));
     F := res(prune modT, LengthLimit=>3);
     g := transpose F.dd_2;
     G := res(coker g,LengthLimit=>4);
     coker symExt(G.dd_4, R));

EPY (Ideal) := Module => (j) -> (
     n := numgens ring j;
     f := symbol f;
     R := coefficientRing(ring j)[X_1..X_n];
     EPY(j, R));

EPY (Arrangement) := Module => A -> EPY orlikSolomon A;
EPY (Arrangement,Ring) := Module => (A,R) -> EPY(orlikSolomon A, R);

-- add exceptionals, complex refl groups?

-- module of derivations;  needs adjustment if ring of A is not polynomial.
-- returns matrix whose image is module of derivations

der = method(TypicalValue => Matrix);
der (Arrangement) := Matrix => A -> (
     R := ring A;
     n := numgens R;
     I := ideal product A.hyperplanes;  -- defining polynomial
     J := map(R^1,R^n, transpose jacobian I);
     P := map(R^1/module (intersect(ideal image J,I)),R^1,1);
     mingens ker(P*J));

-- critical set ideal: internal use only.

crit = method(TypicalValue => Ideal);
crit (Arrangement) := Ideal => A -> (
     R := ring A; Q := product A.hyperplanes;
     m := #(A.hyperplanes);
     a := symbol a;
     S := R[a_1..a_m];
     v := transpose(matrix{apply(m,i->S_i*Q//A.hyperplanes_i)});
     I := ideal flatten entries ((coefficients A)*v);
     f := map(S,R);
     I:(f Q));

omega = method(TypicalValue => Matrix);  -- basis for dual to free Der
omega (Arrangement) := Matrix => A -> (
     P := der A;
     R := ring P;
     F := frac R;
     transpose inverse(map(F,R))(P));

dlogPhi = method(TypicalValue => Matrix);
dlogPhi (Arrangement,List) := Matrix => (A,a) -> (
     R := ring A;
     F := frac R;
     n := #(A.hyperplanes);
     v := transpose matrix {apply(n,i->a_i/(A.hyperplanes)_i)};
     (coefficients A)*v);

freeDlogPhi = method(TypicalValue => Matrix);
freeDlogPhi (Arrangement,List) := Matrix => (A,a) -> (
     M := (transpose der A)*dlogPhi(A,a);
     R := ring numerator M_(0,0);
     (map(R,ring M))(M));

HS = i-> reduceHilbert hilbertSeries i;

beginDocumentation()

undocumented {HS,omega,crit}

document { 
     Key => HyperplaneArrangements,
     Headline => "hyperplane arrangements",
     EM "HyperplaneArrangement", " is a package for manipulating
     hyperplane arrangements.",
     PARA{},
          "A hyperplane arrangement is a finite set of hyperplanes in an
     affine or projective space.  In this package, an arrangement is 
     expressed as a list of (linear) defining equations for the hyperplanes.
     The tools provided allow the user to create new arrangements from
     old, and to compute various algebraic invariants of arrangements."
     }

document {  Key => Arrangement,
     Headline => "class of hyperplane arrangements",
     PARA{},
     "A hyperplane is a linear subspace of codimension one.  An
     arrangement is a finite set of hyperplanes.",
     }
document { 
     Key => {arrangement, (arrangement,List), (arrangement,List,Ring),
     	  (arrangement,Arrangement,Ring)},
     Headline => "create a hyperplane arrangement",
     Usage => "arrangement(L,R)",
     Inputs => {
	  "L" => {"a list of linear equations in the ring ", TT "R"},
	  "R" => {"a polynomial ring or linear quotient of a
	       polynomial ring"},	  
          },
     Outputs => {
	  Arrangement => {"the hyperplane arrangement determined by ",
	       TT "L", " and ", TT "R"},
          },
     "A hyperplane is a linear subspace of codimension one.  An
     arrangement is a finite set of hyperplanes.",     
     PARA{},
     "Probably the best-known hyperplane arrangement is the braid
     arrangement consisting of all the diagonal hyperplanes.  In
     4-space, it is constructed as follows: ",
     EXAMPLE {
          "S = ZZ[w,x,y,z];",
	  "A3 = arrangement {w-x,w-y,w-z,x-y,x-z,y-z}",
	  "describe A3",
	  },
     "If we project along onto a subspace, then we obtain an essential
     arrangement:",
     EXAMPLE {     
	  "R = S/ideal(w+x+y+z)",
	  "A3' = arrangement({w-x,w-y,w-z,x-y,x-z,y-z},R)",
	  "describe A3'",
	  },          
     "The trivial arrangement has no equations.",
     EXAMPLE {     
	  "trivial = arrangement({},S)",
	  "describe trivial",
	  "ring trivial",
	  },
     Caveat => {"If the elements of ", TT "L", " are not ",
	  TO2(RingElement, "ring elements"), " in ", TT "R", ", then
	  the induced identity map is used to map them from ", 
	  TT "ring L#0", " into ", TT "R", "."},
     SeeAlso => {HyperplaneArrangements}
     }
document { 
     Key => (ring,Arrangement),
     Headline => "get the associated ring",
     Usage => "ring A",
     Inputs => {
	  "A" => Arrangement,
          },
     Outputs => { 
	  {"the ", TO2(Ring, "ring"), " that contains the defining
	  equations of ", TT "A"} 
	  },
     "description",
     Caveat => {},
     SeeAlso => {Arrangement,arrangement}
     }
document { 
     Key => (matrix,Arrangement),
     Headline => "create a matrix from the equations of an arrangement",
     Usage => "matrix A",
     Inputs => {
	  "A" => Arrangement,
          },
     Outputs => {
	  {"the ", TO2(Matrix, "matrix"), " constructed from the list
	  of defining equations of ", TT "A"}     	  
          },
     EXAMPLE {
          },
     SeeAlso => {Arrangement,arrangement,coefficients}
     }
document { 
     Key => (coefficients,Arrangement),     
     Headline => "create a matrix from the coefficients of the
     equations of an arrangment",
     Usage => "coefficients A",
     Inputs => {
	  "A" => Arrangement
          },
     Outputs => {
	  Matrix
          },
     "description",
     EXAMPLE {
          },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (rank,Arrangement),
     Headline => "compute the rank",
     Usage => "rank A",
     Inputs => {
	  "A" => Arrangement
          },
     Outputs => {
	  {"the rank of ", TT "A"}
          },
     EXAMPLE {
          },
     SeeAlso => {}
     }
document { 
     Key => (trim,Arrangement),
     Headline => "minimize the generators",
     Usage => "trim A",
     Inputs => {
	  "A" => Arrangement
          },
     Outputs => {
	  Arrangement
          },
     "description",
     EXAMPLE {
          },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (compress,Arrangement),
     Headline => "extract nonzero equations",
     Usage => "compress A",
     Inputs => {
	  "A" => Arrangement
          },
     Outputs => {
	  Arrangement
          },
     Consequences => {
          },     
     "description",
     EXAMPLE {
          },
     SeeAlso => {}
     }

document {
     Key => {typeA,(typeA,ZZ)},
     Headline => "Type A reflection arrangement",
     SeeAlso => {typeB, typeD},
     Usage => "typeA(n) or typeA(n,R) or typeA(n,k)",
     Inputs => {
	  "n" => ZZ => "the rank",
	  "R" => PolynomialRing => "a polynomial (coordinate) ring in n+1 variables",
	  "k" => Ring => {"a coefficient ring; by default, ", TT "QQ"}
	  },
     Outputs => {
	  Arrangement => "the A_n reflection arrangement"
	  },
     "The hyperplane arrangement with hyperplanes x_i-x_j.",
     EXAMPLE lines ///
          A3 = typeA(3)
	  describe A3
	  ring A3
	  ///,
     "Alternatively, one may specify a coordinate ring,",
     EXAMPLE lines ///
	  S = ZZ[w,x,y,z];
	  A3' = typeA(3,S)
	  describe A3'
	  ///,
     "or a coefficient ring:",
     EXAMPLE lines ///
     	  A4 = typeA(4,ZZ/3)
	  ring A4
	  ///
}

document {
     Key => (typeA,ZZ,PolynomialRing),
     Headline => "A_n arrangement with specified coordinate ring",
     SeeAlso => {typeB, typeD},
     Usage => "typeA(n,R)",
     Inputs => {
	  "n" => ZZ => "the rank",
	  "R" => PolynomialRing => "a polynomial (coordinate) ring in n+1 variables",
	  },
     Outputs => {
	  Arrangement => "the A_n reflection arrangement"
	  }
     }

document {
     Key => (typeA,ZZ,Ring),
     Headline => "A_n reflection arrangement with specified coefficient ring",
     SeeAlso => {typeB, typeD},
     Usage => "typeA(n,k)",
     Inputs => {
	  "n" => ZZ => "the rank",
	  "k" => Ring => "a coefficient ring"
	  },
     Outputs => {
	  Arrangement => {"the A_n reflection arrangement, over ", TT "k"}
	  }
     }

document {
     Key => {typeB,(typeB,ZZ),(typeB,ZZ,PolynomialRing),(typeB,ZZ,Ring)},
     Headline => "Type B reflection arrangement",
     SeeAlso => {typeA, typeD},
     Usage => "typeB(n) or typeB(n,R) or typeB(n,k)",
     Inputs => {
	  "n" => ZZ => "the rank",
	  "R" => PolynomialRing => "a polynomial (coordinate) ring in n variables",
	  "k" => Ring => {"a coefficient ring; by default, ", TT "QQ"}
	  },
     Outputs => {
	  Arrangement => "the B_n reflection arrangement"
	  },
     "The hyperplane arrangement with hyperplanes defined by the type-B_n root system: for example,",
     EXAMPLE {
          "B3 = typeB(3)",
	  "describe B3",
	  "ring B3",
 	  }
     }

document {
     Key => {typeD,(typeD,ZZ),(typeD,ZZ,PolynomialRing),(typeD,ZZ,Ring)},
     Headline => "Type D reflection arrangement",
     SeeAlso => {typeA, typeB},
     Usage => "typeD(n) or typeD(n,R) or typeD(n,k)",
     Inputs => {
	  "n" => ZZ => "the rank",
	  "R" => PolynomialRing => "a polynomial (coordinate) ring in n variables",
	  "k" => Ring => {"a coefficient ring; by default, ", TT "QQ"}
	  },
     Outputs => {
	  Arrangement => "the D_n reflection arrangement"
	  },
     "The hyperplane arrangement with hyperplanes defined by the type D_n root system: for example,",
     EXAMPLE {
          "D4 = typeD(4)",
	  "describe D4",
	  "ring D4",
	  }
     }

document {
     Key => {orlikSolomon,(orlikSolomon,Arrangement),
	     (orlikSolomon,Arrangement,Ring),
	     (orlikSolomon,Arrangement,Symbol)},
     Headline => "defining ideal for the Orlik-Solomon algebra",
     Usage => "orlikSolomon(A) or orlikSolomon(A,E) or orlikSolomon(A,e)",
     Inputs => {
	  "A" => Arrangement => "an arrangement",
	  "E" => Ring => "a skew-commutative polynomial ring
	   with one variable for each hyperplane",
	  "e" => Symbol => "a name for an indexed variable"
	  },
     Outputs => {
	  Ideal => {"the defining ideal of the Orlik-Solomon algebra of ", 
	            TT "A"}
	  },
     "The Orlik-Solomon algebra is the cohomology ring of the complement of 
      the hyperplanes, either in complex projective or affine space.  The
      optional Boolean argument ", TT "Projective", " specifies which.",
     EXAMPLE lines ///
     	  A = typeA(3)
     	  I = orlikSolomon(A,e)
	  reduceHilbert hilbertSeries I
	  I' = orlikSolomon(A,Projective=>true,HypAtInfinity=>2)
	  reduceHilbert hilbertSeries I'
     ///
     }
 
document {
     Key => [orlikSolomon,HypAtInfinity],
     Headline => "hyperplane at infinity",
     TT "HypAtInfinity => n", " specifies that n indexes the hyperplane 
     at infinity to be used in computing the (projective) Orlik-Solomon
     algebra.  Ignored without ", TT "Projective => true", "."
     }

document {
     Key => [orlikSolomon,Projective],
     Headline => "specify projective complement",
     TT "Projective => true", " specifies the cohomology ring of the
     complement of the hyperplanes in complex projective space.  For
     practical reasons (at the expense of some elegance) an affine chart
     for the complement is chosen by making the hyperplane numbered ", 
     TT "n", " the hyperplane at infinity.  By default, ", TT "n=0",
     "; otherwise, use the option ", TT "HypAtInfinity => n", "."
     }

document {
     Key => Flat,
     Headline => "intersection of hyperplanes",
     PARA {},
     "A flat is a set of hyperplanes, maximal with respect to the property
     that they contain a given subspace.  In this package, flats are treated
     as lists of indices of hyperplanes in the arrangement."
     }

document {
     Key => flat,
     Headline => "make a flat from a list of indices",
     Usage => "flat(A,L)",
     Inputs => {
	  "A" => Arrangement => "hyperplane arrangement",
	  "L" => List => "list of indices in flat"
	  },
     Outputs => {
	  "F" => Flat => "corresponding flat"
	  }
     }

document {
     Key => {flats,(flats,ZZ,Arrangement),(flats,Arrangement)},
     Headline => "list the flats of an arrangement of given rank",
     Usage => "flats(n,A)",
     Inputs => {
  	  "n" => ZZ => "rank",
	  "A" => Arrangement => "hyperplane arrangement"
	  },
     Outputs => {
	  "L" => List => {"A list of ", TO2{Flat,"flats"}, " of rank ", TT "n"}
	  },
     "If the rank is omitted, the ", TO2{Flat,"flats"}, " of each rank are
     listed.",
     EXAMPLE lines ///
     	  A = typeA(3)
	  flats(2,A)
     ///
     }
     
document {
     Key => {closure,(closure,Arrangement,List)},
     Headline => "closure operation in the intersection lattice",
     Usage => "closure(A,L)",
     Inputs => {
	  "A" => Arrangement => "ambient arrangement",
	  "L" => List => "list of hyperplanes"
	  },
     Outputs => {
	  "F" => Flat => {
	       {"the flat of least codimension containing hyperplanes ", TT "L"}
		}
	  }
     }
     
document {
     Key => {meet,(meet,Flat,Flat)},
     Headline => "meet operation in intersection lattice",
     Usage => "meet(F,G)",
     Inputs => {
	  "F" => Flat => "a flat",
	  "G" => Flat => "another flat from the same arrangement"
	  },
     Outputs => {
	  "H" => Flat => {"the flat of greatest codimension that is
	  contained in both ", TT "F", " and ", TT "G", ".  If one identifies
	  flats with subspaces, this is the Minkowski sum of subspaces", 
	  TT "F", " and ", TT "G", "."}
	  },
     "The operator ", TO (symbol ^, Flat, Flat), " can be used as a synonym."
     }

document {
     Key => {(symbol ^, Flat, Flat)},
     Headline => "meet operation in intersection lattice",
     SeeAlso => meet,
     "A synonym for ", TO (meet, Flat, Flat), "."
     }

document {
     Key => {vee,(vee,Flat,Flat)},
     Headline => "join operation in intersection lattice",
     Usage => "vee(F,G)",
     Inputs => {
	  "F" => Flat => "a flat",
	  "G" => Flat => "another flat from the same arrangement"
	  },
     Outputs => {
	  "H" => Flat => {"the flat of least codimension that
	  contains in both ", TT "F", " and ", TT "G", ".  If one identifies
	  flats with subspaces, this is the intersection of subspaces", 
	  TT "F", " and ", TT "G", "."}
	  },
     "The operator ", TO (symbol |, Flat, Flat), " can be used as a synonym."
     }

document {
     Key => {(symbol |, Flat, Flat)},
     Headline => "join operation in intersection lattice",
     SeeAlso => vee,
     "A synonym for ", TO (vee, Flat, Flat), "."
     }

document {
     Key => {euler,(euler,Flat),(euler,Arrangement)},
     Usage => "euler(F) or euler(A)",
     Inputs => {
	  "F" => Flat => "a flat",
	  "A" => Arrangement => "an arrangement"
	  },
     Outputs => {
	  "k" => ZZ => {"the beta invariant of the flat ", TT "F", " or
	       arrangement ", TT "A"},
	       },
     "The beta invariant of an arrangement ", TT "A", " is, by definition, 
     the Euler characteristic of complement of ", TT "A", " in complex 
     projective space.", PARA {},
     "The beta invariant of a flat ", TT "F", " is the beta invariant of the ",
     TO2(restriction, "restriction"), " of ", TT "A", " to ", TT "F", ".",
     EXAMPLE lines ///
     	  A = typeA(3)
	  euler A -- for a real arrangement, equals number of bounded chambers
     ///
     }
	  
document {
     Key => {restriction,(restriction,Arrangement,Flat),
	  (restriction,Flat),
  	  (restriction,Arrangement,RingElement),
	  (restriction,Arrangement,Ideal)},
     Headline => "restriction of arrangement to flat/hyperplane",
     Usage => "restriction(A,F) or restriction(A,x) or restriction(A,I)",
     Inputs => {
	  "A" => Arrangement => "a hyperplane arrangement (optional)",
	  "F" => Flat => "flat to which you restrict",
	  "x" => RingElement => "equation of hyperplane to which you restrict",
	  "I" => Ideal => "ideal defining subspace to which you restrict"
	    },
     Outputs => {
	  Arrangement => {"the restriction of ", TT "A"}
	  },
     "The restriction of an arrangement to the subspace ", TT "X", 
     " indexed by a flat is the (multi)set of hyperplanes ", TT "H intersect X",
     " for all ", TT "H", " in the arrangement ", TT "A", ".  In the first
     case, one can also write ", TO2((symbol ^, Arrangement, Flat), "A^F"), 
     ".",
     EXAMPLE lines ///
     	  A = typeA(3)
     	  flats(2,A)
	  restriction first oo
	  x = (ring A)_0
	  restriction(A,x)
     ///,
     SeeAlso =>  (symbol ^,Arrangement,Flat),
     }

document {
     Key => (symbol ^, Arrangement, Flat),
     Headline => "restriction of arrangement to flat",
     Usage => "A^F",
     Inputs => {
	  "A" => Arrangement => "a hyperplane arrangement",
	  "F" => Flat => "flat to which you restrict"
	  },
     Outputs => {
	  Arrangement => {"the restriction of ", TT "A", " to ", 
	  TT "F"}
     },
     "A synonym for ", TO(restriction), "."
     }

document {
     Key => {graphic, (graphic,List), (graphic,List,Ring), 
	  (graphic,List,PolynomialRing)},
     Headline => "Make a graphic arrangement",
     Usage => "graphic(G) or graphic(G,k) or graphic(G,R)",
     Inputs => {
	  "G" => List => "a graph, expressed as a list of pairs of vertices",
	  "k" => Ring => {"an optional coefficient ring, by default ", 
	       TT "QQ"},
	  "R" => PolynomialRing => "an optional coordinate ring for the 
	  arrangement"
     },
     Outputs => {
	  Arrangement => {"the graphic arrangement from graph ", TT "G"}
     },
     SeeAlso => typeA,
     "A graph ", TT "G", " has vertices 1, 2, ..., n, and its edges are
     a list of lists of length 2.  The graphic arrangement ", TT "A(G)", 
     " of ", TT "G", " is, by definition, the subarrangement of the
     type A_(n-1) arrangement with hyperplanes ", TT "x_i-x_j", " for
     each edge ", TT "{i,j}", " of ", TT "G",
     EXAMPLE lines ///
     	  G = {{1,2},{2,3},{3,4},{4,1}}; -- a four-cycle
	  AG = graphic G
	  describe AG
	  rank AG -- the number of vertices minus number of components
	  ring AG
	  ring graphic(G,ZZ[x,y,z,w])
	  ///
	  }
     

     

	  
     

TEST ///
R = ZZ[x,y,z];
trivial = arrangement({},R);
assert(rank trivial == 0)
assert(ring trivial == R)
assert(0 == matrix trivial)
assert(0 == coefficients trivial)
///
end
--trim trivial
--
A3 = arrangement({x,y,z,x-y,x-z,y-z},R)
describe A3
product A3

A3' = arrangement {x,y,z,x-y,x-z,y-z}
A3' == A3
--assert(rank A3 == 3)
--product A3
--A3.hyperplanes
--X3 = arrangement {x,y,z,y-z,x-z,2*x+y}
--NF = arrangement {x,y,z,x-y,x-z,y-z,x+y-z}
--///

end




document {
     Key => HyperplaneArrangements,
     Headline => "hyperplane arrangements",
     EM "HyperplaneArrangements", " is a package for manipulating hyperplane
     arrangements.",
     "A hyperplane arrangement is a finite set of hyperplanes in an
     affine or projective space.  In this package, an arrangement is 
     expressed as a list of (linear) defining equations for the hyperplanes.
     The tools provided allow the user to create new arrangements from
     old, and to compute various algebraic invariants of arrangements."
}

document {
     Key => Arrangement,
     Headline => "the class of all arrangements",
     TT "Arrangement", " -- the class of hyperplane arrangements"
}


document (
     Key => arrangement,
     Headline => "create a hyperplane arrangement",
     Usage => "arrangement L",
     Inputs => (
	  "L" => List => "a list of ring elements defining the hyperplanes"
	  ),
     Outputs => (
	  Arrangement => ("the arrangement with hyperplanes ", TT "L")
	  ),
     "A hyperplane arrangement consists of a list of degree-1 ring elements."
     )
     
end

path = append(path, homeDirectory | "exp/hyppack/")
installPackage("HyperplaneArrangements",RemakeAllDocumentation=>true,DebuggingMode => true)
