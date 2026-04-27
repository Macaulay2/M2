---------------------------------------------------------------------------
-- PURPOSE : Methods for Janet bases and Pommaret bases
--           (as particular cases of involutive bases)
-- PROGRAMMER : Daniel Robertz
-- UPDATE HISTORY : 06 August 2009
-- (tested with Macaulay 2, version 1.2)
---------------------------------------------------------------------------
newPackage(
        "InvolutiveBases",
        Version => "1.10",
        Date => "August 06, 2009",
        Authors => {{Name => "Daniel Robertz",
                  Email => "daniel@momo.math.rwth-aachen.de",
                  HomePage => "http://wwwb.math.rwth-aachen.de/~daniel/"}},
        Headline => "Methods for Janet bases and Pommaret bases in Macaulay 2",
        PackageExports => { "Complexes" },
	Keywords => {"Groebner Basis Algorithms"},
        DebuggingMode => false
        )


-- news in version 1.10:
--      janetBasis accepts the zero ideal etc.
--      new: invNoetherNormalization

-- news in version 1.01:
--      invSyzygies and janetResolution respect gradings
--      new: factorModuleBasis enumerates standard monomials
--      "pretty printing" for InvolutiveBasis and FactorModuleBasis


export {"basisElements", "multVar", "janetMultVar", "pommaretMultVar", "janetBasis", "InvolutiveBasis",
     "isPommaretBasis", "invReduce", "invSyzygies", "janetResolution", "Involutive", "multVars",
     "FactorModuleBasis", "factorModuleBasis", "invNoetherNormalization", "PermuteVariables"}

----------------------------------------------------------------------
-- type InvolutiveBasis
----------------------------------------------------------------------

InvolutiveBasis = new Type of HashTable

basisElements = method()

basisElements InvolutiveBasis := J -> J#0

multVar = method()

multVar(InvolutiveBasis) := J -> (
     v := generators ring J#0;
     apply(J#1, i->set(select(v, j -> i#j == 1))))

multVar(Complex, ZZ) := (C, n) -> (
     v := generators C.ring;
     apply(C.dd_n.cache.multVars, i->set(select(v, j -> i#j == 1))))


----------------------------------------------------------------------
-- type FactorModuleBasis
----------------------------------------------------------------------

FactorModuleBasis = new Type of HashTable

--basisElements = method()   has already been declared above

basisElements FactorModuleBasis := F -> F#0

--multVar = method()         has already been declared above

multVar(FactorModuleBasis) := F -> (
     v := generators ring F#0;
     apply(F#1, i->set(select(v, j -> i#j == 1))))


----------------------------------------------------------------------
-- subroutines (not exported)
----------------------------------------------------------------------

involutiveBasisPrettyPrint = J -> (
     if numgens source J#0 == 0 then (
	  netList(transpose {{0}, apply(multVar J, elements)})
     )
     else (
	  if numgens target J#0 == 1 then
	       netList(transpose {first entries J#0, apply(multVar J, elements)})
	  else
	       netList(transpose { apply(toList(0..(numgens source J#0)-1), i->(J#0)_i),
		    apply(multVar J, elements)})
     )
     -- alternatively,
     -- netList(transpose {transpose entries J#0, apply(multVar J, elements)})
     )


InvolutiveBasis#{Standard,BeforePrint} = involutiveBasisPrettyPrint;

FactorModuleBasis#{Standard,BeforePrint} = involutiveBasisPrettyPrint;


-- determine multiplicative variables for list of exponents exp2 with
-- respect to Janet division
-- (exp1 is list of exponents of monomial which is lexicographically
-- next greater than that with exp2, and has multiplicative variables
-- specified by mult1)

janetDivision = (exp1, exp2, mult1) -> (
     n := length(exp1);
     k := 0;
     mult2 := {};
     while (k < n and exp1#k == exp2#k) do (
	  mult2 = append(mult2, mult1#k);
	  k = k+1;
     );
     if k == n then error "list of polynomials is not autoreduced";
     mult2 = append(mult2, 0);
     k = k+1;
     while k < n do (
	  mult2 = append(mult2, 1);
	  k = k+1;
     );
     mult2)


-- determine multiplicative variables for list of exponents expon with
-- respect to Pommaret division

pommaretDivision = (expon) -> (
     n := length(expon);
     k := n-1;
     mult := {};
     while (k >= 0 and expon#k == 0) do (
	  mult = prepend(1, mult);
	  k = k-1;
     );
     if k >= 0 then ( mult = prepend(1, mult); k = k-1; );
     while k >= 0 do (
	  mult = prepend(0, mult);
	  k = k-1;
     );
     mult)


-- decide whether or not list of exponents exp1 (with multiplicative
-- variables specified by mult1) is an involutive divisor of exp2

involutiveDivisor = (exp1, exp2, mult1) -> (
     for i from 0 to length(exp1)-1 do (
	  if (exp1#i > exp2#i or (exp1#i < exp2#i and mult1#i == 0)) then return false;
     );
     true)


-- given a list L of (leading) monomials, return list of their
-- multiplicative variables with respect to Janet division

janetMultVarMonomials = L -> (
     R := ring L#0;
     F := coefficientRing R;
     v := generators R;
     n := length v;
     S := F[v, MonomialOrder=>Lex];
     M := matrix { for i in L list substitute(i, S) };
     p := reverse sortColumns M;
     mult := for i in v list 1;
     J := { hashTable(for j from 0 to n-1 list v#j => mult#j) } |
          for i from 1 to length(L)-1 list (
	       mult = janetDivision(
		    (exponents (entries M_(p#(i-1)))#0)#0,
		    (exponents (entries M_(p#i))#0)#0,
		    mult);
	       hashTable(for j from 0 to n-1 list v#j => mult#j)
	       );
     p = for i from 0 to length(p)-1 list position(p, j -> j == i);
     use R;
     for i from 0 to length(p)-1 list J#(p#i))


-- recursive procedure that partitions the complement of a
-- multiple closed set of monomials (in particular a given
-- initial ideal) into monomial cones;
-- decomposeComplement is used by factorModuleBasis

decomposeComplement = (E, eta, a) -> (
     m := length E;
     if m == 0 then (
          { (a, eta) }
     )
     else (
	  n := length eta;
	  -- assuming length(eta) = length(a) = n
	  -- and for every entry m of the list E, length(m) = n
	  i := 0;
	  while (i < n and eta#i == 0) do i = i+1;
          if i == n then error "list of polynomials is not autoreduced";
	  if sum(eta) == 1 then (
	       z := for j from 0 to n-1 list 0;
	       for j when a#i + j < E#0#i list (a_{0..(i-1)} | {a#i + j} | a_{(i+1)..(n-1)}, z)
	  )
          else (
	       -- assuming E is sorted lexicographically
               d := E#(m-1)#i;
	       s := 0;
	       local t;
	       C := flatten for j from 0 to d-1 list (
		    t = s;
	            while (t < m and E#t#i == j) do t = t+1;
		    if t == m then error "list of exponent lists is not sorted lexicographically";
		    D := decomposeComplement(E_{s..(t-1)},
                         eta_{0..(i-1)} | {0} | eta_{(i+1)..(n-1)},
                         a_{0..(i-1)} | {a#i + j} | a_{(i+1)..(n-1)});
		    s = t;
		    D);
               C | apply(decomposeComplement(E_{s..(m-1)},
                         eta_{0..(i-1)} | {0} | eta_{(i+1)..(n-1)},
                         a_{0..(i-1)} | {a#i + d} | a_{(i+1)..(n-1)}),
                    k->(k#0, (k#1)_{0..(i-1)} | {1} | (k#1)_{(i+1)..(n-1)}))
	  )
     ))


-- given a monomial m in a polynomial ring with n variables,
-- return the class of m

monomialClass = (m, n) -> (
     -- alternatively, use 'support' and 'index'
     expon := (exponents m)#0;
     k := n-1;
     while (k >= 0 and expon#k == 0) do k = k-1;
     k)


-- subroutine used by janetResolution,
-- sorts Janet basis such that iterated syzygy computation is
-- possible (schreyerOrder depends on this order of the
-- involutive basis)

sortByClass = J -> (
     R := ring J#0;
     v := generators R;
     n := length v;
     mult := J#1;
     L := leadTerm J#0;
     L = for i from 0 to (numgens source L)-1 list
          { leadMonomial sum entries L_i, leadComponent L_i };
     p := toList(0..(length(mult)-1));
     F := coefficientRing R;
     S := F[v, MonomialOrder=>Lex];
     modified := true;
     while modified do (
	  modified = false;
	  for i from 1 to length(p)-1 do
	       if (L#(p#i)#1 < L#(p#(i-1))#1 or (L#(p#i)#1 == L#(p#(i-1))#1 and
		  (monomialClass(L#(p#i)#0, n) < monomialClass(L#(p#(i-1))#0, n) or
		  (monomialClass(L#(p#i)#0, n) == monomialClass(L#(p#(i-1))#0, n) and
		   substitute(L#(p#i)#0, S) > substitute(L#(p#(i-1))#0, S))))) then (
	          p = for j from 0 to length(p)-1 list (
		       if j == i-1 then
		          p#i
		       else if j == i then
		          p#(i-1)
		       else
		          p#j
	          );
	          modified = true;
	       );
     );
     use R;
     new InvolutiveBasis from hashTable {0 => submatrix(J#0, p),
          1 => for i from 0 to length(p)-1 list mult#(p#i)})


----------------------------------------------------------------------
-- main routines
----------------------------------------------------------------------

-- given a matrix M of polynomials, return the list that gives for
-- each column of M the set of multiplicative variables with respect
-- to Janet division

-- given a list L of polynomials, do the same for the matrix formed
-- by L as its only row

janetMultVar = method(TypicalValue => List)

janetMultVar(Matrix) := M -> (
     R := ring M;
     F := coefficientRing R;
     v := generators R;
     n := length v;
     S := F[v, MonomialOrder=>{Position=>Up, Lex}];
     L := substitute(leadTerm M, S);
     p := reverse sortColumns L;
     J := for i from 0 to length(p)-1 list
          { leadMonomial sum entries L_(p#i), leadComponent L_(p#i) };
     -- select generators according to their leading component
     r := numgens target M;
     J = for i from 0 to r-1 list
	  select(J, j -> j#1 == i);
     local mult;
     J = flatten for k from 0 to r-1 list (
          mult = for i in v list 1;
	  { set v } |
          for i from 1 to length(J#k)-1 list (
	       mult = janetDivision(
		    (exponents J#k#(i-1)#0)#0,
		    (exponents J#k#i#0)#0,
		    mult);
	       set(apply(select(toList(0..n-1), j -> mult#j == 1), k -> v#k))
	       )
	  );
     p = for i from 0 to length(p)-1 list position(p, j -> j == i);
     use R;
     for i from 0 to length(p)-1 list J#(p#i))

janetMultVar(List) := L -> janetMultVar(matrix {L})


-- given a matrix M of polynomials, return the list that gives for
-- each column of M the set of multiplicative variables with respect
-- to Pommaret division

-- given a list L of polynomials, do the same for the matrix formed
-- by L as its only row

pommaretMultVar = method(TypicalValue => List)

pommaretMultVar(Matrix) := M -> (
     v := generators ring M;
     n := length v;
     local mult;
     for i from 0 to (numgens source M)-1 list (
	  mult = pommaretDivision((exponents leadMonomial sum entries leadTerm M_i)#0);
	  set(apply(select(toList(0..n-1), j -> mult#j == 1), k -> v#k))
          ))

pommaretMultVar(List) := L -> pommaretMultVar(matrix {L})


-- given a (minimal) Groebner basis G for a submodule of a
-- free module over a polynomial ring, return a (minimal)
-- Janet basis for the same submodule
-- (up to now, it is not tail-reduced), that is a sequence
-- (matrix, list of hash tables specifying the
-- multiplicative variables for each column)

-- given a matrix M of polynomials, return a Janet basis
-- for the module generated by the columns of M

janetBasis = method(TypicalValue => InvolutiveBasis)

janetBasis(GroebnerBasis) := G -> (
     M := generators G;
     R := ring M;
     if not isPolynomialRing(R) then error "janetBasis is only defined for polynomial rings";
     if not isField(coefficientRing R) then error "expecting the ground ring to be a field";
     v := generators R;
     if zero M then (
	  new InvolutiveBasis from hashTable {0 => M, 1 => { hashTable(for j in v list j => 1) }}
     )
     else (
	  M = for i from 0 to (numgens target M)-1 list
	       submatrix(M, select(toList(0..(numgens source M)-1),
		    j -> leadComponent leadTerm M_j == i));
	  local J;
	  local N;
	  local P;
	  local Q;
	  M = for c from 0 to length(M)-1 list (
	       N = M#c;
	       if numgens source N == 0 then continue;
	       -- leading monomials are all in c-th row
	       J = janetMultVarMonomials for i in flatten entries N^{c} list leadMonomial i;
	       P = flatten for i from 0 to length(J)-1 list (
		    for j in v list (
			 if J#i#j == 1 then continue;
			 map(target N, R^{ -(degree(j) + (degrees source N_{i})_0)}, j * N_{i})
			 )
		    );
	       P = for i in P list (
		    if length(select(toList(0..length(J)-1),
			 j -> involutiveDivisor(
			 (exponents leadMonomial (entries N^{c}_j)#0)#0,
			 (exponents leadMonomial (entries i^{c})#0#0)#0,
			 for k in v list J#j#k))) > 0 then continue;
		    i
		    );
	       while length(P) > 0 do (
		    Q = fold((M1, M2) -> M1 | M2, P);
		    N = N | (sort Q)_{0};
		    J = janetMultVarMonomials for i in flatten entries N^{c} list leadMonomial i;
		    P = flatten for i from 0 to length(J)-1 list (
			 for j in v list (
			      if J#i#j == 1 then continue;
			      map(target N, R^{ -(degree(j) + (degrees source N_{i})_0)}, j * N_{i})
			      )
			 );
		    P = for i in P list (
			 if length(select(toList(0..length(J)-1),
			      j -> involutiveDivisor(
			      (exponents leadMonomial (entries N^{c}_j)#0)#0,
			      (exponents leadMonomial (entries i^{c})#0#0)#0,
			      for k in v list J#j#k))) > 0 then continue;
			 i
			 );
	       );
	       (N, J)
	  );
	  p := sortColumns M#0#0;
	  P = submatrix(M#0#0, p);
	  J = for j from 0 to length(p)-1 list M#0#1#(p#j);
	  for i from 1 to length(M)-1 do (
	       p = sortColumns M#i#0;
	       P = P | submatrix(M#i#0, p);
	       J = J | for j from 0 to length(p)-1 list M#i#1#(p#j);
	  );
	  new InvolutiveBasis from hashTable {0 => P, 1 => J}
     ))

janetBasis(Matrix) := M -> janetBasis gb M

janetBasis(Ideal) := I -> janetBasis gb I

janetBasis(Module) := M -> janetBasis presentation M

janetBasis(Complex, ZZ) := (C, n) -> new InvolutiveBasis from hashTable {0 => C.dd_n, 1 => C.dd_n.cache.multVars}


-- given a Janet basis J for a submodule of a free module
-- over a polynomial ring, as returned by janetBasis, decide
-- whether or not it is also a Pommaret basis for the same module

isPommaretBasis = method(TypicalValue => Boolean)

isPommaretBasis(InvolutiveBasis) := J -> pommaretMultVar(J#0) === multVar(J)


-- given a Janet basis J for a submodule of a free module
-- over a polynomial ring and an element p of this free module,
-- return the normal form of p modulo the Janet basis and the
-- coefficients used for the involutive reduction
-- (more precisely, we have p = r + J#0 * c,
-- where (r, c) is the result of invReduce, and * is
-- matrix multiplication)

-- given the Janet basis J and a matrix whose columns consist
-- of elements of the free module, do the same for each column

invReduce = method()

invReduce(Matrix,InvolutiveBasis) := (p, J) -> (
     if numgens target p != numgens target J#0 then error "the free modules containing the Janet basis and the element to be reduced must be the same";
     R := ring J#0;
     v := generators R;
     L := leadTerm J#0;
     L = for i from 0 to (numgens source L)-1 list
          { leadMonomial sum entries L_i,
	    leadComponent L_i,
	    leadCoefficient sum entries L_i };
     if length(L) == 0 then (
	  (p, matrix {{}})
     )
     else (
	  zl := 0*(target p)_0;
	  zr := 0*(R^(length L))_0;
	  local i;
	  local c;
	  local f;
	  local lc;
	  local lm;
	  local lt;
	  local m;
	  local q;
	  local r;
	  L = for j from 0 to (numgens source p)-1 list (
	       q = p_j;
	       r = zl;
	       c = zr;
	       f = (v#0)^0;  -- equals 1
	       while matrix {q} != 0 do (
		    lt = leadTerm q;
		    m = leadComponent lt;
		    lc = leadCoefficient sum flatten entries lt;
		    lm = leadMonomial sum flatten entries lt;
		    i = 0;
		    while i < length(L) do (
			 if (m == L#i#1) and involutiveDivisor(
			      (exponents L#i#0)#0,
			      (exponents lm)#0,
			      for k in v list J#1#i#k) then break;
			 i = i + 1;
		    );
		    if i < length(L) then (
			 -- didn't work without "substitute" for coefficients in finite fields
			 q = substitute(L#i#2, R) * q - lc * (lm // L#i#0) * J#0_i;
			 c = c + lc * (lm // L#i#0) * (R^(length L))_i;
			 r = substitute(L#i#2, R) * r;
			 f = L#i#2 * f;
		    )
		    else (
			 q = q - lt;
			 r = r + lt;
		    );
	       );
	       r = apply(r, i -> i // f);
	       c = apply(c, i -> i // f);
	       (r, c)
	  );
	  N := matrix { L#0#0 };
	  C := matrix { L#0#1 };
	  for j from 1 to length(L)-1 do (
	       N = N | matrix { L#j#0 };
	       C = C | matrix { L#j#1 };
	  );
	  (N, C)
     ))

invReduce(RingElement,InvolutiveBasis) := (p, J) -> invReduce(matrix {{p}}, J)


-- given a Janet basis J for a submodule of a free module
-- over a polynomial ring, as returned by janetBasis, return
-- Janet basis for the syzygies of J;
-- caveat: cannot be iterated because schreyerOrder is not used
-- (see janetResolution)

invSyzygies = method(TypicalValue => InvolutiveBasis)

invSyzygies(InvolutiveBasis) := J -> (
     bas := J#0;
     d := degrees source bas;
     mult := J#1;
     R := ring bas;
     v := generators R;
     zl := 0*(target bas)_0;
     local r;
     S := flatten for i from 0 to (numgens source bas)-1 list (
	  for j in v list (
	       if mult#i#j == 1 then continue;
               r = invReduce(j * bas_{i}, J);
	       if (r#0)_0 != zl then error "given data is not a Janet basis";
	       r = map(source J#0, R^{ -(degree(j) + d#i)}, matrix { j * (R^(length(mult)))_i } - r#1);
	       (r, hashTable(for k in v list if k <= j then ( k => 1 ) else ( k => mult#i#k )))
	  )
     );
     if length(S) > 0 then (
	  M := S#0#0;
	  L := { S#0#1 };
	  for j from 1 to length(S)-1 do (
	       M = M | S#j#0;
	       L = L | { S#j#1 };
	  );
          return new InvolutiveBasis from hashTable {0 => M, 1 => L};
     )
     else (
          return new InvolutiveBasis from
	       hashTable {0 => matrix(R, apply(length(mult), i -> {})), 1 => {}};
     );
     )


-- given a Janet basis for a submodule of a free module
-- over a polynomial ring, construct a free resolution R for
-- this module: R is a list of InvolutiveBasis such that R#i
-- is an involutive basis for the i-th syzygies

-- given a matrix M of polynomials, construct a free resolution
-- using Janet bases for the module generated by the columns of M

-- given an ideal of a polynomial ring or a module over a polynomial ring,
-- construct a free resolution using Janet bases for this ideal or this module

janetResolution = method(TypicalValue => Complex)

janetResolution(InvolutiveBasis) := J -> (
     R := { sortByClass(J) };
     S := invSyzygies R#(-1);
     S = (map(source schreyerOrder leadTerm R#(-1)#0, source S#0, S#0), S#1);
     while length(S#1) > 0 do (
	  R = R | { sortByClass(S) };
	  S = invSyzygies R#(-1);
          S = (map(source schreyerOrder leadTerm R#(-1)#0, source S#0, S#0), S#1);
     );
     C := complex(apply(R, i->i#0) | { map(source R#-1#0, (ring R#0#0)^0, 0) });
     for i from 1 to length(R) do C.dd_i.cache.multVars = R#(i-1)#1;
     C)

janetResolution(Matrix) := M -> janetResolution janetBasis M

janetResolution(Ideal) := I -> janetResolution janetBasis I

janetResolution(Module) := M -> janetResolution janetBasis presentation M

addHook((freeResolution, Module), Strategy => Involutive, (opts, M) ->
    if opts.Strategy === Involutive then janetResolution M)


-- enumeration of a (monomial) vector space basis of R/I
-- in terms of monomial cones, where I is an ideal of R
-- with Janet basis J

factorModuleBasis = method(TypicalValue => FactorModuleBasis)

factorModuleBasis(InvolutiveBasis) := J -> (
     R := ring J#0;
     v := generators R;
     n := length v;
     e := for i from 0 to n-1 list 1;
     z := for i from 0 to n-1 list 0;
     r := numgens target J#0;
     l := numgens source J#0;
     G := gens target J#0;
     L := leadTerm J#0;
     L = for i from 0 to l-1 list
          { leadMonomial sum entries L_i, leadComponent L_i };
     L = for i from 0 to r-1 list (
	  for j from 0 to l-1 list (
	       if L#j#1 != i then continue;
	       (exponents L#j#0)#0
	  ));
     L = apply(L, E->for i in decomposeComplement(sort E, e, z) list
	  (product for j from 0 to n-1 list (v#j)^(i#0#j), i#1));
     L = { for i from 0 to r-1 list (
	       if length(L#i) == 0 then continue;
	       matrix({ for j in L#i list j#0 }) ** G_{i}
          ),
          flatten for i from 0 to r-1 list
	  for j in L#i list hashTable(for k from 0 to n-1 list v#k => j#1#k) };
     if length(L#0) == 0 then
	  new FactorModuleBasis from hashTable { 0 => matrix {{}}, 1 => {} }
     else
	  new FactorModuleBasis from hashTable {
	       0 => fold((M1, M2) -> M1 | M2, L#0),
	       1 => L#1 })


-- compute a Noether normalization for the ideal generated by J
-- using the corresponding factor module basis

invNoetherNormalization = method(
     Options => {
	  PermuteVariables => false
	  }
     )

invNoetherNormalization(InvolutiveBasis) := o -> (J) -> (
     if (numgens target basisElements J != 1) then error "expecting an involutive basis for an ideal";

     R := ring J#0;
     Jnew := J;
     F := factorModuleBasis(Jnew);
     m := multVar F;
     if length(m) == 0 then error "expecting an involutive basis for a proper ideal";
     if length(toList(m#0)) == numgens(R) then error "expecting an involutive basis for a non-zero ideal";

     -- union of multiplicative variables of all cones
     nu := toList fold((S1, S2)->S1 + S2, m);
     -- maximum number of multiplicative variables for one cone
     d := max apply(m, i->length toList(i));
     -- coordinate change to be determined
     ch := {};

     while length(nu) > d do (
	  -- variable among the multiplicative ones which has
	  -- highest priority
	  z := max nu;
	  lm := flatten entries leadTerm Jnew#0;
	  -- only retain leading monomials involving variables in nu only
	  l := select(toList(0..(length(lm)-1)),
	       i->isSubset(support lm_i, nu));
	  -- take a leading monomial involving the least number
	  -- of variables (the smallest one w.r.t. the monomial ordering
	  -- on R is chosen here)
	  n := minPosition apply(l,
	       i->length support substitute(lm_i, z => 1_R));
	  lm = lm_(l_n);
	  -- determine the list of variables to be altered by
	  -- coordinate change
	  w := support substitute(lm, z => 1_R);
	  s := length w;
	  local deg;
	  local b;
	  local k;

          if char R == 0 then (
	       deg = sum degree lm;
	       -- get top degree part of polynomial in Janet basis
	       -- that has lm as leading monomial
	       head := sum select(terms (flatten entries Jnew#0)_(l_n),
		    i->sum degree(i) == deg);
	       b = for j from 0 to s-1 list 1_R;
	       k = 0;
	       -- change coefficients of coordinate change
	       -- until monomial z^deg appears in transformed head
	       while length select(terms substitute(head,
		    for j from 0 to s-1 list w#j => w#j - b#j * z),
		    i->support(i) == {z}) == 0 do (
		    b = b_{0..(k-1)} | { b_k + 1 } | b_{(k+1)..(s-1)};
		    k = (k + 1) % s;
	       );
               ch = append(ch, for j from 0 to s-1 list w#j => w#j - b#j * z);
	  )
          else (
	       b = for j from 0 to s-1 list 1_ZZ;
	       k = 0;
	       p := substitute((flatten entries Jnew#0)_(l_n),
		    for j from 0 to s-1 list w#j => w#j - z^(b#j));
	       deg = sum degree p;
	       -- change exponents in coordinate change
	       -- until monomial z^deg appears in transformed generator
	       while length select(terms p, i->i == z^deg) == 0 do (
		    b = b_{0..(k-1)} | { b_k + 1 } | b_{(k+1)..(s-1)};
		    k = (k + 1) % s;
		    p = substitute((flatten entries Jnew#0)_(l_n),
			 for j from 0 to s-1 list w#j => w#j - z^(b#j));
	            deg = sum degree p;
	       );
               ch = append(ch, for j from 0 to s-1 list w#j => w#j - z^(b#j));
	  );
	  Jnew = janetBasis substitute(Jnew#0, ch_(-1));
	  F = factorModuleBasis(Jnew);
	  m = multVar F;
	  nu = toList fold((S1, S2)->S1 + S2, m);
	  d = max apply(m, i->length toList(i));
     );
     if o.PermuteVariables then (
	  -- permute variables so that nu consists of the |nu| last variables
	  v := sort generators R;
	  g := set v_{0..(d-1)} * set nu;
	  u := toList(set nu - g);
	  if length u > 0 then (
	       g = toList(set v_{0..(d-1)} - g);
	       ch = append(ch,
		    (for j from 0 to length(u)-1 list g#j => u#j) |
		    (for j from 0 to length(u)-1 list u#j => g#j));
	       nu = toList(set v_{0..(d-1)});
	  );
     );
     -- compose all the above coordinate transformations
     tr := vars R;
     for i in ch do (
	  tr = substitute(tr, i);
     );
     -- alternative output:
     -- tr = flatten entries tr;
     -- {for i from 0 to length(tr)-1 list R_i => tr_i, sort nu}
     {flatten entries tr, sort nu})

invNoetherNormalization(GroebnerBasis) := o -> (G) -> invNoetherNormalization(janetBasis G, o)

invNoetherNormalization(Matrix) := o -> (M) -> invNoetherNormalization(janetBasis M, o)

invNoetherNormalization(Ideal) := o -> (I) -> invNoetherNormalization(janetBasis I, o)

invNoetherNormalization(Module) := o -> (M) -> invNoetherNormalization(janetBasis M, o)


----------------------------------------------------------------------
-- documentation
----------------------------------------------------------------------

beginDocumentation()

document { 
        Key => InvolutiveBases,
        Headline => "Methods for Janet bases and Pommaret bases in Macaulay 2",
        EM "InvolutiveBases", " is a package which provides routines for dealing with Janet and Pommaret bases.",
	PARA{
             TEX "Janet bases can be constructed from given Gr\\\"obner bases. It can be checked whether a Janet basis is a Pommaret basis. Involutive reduction modulo a Janet basis can be performed. Syzygies and free resolutions can be computed using Janet bases. A convenient way to use this strategy is to use an optional argument for ", TO "resolution", ", see ", TO "Involutive", "."
	    },
	PARA{
             "Some references:"
	    },
	UL {
	     "J. Apel, The theory of involutive divisions and an application to Hilbert function computations. J. Symb. Comp. 25(6), 1998, pp. 683-704.",
             TEX "V. P. Gerdt, Involutive Algorithms for Computing Gr\\\"obner Bases. In: Cojocaru, S. and Pfister, G. and Ufnarovski, V. (eds.), Computational Commutative and Non-Commutative Algebraic Geometry, NATO Science Series, IOS Press, pp. 199-225.",
             "V. P. Gerdt and Y. A. Blinkov, Involutive bases of polynomial ideals. Minimal involutive bases. Mathematics and Computers in Simulation 45, 1998, pp. 519-541 resp. 543-560.",
             "M. Janet, Leçons sur les systèmes des équations aux dérivées partielles. Cahiers Scientifiques IV. Gauthiers-Villars, Paris, 1929.",
             "J.-F. Pommaret, Partial Differential Equations and Group Theory. Kluwer Academic Publishers, 1994.",
             "W. Plesken and D. Robertz, Janet's approach to presentations and resolutions for polynomials and linear pdes. Archiv der Mathematik 84(1), 2005, pp. 22-37.",
	     TEX "D. Robertz, Janet Bases and Applications. In: Rosenkranz, M. and Wang, D. (eds.), Gr\\\"obner Bases in Symbolic Analysis, Radon Series on Computational and Applied Mathematics 2, de Gruyter, 2007, pp. 139-168.",
             "W. M. Seiler, A Combinatorial Approach to Involution and delta-Regularity: I. Involutive Bases in Polynomial Algebras of Solvable Type. II. Structure Analysis of Polynomial Modules with Pommaret Bases. Preprints, arXiv:math/0208247 and arXiv:math/0208250."
           }
        }

document {
        Key => {basisElements,(basisElements,InvolutiveBasis),(basisElements,FactorModuleBasis)},
        Headline => "extract the matrix of generators from an involutive basis or factor module basis",
        Usage => "B = basisElements J\nB = basisElements F",
        Inputs => {
	     "J" => InvolutiveBasis,
	     "F" => FactorModuleBasis
	     },
        Outputs => {
	   "B" => Matrix
	   },
	PARA{
	     TEX "If the argument of basisElements is ", ofClass InvolutiveBasis, ", then the columns of B are generators for the module spanned by the involutive basis. These columns form a Gr\\\"obner basis for this module."
	    },
	PARA{
	     "If the argument of basisElements is ", ofClass FactorModuleBasis, ", then the columns of B are generators for the monomial cones in the factor module basis."
	    },
        EXAMPLE lines ///
          R = QQ[x,y];
	  I = ideal(x^3,y^2);
	  J = janetBasis I;
	  basisElements J
        ///,
        EXAMPLE lines ///
	  R = QQ[x,y,z];
	  M = matrix {{x*y,x^3*z}};
	  J = janetBasis M;
	  F = factorModuleBasis J
	  basisElements F
	  multVar F
        ///,
        SeeAlso => {multVar,janetBasis,factorModuleBasis}
        }

document {
        Key => {multVar,(multVar,InvolutiveBasis),(multVar,Complex,ZZ),(multVar,FactorModuleBasis)},
        Headline => "extract the sets of multiplicative variables for each generator (in several contexts)",
        Usage => "m = multVar(J) or m = multVar(C,n) or m = multVar(F)",
	Inputs => {
	     "J" => InvolutiveBasis,
	     "C" => Complex,
	     "n" => ZZ,
	     "F" => FactorModuleBasis
	     },
        Outputs => {
	   "m" => List => { "list of sets of variables of the polynomial ring" }
	   },
	PARA{
	     "If the argument of multVar is ", ofClass InvolutiveBasis, ", then the i-th set in m consists of the multiplicative variables for the i-th generator in J."
	    },
	PARA{
	     "If the arguments of multVar are ", ofClass Complex, " and ", ofClass ZZ, ", where C is the result of either ", TO "janetResolution", " or ", TO "resolution", " called with the optional argument 'Strategy => Involutive', then the i-th set in m consists of the multiplicative variables for the i-th generator in the n-th differential of C."
	    },
	PARA{
	     "If the argument of multVar is ", ofClass FactorModuleBasis, ", then the i-th set in m consists of the multiplicative variables for the i-th monomial cone in F."
	    },
        EXAMPLE lines ///
          R = QQ[x,y];
	  I = ideal(x^3,y^2);
	  J = janetBasis I;
	  multVar J
        ///,
        EXAMPLE lines ///
          R = QQ[x,y,z];
	  I = ideal(x,y,z);
	  C = freeResolution(I, Strategy => Involutive)
	  multVar(C, 2)
        ///,
        EXAMPLE lines ///
	  R = QQ[x,y,z];
	  M = matrix {{x*y,x^3*z}};
	  J = janetBasis M
	  F = factorModuleBasis J
	  basisElements F
	  multVar F
        ///,
        SeeAlso => {janetBasis,janetMultVar,pommaretMultVar,basisElements,janetResolution,Involutive,factorModuleBasis}
        }

document {
        Key => {janetBasis,(janetBasis,Matrix),(janetBasis,Ideal),(janetBasis,GroebnerBasis),(janetBasis,Complex,ZZ)},
        Headline => "compute Janet basis for an ideal or a submodule of a free module",
        Usage => "J = janetBasis M or J = janetBasis(C,n)",
	Inputs => {
	     "M" => InvolutiveBasis,
	     "M" => Ideal,
	     "M" => GroebnerBasis,
	     "C" => Complex,
	     "n" => ZZ
	     },
        Outputs => {
	   "J" => InvolutiveBasis
	   },
	PARA{
             "If the argument for janetBasis is ", ofClass Matrix, " or ", ofClass Ideal, " or ", ofClass GroebnerBasis, ", then J is a Janet basis for (the module generated by) M."
	    },
	PARA{
	     "If the arguments for janetBasis are ", ofClass Complex, " and ", ofClass ZZ, ", where C is the result of either ", TO "janetResolution", " or ", TO "resolution", " called with the optional argument 'Strategy => Involutive', then J is the Janet basis extracted from the n-th differential of C."
	    },
        EXAMPLE lines ///
          R = QQ[x,y];
	  I = ideal(x^3,y^2);
	  J = janetBasis I;
	  basisElements J
	  multVar J
        ///,
        EXAMPLE lines ///
	  R = QQ[x,y];
	  M = matrix {{x*y-y^3, x*y^2, x*y-x}, {x, y^2, x}};
	  J = janetBasis M;
	  basisElements J
	  multVar J
        ///,
        EXAMPLE lines ///
          R = QQ[x,y,z];
	  I = ideal(x,y,z);
	  C = freeResolution(I, Strategy => Involutive)
	  janetBasis(C, 2)
        ///,
        SeeAlso => {janetMultVar,pommaretMultVar,isPommaretBasis,invReduce,invSyzygies,janetResolution}
        }

document {
        Key => {janetMultVar,(janetMultVar,Matrix),(janetMultVar,List)},
        Headline => "return table of multiplicative variables for given module elements as determined by Janet division",
        Usage => "janetMultVar M",
        Inputs => {{ "M, ", ofClass Matrix, " or ", ofClass List }},
        Outputs => {{ "list of sets of variables of the polynomial ring; the i-th set consists of the multiplicative variables for the i-th generator in J" }},
        EXAMPLE lines ///
          R = QQ[x1,x2,x3];
	  M = matrix {{ x1*x2*x3, x2^2*x3, x1*x2*x3^2 }};
	  janetMultVar M
        ///,
        SeeAlso => {pommaretMultVar,janetBasis,multVar,isPommaretBasis}
        }

document {
        Key => {pommaretMultVar,(pommaretMultVar,Matrix),(pommaretMultVar,List)},
        Headline => "return table of multiplicative variables for given module elements as determined by Pommaret division",
        Usage => "pommaretMultVar M",
        Inputs => {{ "M, ", ofClass Matrix, " or ", ofClass List }},
        Outputs => {{ "list of sets of variables of the polynomial ring; the i-th set consists of the multiplicative variables for the i-th generator in J" }},
        EXAMPLE lines ///
          R = QQ[x1,x2,x3];
	  M = matrix {{ x1*x2*x3, x2^2*x3, x1*x2*x3^2 }};
	  pommaretMultVar M
        ///,
        SeeAlso => {janetMultVar,janetBasis,multVar,isPommaretBasis}
        }

document {
        Key => {isPommaretBasis,(isPommaretBasis,InvolutiveBasis)},
        Headline => "check whether or not a given Janet basis is also a Pommaret basis",
        Usage => "P = isPommaretBasis J",
        Inputs => {{ "J, ", ofClass InvolutiveBasis, ", a Janet basis as returned by ", TO "janetBasis" }},
        Outputs => {
	   "P" => Boolean => { "the result equals true if and only if J is a Pommaret basis" }
	   },
        EXAMPLE lines ///
          R = QQ[x,y];
	  I = ideal(x^3,y^2);
	  J = janetBasis I
	  isPommaretBasis J
        ///,
        EXAMPLE lines ///
          R = QQ[x,y];
	  I = ideal(x*y,y^2);
	  J = janetBasis I
	  isPommaretBasis J
        ///,
        SeeAlso => {janetBasis,basisElements,multVar,janetMultVar,pommaretMultVar}
        }

TEST ///
     -- loadPackage "InvolutiveBases"
     R = QQ[x,y]
     I = ideal(x^3,y^2)
     G = gb I
     J = janetBasis G
     assert ( isPommaretBasis J == true )
///

TEST ///
     -- loadPackage "InvolutiveBases"
     R = QQ[x,y]
     I = ideal(x*y,y^2)
     G = gb I
     J = janetBasis G
     assert ( isPommaretBasis J == false )
///

document {
        Key => {factorModuleBasis,(factorModuleBasis,InvolutiveBasis)},
        Headline => "enumerate standard monomials",
        Usage => "F = factorModuleBasis(J)",
	Inputs => {
	     "J" => InvolutiveBasis
	     },
        Outputs => {
	   "F" => FactorModuleBasis => {"a partition of the set of monomials that are not leading monomial of any element of the module spanned by J, into monomial cones"}
	   },
	PARA{ "The result represents a collection of finitely many cones of monomials, each cone being the set of multiples of a certain monomial by all monomials in certain variables; the generating monomials are accessed by ", TO "basisElements", "; the sets of variables for each cone are obtained from ", TO "multVar", "." },
        EXAMPLE lines ///
	  R = QQ[x,y,z];
	  M = matrix {{x*y,x^3*z}};
	  J = janetBasis M;
	  F = factorModuleBasis J
	  basisElements F
	  multVar F
        ///,
        EXAMPLE lines ///
	  R = QQ[x,y];
          M = matrix {{x*y-y^3, x*y^2, x*y-x}, {x, y^2, x}};
          J = janetBasis M
          F = factorModuleBasis J
	  basisElements F
	  multVar F
        ///,
        SeeAlso => {janetBasis,basisElements,multVar}
        }

TEST ///
     -- loadPackage "InvolutiveBases"
     R = QQ[x1,x2,x3]
     M = matrix {{x1*x2,x1^3*x3}}
     J = janetBasis M
     F = factorModuleBasis J
     assert ( F#0 == matrix {{ 1, x1, x1^2, x1^3 }} )
     assert ( values applyPairs (F#1#0, (a,b)->if a == x1 then (a, b == 0) else (a, b == 1)) == { true, true, true } )
     assert ( values applyPairs (F#1#1, (a,b)->if a == x3 then (a, b == 1) else (a, b == 0)) == { true, true, true } )
     assert ( values applyPairs (F#1#2, (a,b)->if a == x3 then (a, b == 1) else (a, b == 0)) == { true, true, true } )
     assert ( values applyPairs (F#1#3, (a,b)->if a == x1 then (a, b == 1) else (a, b == 0)) == { true, true, true } )
///

document {
        Key => {invNoetherNormalization,(invNoetherNormalization,InvolutiveBasis),(invNoetherNormalization,GroebnerBasis),(invNoetherNormalization,Matrix),(invNoetherNormalization,Ideal),(invNoetherNormalization,Module)},
        Headline => "Noether normalization",
        Usage => "N = invNoetherNormalization I",
	Inputs => {
	     "I" => InvolutiveBasis,
	     "I" => GroebnerBasis,
	     "I" => Ideal
	     },
        Outputs => {
	   "N" => List => {"the first entry of which is a list defining an invertible coordinate transformation in terms of the images of the variables of the polynomial ring; the second entry is a list of variables whose residue classes modulo the ideal in the new coordinates are (maximally) algebraically independent"}
	   },
	PARA{ "invNoetherNormalization constructs an automorphism of the polynomial ring in which I defines an ideal, such that the image of I under this automorphism is in Noether normal position." },
	PARA{ "The automorphism is defined by the first list returned: the i-th variable of the polynomial ring is mapped to the i-th entry of that list." },
	PARA{ "In the new coordinates, the residue class ring is an integral ring extension of the polynomial ring in the variables given in the second list returned." },
	PARA{ "If the option ", TO "PermuteVariables", " is set to true, the second list consists of the last d variables, where d is the Krull dimension of the residue class ring." },
        PARA{
             "Reference: D. Robertz, Noether normalization guided by monomial cone decompositions, Journal of Symbolic Computation 44, 2009, pp. 1359-1373."
            },
        EXAMPLE lines ///
	  R = QQ[x,y,z];
	  I = ideal(x*y*z);
	  J = janetBasis I;
	  N = invNoetherNormalization J
        ///,
        EXAMPLE lines ///
	  R = QQ[w,x,y,z];
	  I = ideal(y^2*z-w*x*y^2, x*y*z-w*z^2, y^2*z-w*x^2*y*z);
	  J = janetBasis I;
	  N = invNoetherNormalization J
        ///,
        SeeAlso => {janetBasis,factorModuleBasis}
        }

TEST ///
     -- loadPackage "InvolutiveBases"
     R = QQ[x,y,z]
     I = ideal(x*y*z);
     J = janetBasis I;
     N = invNoetherNormalization J
     assert (N == {{x, - x + y, - x + z}, {z, y}})
///

TEST ///
     -- loadPackage "InvolutiveBases"
     R = QQ[w,x,y,z];
     I = ideal(y^2*z-w*x*y^2, x*y*z-w*z^2, y^2*z-w*x^2*y*z);
     J = janetBasis I;
     N = invNoetherNormalization J
     assert (N == {{w, x, - x + y, - w - x + z}, {z, y}})
///

TEST ///
     -- loadPackage "InvolutiveBases"
     R = QQ[x,y,z];
     I = ideal(x^2-1, y^2-z^2);
     J = janetBasis I;
     N = invNoetherNormalization J
     assert (N == {{x, y, z}, {z}})
///

document {
        Key => PermuteVariables,
        Headline => "ensure that the last dim(I) var's are algebraically independent modulo I",
	PARA{
             "The symbol PermuteVariables is an option for ", TO "invNoetherNormalization", "."
	    },
	PARA{
             "The default value for this option is false. If set to true, the second list of the result of ", TO "invNoetherNormalization", " consists of the last d variables in the new coordinates, where d is the Krull dimension of the ring under consideration."
	    },
	PARA{
             "In the new coordinates defined by ", TO "invNoetherNormalization", " the residue class ring is an integral ring extension of the polynomial ring in the last d variables."
	    },
        EXAMPLE lines ///
	  R = QQ[x,y,z];
	  I = ideal(x*y^2+2*x^2*y, z^3);
	  J = janetBasis I;
	  N1 = invNoetherNormalization J
	  N2 = invNoetherNormalization(J, PermuteVariables => true)
        ///,
        EXAMPLE lines ///
          R = QQ[w,x,y,z];
          I = ideal(w*x-y^2, y*z-x^2)
          J = janetBasis I;
          N1 = invNoetherNormalization J
          J1 = janetBasis substitute(gens I, for i in toList(0..numgens(R)-1) list R_i => N1#0#i);
          F1 = factorModuleBasis(J1)
          N2 = invNoetherNormalization(J, PermuteVariables => true)
          J2 = janetBasis substitute(gens I, for i in toList(0..numgens(R)-1) list R_i => N2#0#i);
          F2 = factorModuleBasis(J2)
        ///
     }

document {
        Key => {invReduce,(invReduce,Matrix,InvolutiveBasis),(invReduce,RingElement,InvolutiveBasis)},
        Headline => "compute normal form modulo involutive basis by involutive reduction",
        Usage => "(r,c) = invReduce(p,J)",
	Inputs => {
	     "p" => Matrix => "the columns are to be reduced modulo J",
	     "J" => InvolutiveBasis
	     },
        Outputs => {
	   "r" => Matrix => {"the normal form of (the columns of) ", TT "p", " modulo ", TT "J", ""},
	   "c" => Matrix => {"the reduction coefficients"}
	   },
	Consequences => { "the columns of r are in normal form modulo J, and p = r + J#0 * c, where * is matrix multiplication" },
        EXAMPLE lines ///
          R = QQ[x,y,z];
	  M = matrix {{x+y+z, x*y+y*z+z*x, x*y*z-1}};
	  J = janetBasis M;
	  p = matrix {{y,y^2,y^3}}
	  invReduce(p,J)
        ///,
        SeeAlso => {janetBasis,invSyzygies}
        }

document {
        Key => {invSyzygies,(invSyzygies,InvolutiveBasis)},
        Headline => "compute involutive basis of syzygies",
        Usage => "invSyzygies J",
        Inputs => {{ "J, ", ofClass InvolutiveBasis }},
        Outputs => {{ ofClass InvolutiveBasis, ", an involutive basis for the syzygies of J" }},
        EXAMPLE lines ///
          R = QQ[x,y,z];
          I = ideal(x,y,z);
          J = janetBasis I
          invSyzygies J
        ///,
        Caveat => { "cannot be iterated because ", TO "schreyerOrder", " is not used; call ", TO "janetResolution", " instead" },
        SeeAlso => {janetBasis,janetResolution}
        }

document {
     Key => InvolutiveBasis,
     Headline => "the class of all involutive bases"
     }

document {
     Key => FactorModuleBasis,
     Headline => "the class of all factor module bases"
     }

document {
        Key => Involutive,
        Headline => "compute a (usually non-minimal) resolution using involutive bases",
	PARA{
             "The symbol Involutive is allowed as value for the optional argument ", TO "Strategy", " for ", TO "resolution", ". If provided, the resolution is constructed using ", TO "janetResolution", "."
	    }
     }

document {
        Key => multVars,
        Headline => "key in the cache table of a differential in a Janet resolution",
	PARA{
             "The symbol multVars is used as a key in the cache table of a differential in a resolution if it is constructed using ", TO "janetResolution", ". In that case it stores the sets of multiplicative variables for the Janet basis given by that differential."
	    }
     }

document {
        Key => {janetResolution,(janetResolution,InvolutiveBasis),(janetResolution,Matrix),(janetResolution,Ideal),(janetResolution,Module)},
        Headline => "construct a free resolution for a given ideal or module using Janet bases",
        Usage => "C = janetResolution M",
        Inputs => {{ "M, ", ofClass Matrix, " or ", ofClass Ideal, " or ", ofClass Module }},
        Outputs => {
	   "C" => Complex => { "a (non-minimal) free resolution of (the module generated by) M" }
	   },
	PARA{
             "The computed Janet basis for each homological degree can be extracted with ", TO "janetBasis", "."
	    },
	PARA{
             "The sets of multiplicative variables can also be extracted from the Janet basis in each homological degree with ", TO "multVar", "."
	    },
	PARA{
             "Note that janetResolution can be combined with ", TO "resolution", ": when providing the option 'Strategy => Involutive' to ", TO "resolution", ", janetResolution constructs the resolution."
	    },
        EXAMPLE lines ///
          R = QQ[x,y,z];
	  M = matrix {{x,y,z}};
          C = janetResolution M
	  janetBasis(C, 2)
	  multVar(C, 2)
        ///,
        EXAMPLE lines ///
          R = QQ[x,y,z];
	  I = ideal(x,y,z);
	  freeResolution(I, Strategy => Involutive)
        ///,
        SeeAlso => {janetBasis,multVar,invSyzygies}
        }

TEST ///
     -- loadPackage "InvolutiveBases"
     R = QQ[f,e,d,c,b,a]
     M = matrix {{a*b*c, a*b*f, a*c*e, a*d*e, a*d*f, b*c*d, b*d*e, b*e*f, c*d*f, c*e*f}}
     S = janetResolution M
     assert ( length(S) == 4 )
     assert ( zero(S.dd_1 * S.dd_2) )
     assert ( zero(S.dd_2 * S.dd_3) )
     assert ( zero(S.dd_3 * S.dd_4) )
///

TEST ///
     -- loadPackage "InvolutiveBases"
     R = ZZ/2[f,e,d,c,b,a]
     M = matrix {{a*b*c, a*b*f, a*c*e, a*d*e, a*d*f, b*c*d, b*d*e, b*e*f, c*d*f, c*e*f}}
     S = janetResolution M
     assert ( length(S) == 4 )
     assert ( zero(S.dd_1 * S.dd_2) )
     assert ( zero(S.dd_2 * S.dd_3) )
     assert ( zero(S.dd_3 * S.dd_4) )
///

