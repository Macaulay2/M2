--		Copyright 1995 by Daniel R. Grayson and Michael Stillman

Module + Module := (M,N) -> (
     if ring M =!= ring N
     then error "expected modules over the same ring";
     R := ring M;
     if ambient M != ambient N
     or M.?relations and N.?relations and M.relations != N.relations
     or M.?relations and not N.?relations
     or not M.?relations and N.?relations
     then error "expected submodules of the same module";
     subquotient(
	  if not M.?generators or not N.?generators then null else M.generators | N.generators,
	  if M.?relations then M.relations else null
	  )
     )
document { (quote +, Module, Module),
     TT "M + N", " -- the sum of two submodules.",
     PARA,
     "The two modules should be submodules of the same module."
     }

Module ** Module := (M,N) -> (
     P := youngest(M,N);
     key := (M,N,quote **);
     if P#?key then P#key
     else M**N = (
	  if M.?generators and not isFreeModule N
	  or N.?generators and not isFreeModule M then (
	       if M.?generators then M = cokernel presentation M;
	       if N.?generators then N = cokernel presentation N;
	       );
	  R := ring M;
	  if R =!= ring N then error "expected modules over the same ring";
	  if isFreeModule M then (
	       if M == R^1 then N
	       else if isFreeModule N then (
		    if N == R^1 then M
		    else (
			 sendgg(ggPush M, ggPush N, ggmult);
			 new Module from R
			 )
		    )
	       else subquotient(
		    if N.?generators then M ** N.generators,
		    if N.?relations then M ** N.relations))
	  else (
	       if isFreeModule N then (
		    if N == R^1 then M
		    else subquotient(
			 if M.?generators then M.generators ** N,
			 if M.?relations then M.relations ** N))
	       else (
		    sendgg(ggPush M.relations, ggPush N.relations, ggmodtensor);
		    cokernel getMatrix R))))
document { (quote **, Module, Module),
     TT "M ** N", " -- produce the tensor product of two modules.",
     PARA,
     "Since M and N may be provided as submodules or subquotient modules, it
     may be necessary to replace them by quotient modules in the course of the
     computation, but the generators provided in the resulting tensor product 
     will correspond to the tensor products of the generators, i.e., the modules
     ", TT "cover M ** cover N", " and ", TT "cover(M ** N)", " are equal.
     This makes it easier to make ", TT "M ** N", " into a functor."
     -- i.e., we don't use 'prune'!
     }

TEST ///
    R = ZZ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    table(modules, modules, (P,Q) -> assert(cover P ** cover Q == cover (P ** Q)));
///

Matrix ** Module := (f,M) -> (
     P := youngest(f,M);
     key := (f,M,quote **);
     if P#?key then P#key
     else f**M = (
     	  f ** id_M
	  )
     )
Module ** Matrix := (M,f) -> (
     P := youngest(M,f);
     key := (M,f,quote **);
     if P#?key then P#key
     else M**f = (
     	  id_M ** f
	  )
     )
document { (quote **, Matrix, Module),
     TT "f ** N", " -- tensor product of a matrix f and a module N.",
     BR,NOINDENT,
     TT "N ** f", " -- tensor product of a matrix f and a module N.",
     PARA,
     "This is the same as tensoring f with the identity map of N.",
     PARA,
     "When ", TT "N", " is a free module of rank 1 the net effect of the
     operation is to shift the degrees of ", TT "f", ".",
     EXAMPLE {
	  "R = ZZ/101[t]",
      	  "f = matrix {{t}}",
      	  "degrees source f",
      	  "degrees source (f ** R^{-3})",
	  },
     SEEALSO {"Matrix", "Module"}
     }

-----------------------------------------------------------------------------
-- base change
-----------------------------------------------------------------------------
Module ** Ring := (M,R) -> (
     P := youngest(M,R);
     key := (M,R,quote **);
     if P#?key then P#key
     else M**R = (
	  k := ring M;
	  if k === R then M
	  else (
	       try promote(1_k, R) else error "can't tensor by this ring";
	       if M.?generators then coker presentation M ** R
	       else if M.?relations then cokernel (M.relations ** R)
	       else if isQuotientOf(R,k) then R^(- degrees M)
	       else R^(rank M)
	       )
	  ))
document { (quote **, Module, Ring),
     TT "M ** R", " -- form the tensor product of a module M with a ring
     R.",
     PARA,
     "The ring of M should be a base ring of R.",
     EXAMPLE {
	  "R = ZZ/101[x,y];",
      	  "M = coker vars R",
      	  "M ** R[t]"
	  },
     }

Matrix ** Ring := (f,R) -> (
     P := youngest(f,R);
     key := (f,R,quote **);
     if P#?key then P#key
     else f**R = (
	  k := ring source f;
	  S := ring target f;
	  if k === R and S === R then f
	  else if S === R then (
	       -- map(target f, (source f ** R) ** R^(-degree f), f)
	       map(target f, source f ** R, f, Degree => degree f)
	       )
	  else map(
	       -- this will be pretty slow
	       target f ** R, source f ** R, applyTable(entries f, r -> promote(r,R)),
	       Degree => if isQuotientOf(R,k) then degree f else degree 1_R
	       )
	  ))
document { (quote **, Matrix, Ring),
     TT "f ** R", " -- form the tensor product of a module map f with a ring R",
     PARA,
     "The ring of f should be a base ring of R.  The degree of the map is
     preserved.",
     EXAMPLE {
	  "R = ZZ/101[a..c]",
      	  "f = basis(2,R)",
	  },
     "A map of R-modules can be obtained by tensoring.",
     EXAMPLE "f ** R"
     }

-----------------------------------------------------------------------------       
gcdDegree Module := F -> (sendgg (ggPush F, gggcd); eePopIntarray());

lcmDegree Module := F -> (sendgg (ggPush F, gglcm); eePopIntarray());

document { quote poincareComputation,
     TT "poincareComputation", " -- a key used in a module or monomial
     ideal to store a computation of Poincare polynomial.",
     PARA,
     SEEALSO {"poincare"}
     }

poincare Module := M -> (
     R := ring M;
     if degreeLength R == 0
     then error "expected nonzero degree length";
     M = coker presentation M;
     -- if not isHomogeneous relations M then error "expected a homogeneous module";
     ZZn := degreesRing R;
     if not M.?poincare then (
	if not M.?poincareComputation then (
            g := generators gb presentation M;
	    sendgg(ggPush ZZn, ggPush g, gghilb);
	    M.poincareComputation = newHandle());
        sendgg(ggPush M.poincareComputation, ggPush (-1), ggcalc, ggpop);
            -- the last ggpop is to remove the return code.  MES: we should
            -- look at it first.
	sendgg(ggPush M.poincareComputation, gggetvalue);
        M.poincare = ZZn.pop());
     M.poincare)

hilbertFunction(ZZ,Module) :=
hilbertFunction(ZZ,Ring) :=
hilbertFunction(ZZ,Ideal) :=
hilbertFunction(List,Ring) := 
hilbertFunction(List,Ideal) := 
hilbertFunction(List,Module) := (d,M) -> (
     if class d === ZZ then (
     	  f := hilbertSeries(M, Order => d+1);
     	  U := monoid ring f;
     	  u := U_0;
     	  f_(u^d))
     else if class d === List and all(d,i->class i === ZZ) then (
	  -- hilbertSeries to finite order doesn't work yet for multi-degrees
	  -- we need more flexible power series handling functions
	  rank source basis(d,M)
	  )
     else error "expected degree to be an integer or list of integers")

TEST "
R = ZZ/101[a..d]
assert( hilbertFunction(3,R) === 20 )
assert( hilbertFunction(10,R) === 286 )
"

geometricSeries := (x,n) -> sum(n, i -> x^i)

trimm := (f,n) -> (
     ff := coefficients(toList(0 .. numgens ring f - 1), f);
     fm := ff#0;			  -- the monomials
     fc := ff#1;			  -- the coefficients
     p := positions(first entries fm, m -> max first exponents m < n);
     (fm_p * transpose fc_p)_(0,0)
     )

hilbertSeries PolynomialRing := options -> (R) -> hilbertSeries(R^1, options)
hilbertSeries Module := options -> (M) -> (
     if M#?{hilbertSeries} and options.Order == infinity
     then M#{hilbertSeries}
     else 
     if M#?{{hilbertSeries}} 
     and options.Order =!= infinity
     and M#{{hilbertSeries}}#1 >= options.Order
     then (
	  if M#{{hilbertSeries}}#1 === options.Order
	  then M#{{hilbertSeries}}#0
	  else trimm(M#{{hilbertSeries}}#0,options.Order)
	  )
     else (
	  A := ring M;
	  num := poincare M;
	  T := degreesRing A;
	  denom := new MutableHashTable from (
	       sort pairs tally apply(generators A, x -> degree x));
	  if options.Order === infinity 
	  then M#{hilbertSeries} = (
	       uf := 0;
	       while 0 == substitute(num, T_0 => 1)
	       do (
		    num = num // (1 - T_0);
		    if denom#?{1} and denom#{1} > 0
		    then (
			 denom#{1} = denom#{1} - 1;
			 if denom#{1} == 0 then remove(denom,{1});
			 )
		    else (
			 uf = uf + 1;
			 )
		    );
	       Divide {
		    if uf == 0 
		    then num
		    else Product {num, Power{1 - T_0, uf}},
		    Product apply(
			 sort pairs denom,
			 (i,e) -> Power{ 1 - product(#i, j -> T_j ^ (i_j)), e }
			 )
		    }
	       )
	  else if class options.Order === ZZ 
	  then first (
	       M#{{hilbertSeries}} = {
		    if num == 0
		    then 0_T
		    else (
			 m := min min(listForm num / first);
			 n := options.Order;
			 N := n - m;
			 f := num * product apply(
			      pairs denom,
			      (i,e) -> (geometricSeries(
				   product(#i, j -> T_j ^ (i_j)),
				   N)) ^ e
			      );
			 trimm(f,n)),
	       options.Order})
	  else error "expected an integer as value of Order option"
	  ))

document { quote hilbertFunction,
     TT "hilbertFunction(d,M)", " -- compute the dimension of the degree d
     part of the module, ring, or ideal M",
     PARA,
     "At the moment, the function is computed simply by calling ", TO "basis", "
     and extracting the number of basis elements.",
     SEEALSO {"hilbertSeries", "hilbertPolynomial"}
     }

TEST ///
R = ZZ/101[x,y]
M = R^1/x
T = degreesRing R
t = T_0
assert( hilbertSeries (M, Order => 5) == t^4+t^3+t^2+t+1 )
assert( hilbertSeries (M, Order => 4) == t^3+t^2+t+1 )
assert( hilbertSeries (M, Order => 7) == t^6+t^5+t^4+t^3+t^2+t+1 )
///

document { quote Order,
     TT "Order", " -- an optional argument used with ", TO "hilbertSeries", "
     to specify the order of the series requested."
     }

document { quote hilbertSeries,
     TT "hilbertSeries M", " -- compute the Hilbert series of the ring or
     module M.",
     PARA,
     "The Hilbert series is the formal power series in the variables of the
     degrees ring whose coefficients are the dimensions of the corresponding
     graded component.  The series is provided as an ", TO "Expression", "
     representing a rational function with that series.",
     PARA,
     "If an optional integer argument labelled ", TO "Order", " is used, then
     the power series is expanded to that order.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
      	  "hilbertSeries(R/x^2)",
      	  "numerator oo",
      	  "value oo",
      	  "poincare (R/x^2)",
      	  "hilbertSeries(R/x^2, Order => 12)",
	  },
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
      	  "hilbertSeries (R/x^2)",
	  },
     SEEALSO {"degreesRing", "Order"}
     }

ProjectiveHilbertPolynomial = new Type of HashTable
document { quote ProjectiveHilbertPolynomial,
     TT "ProjectiveHilbertPolynomial", " -- the class of all Hilbert
     polynomials expressed in terms of the Hilbert polynomials of projective
     space.",
     PARA,
     "Functions which produce projective Hilbert polynomials:",
     MENU {
	  TO "hilbertPolynomial",
	  TO "projectiveHilbertPolynomial"
	  },
     "Functions on projective Hilbert polynomials:",
     MENU {
	  (TO "diff", "   -- compute the difference polynomial P(n)-P(n-1)."),
	  (TO "degree", " -- degree of a projective Hilbert polynomial"),
	  (TO "dim", "    -- dimension of a projective Hilbert polynomial"),
	  (TO "euler", "  -- compute the Euler characteristic P(0)"),
	  (TO "+", "      -- add two projective Hilbert polynomials"),
	  (TO "-", "      -- subtract two projective Hilbert polynomials"),
	  (TO "*", "      -- multiply a projective Hilbert polynomial by an integer"),
	  (TO (quote " ", ProjectiveHilbertPolynomial, ZZ), "   -- evaluate a projective Hilbert polynomial at an integer")
	  },
     "The functions ", TO "degree", " and ", TO "dim", " are designed so they
     correspond the degree and dimension of the algebraic variety that may have
     been used to produce the Hilbert polynomial."
     }

ProjectiveHilbertPolynomial ZZ := (P,i) -> sum(pairs P, (n,c) -> c * binomial(n+i,n))
document { (quote " ", ProjectiveHilbertPolynomial, ZZ),
     TT "P i", " -- the value of a projective Hilbert polynomial ", TT "P", " at 
     an integer ", TT "i", ".",
     PARA,
     EXAMPLE {
	  "P = projectiveHilbertPolynomial 2",
      	  "apply(0 .. 12, i -> P i)",
	  },
     SEEALSO ProjectiveHilbertPolynomial
     }

euler ProjectiveHilbertPolynomial := (P) -> P(0)
diff(ProjectiveHilbertPolynomial,ZZ) := (P,i) -> (
     new ProjectiveHilbertPolynomial from select(
     	  apply(pairs P, (n,c) -> (n-i,c)),
	  (n,c) -> n >= 0
	  ))
diff ProjectiveHilbertPolynomial := (P) -> diff(P,1)
ProjectiveHilbertPolynomial + ProjectiveHilbertPolynomial := (h,k) -> (
     select( merge(h,k,plus), c -> c =!= 0 )
     )
- ProjectiveHilbertPolynomial := h -> applyValues(h,minus)
ProjectiveHilbertPolynomial - ProjectiveHilbertPolynomial := (h,k) -> h + -k
ProjectiveHilbertPolynomial == ProjectiveHilbertPolynomial := (h,k) -> h === k
dim ProjectiveHilbertPolynomial := (P) -> if #P === 0 then -1 else max keys P
degree ProjectiveHilbertPolynomial := (P) -> if #P === 0 then 0 else P#(dim P)
ZZ * ProjectiveHilbertPolynomial := (b,h) -> (
     if b === 1 then h 
     else if b === 0 then new ProjectiveHilbertPolynomial from {}
     else applyValues(h,c -> b*c)
     )

PPP := new Holder from {"P"}
expression ProjectiveHilbertPolynomial := (h) -> (
     sum(sort pairs h, (n,c) -> c * new Subscript from {PPP, n})
     )	  
net ProjectiveHilbertPolynomial := (h) -> net expression h

projectiveHilbertPolynomial = method()
projectiveHilbertPolynomial ZZ := (n) -> (
     new ProjectiveHilbertPolynomial from { n => 1 }
     )
projectiveHilbertPolynomial(ZZ,ZZ) := memoize(
     (n,d) -> new ProjectiveHilbertPolynomial from (
     	  if d <= 0 
	  then apply(min(-d+1,n+1), j -> n-j => (-1)^j * binomial(-d,j))
     	  else apply(n+1, j -> n-j => binomial(d-1+j,j))))

document { quote projectiveHilbertPolynomial,
     TT "projectiveHilbertPolynomial n", " -- produces the projective
     Hilbert polynomial corresponding to projective space of dimension n.",
     BR,NOINDENT,
     TT "projectiveHilbertPolynomial(n,d)", " -- produces the projective
     Hilbert polynomial corresponding to the graded ring of projective space
     of dimension n, but with its generator in degree -d.",
     PARA,
     SEEALSO "ProjectiveHilbertPolynomial"
     }

i := quote i
hilbertFunctionRing := QQ[i]
hilbertFunctionQ := method()
hilbertFunctionQ(ZZ) := (n) -> (
     if n === 0 then 1_hilbertFunctionRing
     else (1/n) * (n+i) * hilbertFunctionQ(n-1))
hilbertFunctionQ(ZZ,ZZ) := memoize(
     (n,d) -> (
     	  if d === 0 then hilbertFunctionQ(n)
     	  else substitute(hilbertFunctionQ(n), {i => i+d})))

hilbertPolynomial Module := options -> (M) -> (
    if degreeLength ring M != 1 
    then error "expected a singly graded ring";
    n := numgens ring M - 1;
    f := poincare M;
    T := (ring f)_0;
    p := pairs standardForm f;
    if options.Projective 
    then (
	 if #p===0 
	 then new ProjectiveHilbertPolynomial from {}
	 else sum(p, (d,c) -> (
	      	   if #d === 0 then d = 0 else d = d#0;
	      	   c * projectiveHilbertPolynomial(n,-d))))
    else (
	 if #p===0
	 then 0_hilbertFunctionRing
	 else sum(p, (d,c) -> (
	      	   if #d === 0 then d = 0 else d = d#0;
	      	   c * hilbertFunctionQ(n,-d)))))

hilbertPolynomial Ring := options -> (R) -> hilbertPolynomial(R^1, options)

TEST "
scan(3, n -> scan(-3 .. 3, d -> (
	       h := projectiveHilbertPolynomial(n,d);
	       scan(3, i -> assert( h i === binomial(n+d+i,n) )))))
"

TEST "
scan(3, n -> (
     R = ZZ/101[x_0 .. x_n];
     scan(-2 .. 2, d -> (
	  M = R^{-d};
	  h = hilbertPolynomial M;
	  scan(d .. d + 4, e -> assert(numgens source basis(e,M) == h e))))))
"
TEST "
scan(3, n -> (
     R = ZZ/101[x_0 .. x_n];
     scan(-2 .. 2, d -> (
	  M = R^{-d};
	  h = hilbertPolynomial (M, Projective => false);
	  i = (ring h)_0;
	  scan(d .. d + 4, e -> (
		    r = numgens source basis(e,M);
		    s = substitute(h, { i => e/1 });
	       	    assert( r == s)))))))
"

document { quote Projective,
     TT "Projective => true", " -- an option to ", TO "hilbertPolynomial", " which
     specifies that the Hilbert polynomial produced should be expressed in terms
     of the Hilbert polynomials of projective spaces.  This is the default.",
     BR, NOINDENT,
     TT "Projective => false", " -- an option to ", TO "hilbertPolynomial", " which
     specifies that the Hilbert polynomial produced should be expressed as a 
     polynomial in the degree.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "S = image map(R, R, {a^4, a^3*b, a*b^3, b^4})",
      	  "presentation S",
      	  "h = hilbertPolynomial S",
	  },
     PARA,
     "The rational quartic curve in P^3 is therefore 'like' 4 copies of P^1, with
     three points missing.  One can see this by noticing that there is a deformation
     of the rational quartic to the union of 4 lines, or 'sticks', which intersect
     in three successive points.",
     PARA,
     "These Hilbert polynomials can serve as Hilbert functions, too.",
     EXAMPLE {
	  "h 3",
      	  "basis(3,S)",
      	  "rank source basis(3,S)",
	  },
     PARA,
     "Note that the Hilbert polynomial of P^i is z |--> binomial(z + i, i).",
     PARA,
     SEEALSO "ProjectiveHilbertPolynomial"
     }

document { quote hilbertPolynomial,
     TT "hilbertPolynomial M", " -- the Hilbert polynomial of the module M as
     a polynomial in T.",
     PARA,
     "Options:",
     MENU {
	  TO "Projective"
	  },
     PARA,
     }

Ideal * Ring := (I,S) -> if ring I === S then I else ideal(I.generators ** S)
Ring * Ideal := (S,I) -> if ring I === S then I else ideal(I.generators ** S)

ZZ == Ideal := (n,I) -> I == n
Ideal == ZZ := (I,n) -> (
     if n === 0
     then I.generators == 0
     else if n === 1
     then 1_(ring I) % I == 0
     else error "attempted to compare ideal to integer not 0 or 1"
     )
ZZ == Module := (n,M) -> M == n
Module == ZZ := (M,n) -> (
     if n =!= 0 then error "attempted to compare module to nonzero integer";
     if M.?generators then (
	  if M.?relations then M.generators % M.relations == 0
	  else M.generators == 0
	  )
     else (
	  if M.?relations then (
	       f := M.relations;
	       id_(target f) % f == 0
	       )
	  else M.numgens === 0
	  )
     )


dim Module := M -> (
     if degreeLength ring M === 0 
     then error "can't compute dimension over a ring with zero degree length";
     if not isHomogeneous M
     then M = cokernel leadTerm gens gb presentation M;
     if poincare M == 0
     then -1
     else 1 + dim hilbertPolynomial M
     )

TEST "
R = ZZ/101[a..f]
assert( dim image matrix {{a,b}} == 6 )
assert( dim coker matrix {{a,b}} == 4 )
assert( dim coker matrix {{a-1,b-c^2}} == 4 )
assert( dim ideal (a,b) == 4 )
assert( codim ideal (a,b) == 2 )
assert( dim R == 6 )
assert( dim (R/a) == 5 )
"

codim Module := M -> dim ring M - dim M
document { quote codim,
     TT "codim M", " -- calculate the codimension of the support of a module ", TT "M", ".",
     BR,NOINDENT,
     TT "codim I", " -- calculate the codimension of the quotient ring ", TT "R/I", ".",
     PARA,
     "If ", TT "M", " is an ", TT "R", "-module, then the number return by this 
     routine is ", TT "dim R - dim M", ".  This does not agree with the usual
     definition of codimension unless ", TT "Spec R", " is irreducible.",
     EXAMPLE {
	  "R = QQ[x,y]/(ideal(x,y) * ideal(x-1))",
      	  "codim (R^1/(x,y))"
	  },
     }

document { quote dim,
     TT "dim M", " -- calculate the dimension of the support of a module M.",
     BR,NOINDENT,
     TT "dim R", " -- calculate the dimension of a ring R.",
     BR,NOINDENT,
     TT "dim I", " -- calculate the dimension of the quotient ring R/I.",
     BR,NOINDENT,
     TT "dim r", " -- calculate the dimension of the virtual representation
     corresponding to an element of a Schur ring.",
     PARA,
     SEEALSO {"Schur"}
     }

degree Module := M -> (
  hf := poincare M;
  T := (ring hf)_0;
  if hf == 0 then 0
  else (
       while substitute(hf,{T=>1}) == 0 do hf = hf // (1-T);
       substitute(hf,{T=>1})))

-----------------------------------------------------------------------------
document { quote presentation,
     TT "presentation M", " -- produce a presentation of the module M.",
     BR,NOINDENT,
     TT "presentation R", " -- produce a presentation of the quotient ring R.",
     PARA,
     "A presentation of ", TT "M", " is a map ", TT "p", " so that ", TT "coker p", " is 
     isomorphic to ", TT "M", ".  The presentation obtained is expressed in 
     terms of the given generators, i.e., the modules ", TT "cover M", " and 
     ", TT "target p", " are identical.
     The isomorphism can be obtained as ", TT "map(M,coker p,1)", ".",
     PARA,
     "Since a module M may be described as a submodule or a subquotient 
     module of a free module, some computation may be required to produce 
     a presentation.  See also ", TO "prune", " which does a bit more work to try to
     eliminate redundant generators.",
     PARA,
     "For a quotient ring R, the result is a matrix over the ultimate
     ambient polynomial ring, whose image is the ideal defining R.  The 
     entries of the matrix form a Groebner basis.",
     SEEALSO {"cover"}
     }

TEST ///
    R = ZZ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    scan(modules, M -> assert( cover M == target presentation M ) )
///

presentation(Module) := M -> (
     if M.?presentation then M.presentation else M.presentation = (
	  if M.?generators then (
	       modulo( M.generators, if M.?relations then M.relations)
	       )
	  else relations M))
-----------------------------------------------------------------------------  
document { quote prune,
     TT "prune M", " -- replace M by an isomorphic module with a minimal number
     of generators and relations.",
     BR,NOINDENT,
     TT "prune f", " -- replace f by an isomorphic map of modules by
     pruning its source and target.",
     PARA,
     "The isomorphism from ", TT "N = prune M", " back to ", TT "M", " can 
     be obtained with code such as ", TT "g = N.pruningMap", " unless ", TT "M.pruningMap", "
     already exists, in which case ", TT "N", " is the same as ", TT "M", ".  You may obtain 
     the inverse isomorphism with ", TT "g^-1", ".",
     PARA,
     SEEALSO {"presentation", "trim", "pruningMap"}
     }

document { quote pruningMap,
     TT "pruningMap", " -- the key under which is stored the isomorphism to
     a module ", TT "M", " from the module ", TT "prune M", ".",
     PARA,
     "This map exists only after ", TT "N = prune M", " has been executed
     at least once, and then the map can be obtained with ", TT "N.pruningMap", ".",
     SEEALSO "prune"
     }

prune(Module) := M -> (
     if M.?pruningMap then M
     else if M.?prune then M.prune else M.prune = (
	  if isFreeModule M then (
	       M.pruningMap = id_M;
	       M)
	  else if isHomogeneous M and isAffineRing ring M then (
	       f := presentation M;
	       g := complement f;
	       N := cokernel modulo(g, f);
	       N.pruningMap = map(M,N,g);
	       N)
	  else (
	       f = gens gb presentation M;
	       -- MES: can't it do more here?
	       N = cokernel f;
	       N.pruningMap = map(M,N,id_(cover M));
	       N)
	  )
     )

prune(Matrix) := (m) -> (
     M := source m;
     if not M.?pruningMap then m = m * (prune M).pruningMap;
     N := target m;
     if not N.?pruningMap then m = (prune N).pruningMap^-1 * m;
     m)

TEST "
r = ZZ/101[a,b]
assert ( 2 * degree (a * b^2) === {6} )
M = cokernel matrix (r,{{1}})
assert ( isFreeModule prune M )
"

TEST "
GF(8,Variable => x)
assert ( det matrix{{x,1},{x^2,x^3}} == x^4 - x^2 )
"

TEST "
R = ZZ/101[a..f]

M = cokernel matrix (R, {{1},{-1}})
N = prune M
p = N.pruningMap

assert( source p == N )
assert( target p == M )
assert( prune kernel p == 0 )
assert( prune cokernel p == 0 )
assert isIsomorphism p
assert isIsomorphism p^-1
assert ( p * p^-1 == id_M )
assert ( p^-1 * p == id_N )
"
-----------------------------------------------------------------------------
document { quote dual,
     TT "dual M", " -- the dual.",
     PARA,
     "For details, see one of the following.",
     MENU {
	  TO (dual,ChainComplex),
	  TO (dual,Matrix),
	  TO (dual,Module)
	  }
     }

dual Module := F -> if F.?dual then F.dual else F.dual = (
     if not isFreeModule F then kernel transpose presentation F
     else (
	  sendgg (ggPush F, ggtranspose); 
	  new Module from ring F))
document { (dual, Module),
     TT "dual M", " -- the dual of a module."
     }
-----------------------------------------------------------------------------
Hom(Ideal, Ideal) := (I,J) -> Hom(module I, module J)
Hom(Ideal, Module) := (I,M) -> Hom(module I, M)
Hom(Module, Ideal) := (M,I) -> Hom(M, module I)

Hom(Module, Ring) := (M,R) -> Hom(M, R^1)
Hom(Ring, Module) := (R,M) -> Hom(R^1, M)
Hom(Ideal, Ring) := (I,R) -> Hom(module I, R^1)
Hom(Ring, Ideal) := (R,I) -> Hom(R^1, module I)

Hom(Module, Module) := (M,N) -> (
     if isFreeModule M 
     then dual M ** N
     else kernel Hom(presentation M, N)
     )
-- An alternate Hom routine:
Hom(Module, Module) := (M,N) -> (
     -- This version is perhaps less transparent, but is
     -- easier to determine the link with homomorphisms.
     m := presentation M;
     mdual := transpose m;
     n := presentation N;
     h1 := modulo(mdual ** target n, target mdual ** n);
     MN := trim subquotient(h1,source mdual ** n);
     -- Now we store the information that 'homomorphism'
     -- will need to reconstruct the map corresponding to
     -- an element.
     MN.Hom = {M,N,source mdual,target n};
     MN)

homomorphism = method()
homomorphism Matrix := (f) -> (
     if not isFreeModule(source f) or 
        numgens source f =!= 1 or
        not (target f).?Hom
	then error "homomorphism may only be determined for maps R --> Hom(M,N)";
     MN := (target f).Hom;
     M := MN#0;
     N := MN#1;
     M0 := MN#2;
     N0 := MN#3;
     deg := (degrees source f)#0;
     map(N,M,adjoint1(super f, M0, N0),Degree=>deg))

document { quote homomorphism,
     TT "homomorphism f", " -- finds the matrix M <-- N corresponding to the 
     element f.",
     PARA,
     "This element should be a matrix f : Hom(M,N) <--- R^1, where Hom(M,N) 
     has been previously computed, and R is the ring of M and N.",
     PARA,
     "When A := Hom(M,N) is computed, enough information is stored in A.Hom
     to compute this correspondence.",
     PARA,
     SEEALSO "Hom"
     }

TEST ///
S = ZZ/101[a..d]
I = monomialCurve(S, {1,3,4})
R = S/I
use R
J = module ideal(a,d)
K = module ideal(b^2,c^2)

JK = Hom(J,K)
F = JK_{0}
F1 = homomorphism F
source F1
target F1
ker F1
prune coker F1
///
-----------------------------------------------------------------------------
pdim Module := M -> max select(keys complete resolution M, i -> class i === ZZ)
document { quote pdim,
     TT "pdim M", " -- calculate the projective dimension of a module M.",
     PARA,
     "For now, the method is to measure the length of a projective resolution."
     }

Module / Module := (M,N) -> (
     L := ambient M;
     if L != ambient N then error "expected modules with the same ambient module";
     R := ring M;
     if N.?generators
     then (
	  p := N.generators;
	  if M.?relations then (
	       p = p | M.relations;
	       );
	  subquotient(
	       if M.?generators then M.generators,
	       -- mingens image -- do we need this ???
	       p))
     else image id_L)

document { (quote /, Module, Module),
     TT "M/N", " -- computes the quotient module ", TT "M/N", ".",
     PARA,
     "The modules should be submodules of the same module."
     }

document { (quote /, Module, Ideal),
     TT "M/I", " -- computes the quotient module ", TT "M/IM", ",
     where ", TT "M", " is a module and ", TT "I", " is an ideal.",
     PARA,
     "The module and ideal should belong to the same ring."
     }

document { (quote /, Ideal, Ideal),
     TT "I/J", " -- produces the quotient module ", TT "(I+J)/J", ", where
     ", TT "I", " and ", TT "J", " are ideals in a ring.",
     PARA,
     SEEALSO "Module"
     }

Module / RingElement := (M,x) -> M / (x * M)
Module / Sequence := Module / List := (M,v) -> (
     R := ring M;
     v = toList v;
     if all(v, w -> class w === M)
     then M / image matrix v
     else if all(v, w -> class w == R)
     then M / (ideal v * M)
     else error("expected a list of elements of ", name M, " or of ", name R)
     )
Module / Vector := (M,v) -> (
     if class v =!= M 
     then error("expected ", name v, " to be an element of ", name M);
     M / image matrix {v})
-----------------------------------------------------------------------------
--top Module := M -> (
--     R := ring M;
--     c := codim M; 
--     annihilator prune Ext^c(M, R))
--document { quote top,
--     TT "top M", " -- produce the annihilator of Ext^c(M, R), where c
--     is the codimension of the support of the module M."
--     }
-----------------------------------------------------------------------------

ann = annihilator
document { quote ann,
     TT "ann", " -- a synonym for ", TO "annihilator", "."
     }

annihilator Module := M -> (
     f := presentation M;
     image f : target f )
annihilator Ideal := I -> annihilator module I
annihilator RingElement := f -> annihilator ideal f
document { quote annihilator,
     TT "annihilator M", " -- produce the annihilator ideal of a 
     module, ideal, ring element, or coherent sheaf.",
     PARA,
     "For an abbreviation, use ", TO "ann", "."
     }
-----------------------------------------------------------------------------

ZZ _ Module := (i,M) -> (
     if i === 0 then M#0
     else error "expected integer to be zero"
     )

Module _ ZZ := (M,i) -> (
     if M.?generators then (
	  if i < 0 or i >= rank source M.generators
	  then error ("subscript '", name i, "' out of range");
	  sendgg (ggPush M.generators, ggPush i, ggelem);
	  new M)
     else (
	  if i < 0 or i >= M.numgens 
	  then error ("subscript '", name i, "' out of range");
     	  sendgg(ggPush M, ggPush i, ggfromint);
     	  new M)
     )

document { (quote _, Module, ZZ),
     TT "M_i", " -- get the ", TT "i", "-th generator of a module ", TT "M", "",
     PARA,
     EXAMPLE "(ZZ^5)_2"
     }

-----------------------------------------------------------------------------
Module ^ Array := (M,w) -> if M#?(quote ^,w) then M#(quote ^,w) else M#(quote ^,w) = (
     -- we don't splice any more because natural indices include pairs (i,j).
     w = toList w;
     if not M.?components then error "expected a direct sum module";
     if M.?indexComponents then (
	  ic := M.indexComponents;
	  w = apply(w, i -> if ic#?i 
		    then ic#i 
		    else error "expected an index of a component of a direct sum"));
     -- if the components of M have 3,4,5 generators, then
     -- we want to construct { (0,1,2), (3,4,5,6), (7,8,9,10,11) } for quick access
     k := 0;
     v := apply(M.components, N -> k .. (k = k + numgens N) - 1);
     map(directSum M.components_w, M, (cover M)^(splice apply(w, i -> v#i))))

Module _ Array := (M,w) -> if M#?(quote _,w) then M#(quote _,w) else M#(quote _,w) = (
     -- we don't splice any more because natural indices include pairs (i,j).
     w = toList w;
     if not M.?components then error "expected a direct sum module";
     if M.?indexComponents then (
	  ic := M.indexComponents;
	  w = apply(w, i -> if ic#?i 
		    then ic#i 
		    else error "expected an index of a component of a direct sum"));
     -- if the components of M have 3,4,5 generators, then
     -- we want to construct { (0,1,2), (3,4,5,6), (7,8,9,10,11) } for quick access
     k := 0;
     v := apply(M.components, N -> k .. (k = k + numgens N) - 1);
     map(M, directSum M.components_w, (cover M)_(splice apply(w, i -> v#i))))

document { (quote ^,Module,Array),
     TT "M^[i,j,k]", " -- projection onto some factors of a direct sum module.",
     PARA,
     "The module ", TT "M", " should be a direct sum, and the result is the matrix
     obtained by projection onto the sum of the components numbered
     ", TT "i, j, k", ".  Free modules are regarded as direct sums.",
     PARA,
     EXAMPLE {
	  "M = ZZ^2 ++ ZZ^3",
      	  "M^[0]",
      	  "M^[1]",
      	  "M^[1,0]",
	  },
     SEEALSO {(quote ^,Matrix,Array), (quote _,Module,Array),(quote ^,Module,List)}
     }

document { (quote _,Module,Array),
     TT "M_[i,j,k]", " -- extract some columns of blocks from a matrix ", TT "f", ".",
     PARA,
     "The module ", TT "M", " should be a direct sum, and the result is the matrix
     obtained by inclusion from the sum of the components numbered
     ", TT "i, j, k", ".  Free modules are regarded as direct sums.",
     PARA,
     EXAMPLE {
	  "M = ZZ^2 ++ ZZ^3",
      	  "M_[0]",
      	  "M_[1]",
      	  "M_[1,0]",
	  },
     SEEALSO {submatrix, (quote _,Matrix,Array), (quote ^,Module,Array),(quote _,Module,List)}
     }
-----------------------------------------------------------------------------
Module ^ List := (M,rows) -> submatrix(id_M,rows,)
document { (quote ^, Module, List),
     TT "M^{i,j,k,...}", " -- provides the projection map from a free module
     ", TT "M", " to the free module corresponding to the basis vectors whose
     index numbers are listed.",
     PARA,
     EXAMPLE "(ZZ^5)^{2,3}",
     SEEALSO {quote _, Module, List}
     }
-----------------------------------------------------------------------------
Module _ List := (M,v) -> (
     N := cover M;
     f := id_N_v;
     map(M, source f, f))
document { (quote _, Module, List),
     TT "M_{i,j,k,...}", " -- provides a map from a free module to the module
     ", TT "M", " which sends the basis vectors to the generators of ", TT "M", "
     whose index numbers are listed.",
     PARA,
     EXAMPLE "(ZZ^5)^{2,3}",
     SEEALSO {quote ^, Module, List}
     }
-----------------------------------------------------------------------------
basis(List,Module) := (deg,M) -> (
     if #deg =!= degreeLength ring M then error "expected degree length to match that of ring";
     R := ring M;
     A := ultimate(ambient,R);
     if not (
	  isAffineRing A 
	  or
	  isPolynomialRing A and isField coefficientRing A and (options A).SkewCommutative
	  or
	  isPolynomialRing A and ZZ === coefficientRing A
	  or
	  ZZ === A
	  ) then error "'basis' can't handle this type of ring";
     k := coefficientRing A;
     bottom := generators gb presentation M;
     top := id_(target bottom);
     sendgg(ggPush top, ggPush bottom, ggPush deg, ggkbasis, 
	  ggdup, ggPush deg, ggsetshift);
     p := new Matrix;
     p.target = M;
     sendgg(ggdup,gglength);
     p.source = k^(eePopInt());
     p.handle = newHandle "";
     p)

TEST "
	R = ZZ/101[a..d]
	f = matrix{{a,b},{c,d}}
	g = matrix(R,{{1},{0}})
	M = subquotient(g,f)
	assert( numgens source basis(3,M) == 16 )
"

basis(ZZ,Module) := (deg,M) -> basis({deg},M)
basis(List,Ideal) := basis(ZZ,Ideal) := (n,I) -> basis(n,module I)
basis(List,Ring) := (deg,R) -> basis(deg, R^1)

basis(ZZ,Ring) := (deg,R) -> basis({deg}, R^1)

basis Module := M -> (
     -- check the following:
     --     R = ring m is a polynomial ring
     --     
     R := ring M;
     A := ultimate(ambient,R);
     if not isField coefficientRing A then error "expected an algebra over a field";
     k := coefficientRing A;
     bottom := generators gb presentation M;
     top := id_(target bottom);
     sendgg(ggPush top, ggPush bottom, ggkbasis);
     p := new Matrix;
     p.target = M;
     sendgg(ggdup,gglength);
     p.source = k^(eePopInt());
     p.handle = newHandle "";
     p)

basis Ring := R -> basis(R^1)
basis Ideal := I -> basis module I

document { quote basis,
     TT "basis(i,M)", " -- produce a map (of degree ", TT "i", ") from a free ", TT "k", "-module 
     to ", TT "M", " whose image is the degree ", TT "i", " part of the module (or ring) ", TT "M", ".",
     BR, NOINDENT,
     TT "basis M", " -- produce a map from a free ", TT "k", "-module to ", TT "M", " whose image
     is the finite dimensional module (or ring) ", TT "M", ".",
     PARA,
     "The field ", TT "k", " is the coefficient ring of the ring of ", TT "M", ".  The degree
     ", TT "i", " may be a multi-degree, represented as a list of integers.",
     PARA,
     "Alternatively, if the coefficient ring of the ring of ", TT "M", " is ", TT "ZZ", ", then the
     basis returned is a basis only modulo torsion.",
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "f = basis(2,R)",
	  },
     "A map of R-modules can be obtained by tensoring.",
     EXAMPLE {
	  "f ** R",
      	  "basis(2, ideal(a,b,c)/ideal(a^2,b^2,c^2))",
      	  "basis(R/(a^2-a*b, b^2-c^2, b*c))",
	  },
     EXAMPLE {
      	  "S = ZZ/101[x,y,z,Degrees=>{{1,3},{1,4},{1,-1}}]",
      	  "basis({7,24}, S)",
	  },
     "Here is another example.",
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "f = basis(3, ideal(a^2, b^2))",
	  },
     "Notice that the matrix of ", TT "f", " is expressed in terms of the
     generators of the ideal.  The reason is that the ideal is the target
     of ", TT "f", ", and matrices are always expressed in terms of the
     generators of the target.",
     EXAMPLE "target f",
     "The command ", TO "super", " is useful for getting around this.",
     EXAMPLE "super f"
     }

-----------------------------------------------------------------------------

truncate(List,Ideal) := (deg,I) -> ideal truncate(deg,module I)

truncate(List,Module) := (deg,M) -> (
     -- check the following:
     --     R = ring m is a polynomial ring
     --     
     R := ring M;
     F := ambient M;
     top := (
	  if M.?generators then generators gb (
	       if M.?relations then M.generators | M.relations else M.generators
	       )
	  else id_F
	  );
     bottom := (
	  if M.?relations 
	  then generators gb M.relations 
	  else map(F, R^0, 0)
	  );
     sendgg(ggPush top, ggPush bottom, ggPush deg, ggtruncate);
     subquotient(getMatrix R, if M.?relations then M.relations))

truncate(ZZ,Module) := truncate(ZZ,Ideal) := (deg,M) -> truncate({deg},M)

document { quote truncate,
     TT "truncate", " (i,M) -- yields the submodule of M consisting of all 
     elements of degrees >= i.  If i is a multi-degree, then this yields the
     submodule generated by all elements of degree exactly i, together with
     all generators which have a higher primary degree than that of i.",
     PARA,
     "The degree i may be a multi-degree, represented as a list of integers.
     The ring of M should be a (quotient of a) polynomial ring, 
     where the coefficient ring, k, is a field.",
     PARA,
     "Caveat: if the degrees of the variables are not all one, then there is
     currently a bug in the routine: some generators of higher degree than i
     may be duplicated in the generator list",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "truncate(2,R^1)",
      	  "truncate(2, ideal(a,b,c^3)/ideal(a^2,b^2,c^4))",
	  },
     EXAMPLE {
      	  "S = ZZ/101[x,y,z,Degrees=>{{1,3},{1,4},{1,-1}}];",
      	  "truncate({7,24}, S^1 ++ S^{{-8,-20}})"
	  },
     }
-----------------------------------------------------------------------------

TEST "
R=ZZ/101[a..f]
assert( degrees( R^{1} ++ R^{2} ) == {{-1}, {-2}} )
assert( degrees (R^{1,2} ** R^{3,5}) == {{-4}, {-6}, {-5}, {-7}} )
assert( numgens R^6 == 6 )
assert( rank R^6 == 6 )
f = vars R
M = cokernel (transpose f * f)
assert ( rank M == 5 )
assert ( rank kernel f == 5 )
assert ( rank cokernel f == 0 )
assert(R^{0,0} == R^2)
assert(R^{0,0} != R^{0,1})
"

issub := (f,g) -> (
     g = gb g;
     sendgg(ggPush f, ggPush g, ggissubset);
     -1 == ZZ.pop())

isSubset(Module,Module) := (M,N) -> (
     -- here is where we could use gb of a subquotient!
     ambient M == ambient N and
     if M.?relations and N.?relations then (
	  image M.relations == image N.relations
	  and
	  issub(M.relations | gens M, N.relations | gens N))
     else if not M.?relations and not N.?relations then (
	  issub(gens M, gens N))
     else false
     )
isSubset(Ideal,Ideal) := (I,J) -> isSubset(module I, module J)
isSubset(Module,Ideal) := (M,J) -> isSubset(M, module J)
isSubset(Ideal,Module) := (I,N) -> isSubset(module I, N)

