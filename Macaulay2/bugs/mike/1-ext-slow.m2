Exty = method()
Exty(Module,Module) := Module => (M,N) -> (
  B := ring M;
  p := presentation B;
  A := ring p;
  I := ideal mingens ideal p;
  n := numgens A;
  c := numgens I;
  if c =!= codim B 
  then error "total Ext available only for complete intersections";
  f := apply(c, i -> I_i);
  pM := lift(presentation M,A);
  pN := lift(presentation N,A);
  M' := cokernel ( pM | p ** id_(target pM) );
  N' := cokernel ( pN | p ** id_(target pN) );
time  C := complete resolution M';
  X := getGlobalSymbol "X";
  K := coefficientRing A;
  -- compute the fudge factor for the adjustment of bidegrees
  fudge := if #f > 0 then 1 + max(first \ degree \ f) // 2 else 0;
  S := K(monoid [X_1 .. X_c, toSequence A.generatorSymbols,
    Degrees => {
      apply(0 .. c-1, i -> {-2, - first degree f_i}),
      apply(0 .. n-1, j -> { 0,   first degree A_j})
      },
    Heft => {-fudge, 1}
    ]);
  -- make a monoid whose monomials can be used as indices
  Rmon := monoid [X_1 .. X_c,Degrees=>{c:{2}}];
  -- make group ring, so 'basis' can enumerate the monomials
  R := K Rmon;
  -- make a hash table to store the blocks of the matrix
  blks := new MutableHashTable;
  blks#(exponents 1_Rmon) = C.dd;
time  scan(0 .. c-1, i -> 
       blks#(exponents Rmon_i) = nullhomotopy (- f_i*id_C));
  -- a helper function to list the factorizations of a monomial
  factorizations := (gamma) -> (
    -- Input: gamma is the list of exponents for a monomial
    -- Return a list of pairs of lists of exponents showing the
    -- possible factorizations of gamma.
    if gamma === {} then { ({}, {}) }
    else (
      i := gamma#-1;
      splice apply(factorizations drop(gamma,-1), 
	(alpha,beta) -> apply (0..i, 
	     j -> (append(alpha,j), append(beta,i-j))))));
time   scan(4 .. length C + 1, 
    d -> if even d then (
      scan( flatten \ exponents \ leadMonomial \ first entries basis(d,R), 
	gamma -> (
	  s := - sum(factorizations gamma,
	    (alpha,beta) -> (
	      if blks#?alpha and blks#?beta
	      then blks#alpha * blks#beta
	      else 0));
	  -- compute and save the nonzero nullhomotopies
	  if s != 0 then blks#gamma = nullhomotopy s;
	  ))));
  -- make a free module whose basis elements have the right degrees
  spots := C -> sort select(keys C, i -> class i === ZZ);
  Cstar := S^(apply(spots C,
      i -> toSequence apply(degrees C_i, d -> {i,first d})));
  -- assemble the matrix from its blocks.
  -- We omit the sign (-1)^(n+1) which would ordinarily be used,
  -- which does not affect the homology.
  toS := map(S,A,apply(toList(c .. c+n-1), i -> S_i),
    DegreeMap => prepend_0);
  Delta := map(Cstar, Cstar, 
    transpose sum(keys blks, m -> S_m * toS sum blks#m),
    Degree => {-1,0});
  DeltaBar := Delta ** (toS ** N');
  assert isHomogeneous DeltaBar;
  assert(DeltaBar * DeltaBar == 0);
  if debugLevel > 10 then (
       stderr << describe ring DeltaBar <<endl;
       stderr << toExternalString DeltaBar << endl;
       );
  -- now compute the total Ext as a single homology module
  DDelta = DeltaBar;
time   tot := minimalPresentation time homology(DeltaBar,DeltaBar);
  tot)

end
-- On u123, this takes: (5 Feb 2008) (up through Ext(M,N)
-- 0.9.2:  2.22 sec
-- 1.0.9test: 15.01 sec
restart
debug Core
load "/Users/mike/src/M2/Macaulay2/bugs/mike/1-ext-slow.m2"
K = ZZ/103; 
A = K[x,y,z];
J = trim ideal(x^3,y^4,z^5)
B = A/J;
--f = random (B^3, B^{-2,-3})
f = map(B^{{0}, {0}, {0}}, B^{{-2}, {-3}}, 
	  {{-28*x^2-31*x*y-24*y^2-4*x*z-49*y*z-19*z^2, 
	       -44*x^2*y-4*x*y^2-49*y^3+30*x^2*z-51*x*y*z+51*y^2*z+23*x*z^2-19*y*z^2+42*z^3}, 
	  {47*x^2-6*x*y-49*y^2+9*x*z+47*y*z-25*z^2, 
	       16*x^2*y-9*x*y^2-31*y^3+34*x^2*z-2*x*y*z-16*y^2*z-23*x*z^2+14*y*z^2+50*z^3}, 
	  {-36*x^2-44*x*y-18*y^2+11*x*z-18*y*z+21*z^2, 
	       -36*x^2*y+28*x*y^2-21*y^3-x^2*z-8*x*y*z+6*y^2*z+37*x*z^2+27*y*z^2+43*z^3}})     
M = cokernel f;
N = B^1/(x^2 + z^2,y^3);
time Ext(M,N)

time Exty(M,N);
