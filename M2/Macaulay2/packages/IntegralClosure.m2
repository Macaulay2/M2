newPackage( 
	"IntegralClosure",
    	Version => "1.0", 
    	Date => "October 21, 2009",
    	Authors => {
	     {Name => "Amelia Taylor",
	     HomePage => "http://faculty1.coloradocollege.edu/~ataylor/",
   	     Email => "amelia.taylor@coloradocollege.edu"
	     },
	     {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "http://www.msri.org/~de/"},
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike"}
	     },
    	Headline => "Integral Closure",

    	DebuggingMode => false,
	AuxiliaryFiles => true
    	)
--

needsPackage "PrimaryDecomposition"
debug PrimaryDecomposition
   
export{
     "integralClosure", 
     "integralClosures", 
     "Verbosity",
     "Keep",
     "conductor", 
     "icFractions", 
     "icMap", 
     "isNormal", 

     "icFracP", 
     "ConductorElement",
     "icPIdeal",

--     "canonicalIdeal", 
--     "parametersInIdeal",
--     "randomMinors",
     "makeS2",

     "idealizer", 
     "ringFromFractions", 
     "Index",

     "RadicalCodim1",
     "AllCodimensions"
     } 

     "endomorphisms",
     "vasconcelos",
     "SimplifyFractions", -- simplify fractions
     "Endomorphisms", -- compute end(I)
     "Vasconcelos", -- compute end(I^-1).  If both this and Endomorphisms are set:
                 -- compare them.
     "StartWithS2", -- compute S2-ification first
     "RecomputeJacobian",
     "StartWithOneMinor",
     "S2First", 
     "S2Last", 
     "S2None", -- when to do S2-ification
     "RadicalBuiltin" -- true: use 'intersect decompose' to get radical, other wise use 'rad' in PrimaryDecomposition package

verbosity = 0

needsPackage "ReesAlgebra"

--- Should Singh/Swanson be an option to integralClosure or its own
--- program.  Right now it is well documented on its own.  I'm not
--- sure what is best long term. 

makeVariable = opts -> (
     s := opts.Variable;
     if instance(s,Symbol) then s else
     if instance(s,String) then getSymbol s else
     error "expected Variable option to provide a string or a symbol"
     )

integralClosure = method(Options=>{
	  Variable => "w",
	  Limit => infinity,
	  Strategy => {}, -- a mix of certain symbols
     	  Verbosity => 0,
	  Keep => null -- list of variables to not simplify away.  Default is all original vars
	  }
	)

protect StartWithOneMinor

idealInSingLocus = (S, opts) -> (
     -- Input: flattened poly ring S = S'/I, where S' is a poly ring.
     --        OptionTable from integralClosure
     -- Output:
     --        ideal in singular locus
     --          could be entire sing locus, or could be
     --          discriminant, etc.
     -- private subroutine of integralClosure

     -- Step1: choose an ideal J contained in the radical of the ideal of the singular locus.
     -- Choose an ideal J here.  Allow option to start with a J?

     if opts.Verbosity >= 1 then (
	  << " [jacobian time " << flush;
	  );
     if member(StartWithOneMinor, opts.Strategy) then 
          t1 := timing (J := ideal nonzeroMinor(codim ideal S, jacobian S))
     else
          t1 = timing (J = minors(codim ideal S, jacobian S));

     if verbosity >= 1 then (
        << t1#0 << " sec #minors " << numgens J << "]" << endl;
	);
     J
     )

integralClosure Ring := Ring => o -> (R) -> (
     -- 1 argument: affine ring R.  We might be able to handle rings over ZZ
     --   if we choose J in the non-normal ideal some other way.
     if R.?icMap then return target R.icMap;
     verbosity = o.Verbosity;
     strategies := set o.Strategy;
     (S,F) := flattenRing R;

     -- right here, we will grab the variables to be excluded
     -- this seems a bit awkward, perhaps there is a better way?
     T := ambient S;
     kk := ultimate(coefficientRing,T);
     allgens := generators(T, CoefficientRing => kk);
     keepvars := o.Keep;
     if keepvars === null then keepvars = allgens;

     P := ideal S;
     startingCodim := codim P;
     isCompleteIntersection := (startingCodim == numgens P);
     G := map(frac R, frac S, substitute(F^-1 vars S, frac R));

     isS2 := isCompleteIntersection; -- true means is, false means 'do not know'
     nsteps := 0;
     t1 := null;  -- used for timings

     codim1only := not member(AllCodimensions, strategies);
       -- this means: don't bother to compute the S2-ification
       -- and don't try to take only the codim 1 part of the radical
       -- of the jacobian locus.
       
     ------------------------------------------
     -- Step 1: Find ideal in singular locus --
     ------------------------------------------     
     -- other possible things here: make a list of ideals, and we 
     --   will compute End of each in turn.
     --   (b) use discriminant
     J := idealInSingLocus(S, o); -- returns ideal in S
     codimJ := codim J;
     isR1 := (codimJ > 1);

     if verbosity >= 1 then (
     	  << "integral closure nvars " << numgens S;
	  << " numgens " << numgens ideal S;
	  if isS2 then << " is S2";
	  if isR1 then << " is R1";
	  << " codim " << startingCodim;
	  << " codimJ " << startingCodim + codimJ << endl;
       << endl;
       );

     ------------------------------
     -- Step 2: make the ring S2 --
     ------------------------------
     --  unless we are using an option that
     --  doesn't require it.
     if not isS2 and codim1only then (
	  if verbosity >= 1 then 
	  << "   S2-ification " << flush;
	   t1 = (timing F'G' := makeS2 target F);
	   if verbosity >= 1 then
		<< t1#0 << " seconds" << endl;
	   if F'G' === null then (
		<< "warning: probabilistic computation of S2-ification failed " << endl;
		<< "         reverting to standard algorithm" << endl;
		strategies = strategies + set {AllCodimensions};
		codim1only = false
	   ) else (
	   (F', G') := F'G';
           F = F'*F;
	   G = G*G';
	   -- also extend J to be in this ring
	   J = trim(F' J);
	   isS2 = true;
	   ));

     -------------------------------------------
     -- Step 3: incrementally add to the ring --
     -------------------------------------------
     if not isR1 or not codim1only then (     
     -- loop (note: F : R --> Rn, G : frac Rn --> frac R)
     while (
	  F1 := F;

	  if verbosity >= 1 then << " [step " << nsteps << ": " << flush;

	  t1 = timing((F,G,J) = integralClosure1(F1,G,J,nsteps,makeVariable o,keepvars,strategies));

          if verbosity >= 1 then (
		 if verbosity >= 5 then (
		      << "  time " << t1#0<< " sec  fractions " << first entries G.matrix << "]" << endl << endl;
		      )
		 else (
		      << "  time " << t1#0<< " sec  #fractions " << numColumns G.matrix << "]" << endl;
		      );
		 );

	  nsteps = nsteps + 1;
	  nsteps < o.Limit and target F1 =!= target F
	  ) do (
	  ));
     R.icFractions = first entries G.matrix;
     R.icMap = F;
     target R.icMap
     )


doingMinimalization = true;

--the following finds an element in the intersection of the
--principal ideals generated by the ring elements in a list.
protect Radical
protect AddMinors
commonDenom = X -> findSmallGen intersect(apply (X, x->ideal x));
radicalJ = (J,codim1only,nsteps,strategies) -> (
     -- J is an ideal in R0.
     -- compute the radical of J, or perhaps a list of 
     -- components of J.  Possibly:
     --  remove components of codim > 1 in R0.
     --  add in new elements of the singular locus of J first, or after
     --  computing the radical.
     -- Choices for the radical:
     --  (a) intersection of decompose
     --  (b) use rad, limiting to codim 1
     --  (c) what else?
     useRadical := false;
     useRadicalCodim1 := false;
     useDecompose := true;
     if member(RadicalCodim1, strategies) then useRadicalCodim1 = true;
     if member(Radical, strategies) then useRadical = true;
     
     R0 := ring J;
     J = trim J;
     if false and nsteps > 0 and member(AddMinors, strategies) then ( 
	  -- MES: compute dimension of the orig ring above, so that we know the size of minors here,
	                  -- with no extra computation
	  newminors := ideal (0_R0);
	  while newminors == 0 do
	    newminors = ideal randomMinors(10,numgens R0 - dim R0,jacobian R0);
	  J = J + newminors
	  );
     if codim1only and codim J > 1 then return {};

     if verbosity >= 2 then (
       	  << endl << "      radical " <<
	  (if useRadical then "(use usual radical) "
     	  else if useRadicalCodim1 then "(use codim1radical) "
	  else "(use decompose) ")
          << flush;
	  );

     Jup := trim first flattenRing J;
     Jup = ideal apply(Jup_*, f -> product apply(apply(toList factor f, toList), first));
     Jup = trim ideal gens gb Jup;
     
     if verbosity >= 5 then (
       << "R0 = " << toExternalString ring Jup << endl;
       << "J0 = " << toString Jup << endl;
       );

     t1 := timing(radJup := 
       if useRadical then {radical Jup}
       else if useRadicalCodim1 then {rad(Jup,0)}
       else if useDecompose then decompose Jup);

     radJ := apply(radJup, L -> trim promote(L, R0));
     
     if verbosity >= 4 then << "done computing radical" << endl << flush;     
     if verbosity >= 2 then << t1#0 << " seconds" << endl;

     if codim1only then radJ = select(radJ, J -> codim J == 1);
     
     if verbosity >= 5 then (
	  << "dimension of components: " << apply(radJ, codim) << endl;
	  << "components of radJ: " << endl;
	  << netList radJ 
	  << endl;
	  );

     radJ
     )

protect SimplifyFractions				    -- unexported ??

integralClosure1 = (F,G,J,nsteps,varname,keepvars,strategies) -> (
     -- F : R -> R0, R0 is assumed to be a domain
     -- G : frac R0 --> frac R
     -- J : ideal in the non-normal ideal of R0
     -- new variables will be named varname_(nsteps,0),...
     -- Return value:
     --  (F1,G1,J1)
     --    where
     --      F1 : R --> R1
     --      G1 : frac R1 --> frac R
     --      J1 : is the extension of J to an ideal of R1.
     -- R1 is integrally closed iff target F === target F1
     codim1only := not member(AllCodimensions, strategies);

     R0 := target F;
     J = trim J;
     radJ := radicalJ(J, codim1only, nsteps, strategies);
     if #radJ == 0 then return (F,G,ideal(1_R0));
     radJ = trim intersect radJ;

     f := findSmallGen radJ; -- we assume that f is a non-zero divisor!!
     -- Compute Hom_S(radJ,radJ), using f as the common denominator.

     if verbosity >= 3 then <<"      small gen of radJ: " << f << endl << endl;
     if verbosity >= 6 then << "rad J: " << netList flatten entries gens radJ << endl;
     if verbosity >= 2 then <<"      idlizer1:  " << flush;

     t1 := timing((He,fe) := endomorphisms(radJ,f));

     if verbosity >= 2 then << t1#0 << " seconds" << endl;
     if verbosity >= 6 then << "endomorphisms returns: " << netList flatten entries He << endl;
     
     -- here is where we improve or change our fractions
     if He == 0 then (
	  -- there are no new fractions to add, and this process will add no new fractions
	  return (F,G,ideal(1_R0));
	  );

     if verbosity >= 6 then (
	  << "        about to add fractions" << endl;
          << "        " << apply(flatten entries He, g -> G(g/f)) << endl;
	  );

     if verbosity >= 2 then <<"      idlizer2:  " << flush;
     
     --Here is where the fractions are moved back to the orig ring and reduced there;
     --need to put in a strategy option to decide whether to do this.
     
{*     
     G1 := map(target G, R0, matrix G);
     feR := G1 fe;
     HeR := (G1 He);
     HeRnum := (flatten entries HeR)/numerator;
     HeRden := (flatten entries HeR)/denominator;
     feRnum := numerator feR;
     feRden := denominator feR;
	  
     L := commonDenom (HeRden/(H->H*feRnum));
     multipliers := apply(HeRden, H -> L//(H*feRnum));
     HeRnum1 := apply(#HeRnum, i->(HeRnum_i*multipliers_i*feRden)%L);

     (He1, fe1) = (F(matrix{HeRnum1}), F L);

<<endl<< "L= " << L << endl;
<<"multipliers= " << multipliers << endl;
<<"feRnum =     " << feRnum << endl;
<<"HeRden =     " << HeRden << endl;


<<"fe1= " << fe1  << endl;

{*
{*He1 := matrix{
     apply(flatten entries He, 
	  h->(
	       fr:=h/fe;
	       (numerator(fr) % fe) *(fe//denominator fr)))
     };
*}

       if member(SimplifyFractions, strategies)
       then (He,fe) = (
     	    Hef := apply(flatten entries He, h->h/f);
     	    Henum := Hef/numerator;
     	    Heden := Hef/denominator;
     	    Henumred := apply(#Hef, i-> Henum_i % Heden_i);
     	    fe1 := commonDenom Heden;
     	    multipliers := apply(Heden, H -> fe1//H);
     	    He1 := matrix{
	       	 apply(#Hef, i -> (Henumred#i * multipliers#i))
		 };
	    (He1,fe1));
	     
--<<endl;
--<<"He= " << flatten entries He  << endl;
--<<"He1= " << flatten entries He1  << endl;

     if verbosity >= 6 then (
	  << "        reduced fractions: " << endl;
          << "        " << apply(flatten entries He, g -> G(g/fe)) << endl;
	  );

--error();     
     t1 = timing((F0,G0) := ringFromFractions(He,fe,Variable=>varname,Index=>nsteps));
     
     if verbosity >= 2 then << t1#0 << " seconds" << endl;
{*
     time (F0,G0) = 
         idealizer(radJ, f, 
	      Variable => varname, 
	      Index => nsteps,
	      Strategy => strategies);
*}	 
     -- These would be correct, except that we want to clean up the
     -- presentation
     R1temp := target F0;
     if R1temp === R0 then return(F,G,radJ);

     if doingMinimalization then (
       if verbosity >= 2 then << "      minpres:   " << flush;
       R1tempAmbient := ambient R1temp;
       keepindices := apply(keepvars, x -> index substitute(x, R1tempAmbient));
       t1 = timing(R1 := minimalPresentation(R1temp, Exclude => keepindices));
       if verbosity >= 2 then << t1#0 << " seconds" << endl;
       i := R1temp.minimalPresentationMap; -- R1temp --> R1
       iinv := R1temp.minimalPresentationMapInv.matrix; -- R1 --> R1temp
       iinvfrac := map(frac R1temp , frac R1, substitute(iinv,frac R1temp));
     
       -- We also want to trim the ring     
--error();     
       F0 = i*F0; -- R0 --> R1
       (F0*F,G*G0*iinvfrac,F0 radJ)
       )
     else 
       (F0,G0,F0 radJ)
     )

---------------------------------------------------
-- Support routines, perhaps should be elsewhere --
---------------------------------------------------

randomMinors = method()
randomMinors(ZZ,ZZ,Matrix) := (n,d,M) -> (
     --produces a list of n distinct randomly chosend d x d minors of M
     r := numrows M;
     c := numcols M;
     if d > min(r,c) then return null;
     if n >= binomial(r,d) * binomial(c,d)
	then return (minors(d,M))_*;
     L := {}; -- L will be a list of minors, specified by the pair of lists "rows" and "cols"
     dets := {}; -- the list of determinants taken so far
     rowlist := toList(0..r-1);
     collist := toList(0..c-1);
     ds := toList(0..d-1);

     for i from 1 to n do (
      -- choose a random set of rows and of columns, add it to L 
      -- only if it doesn't appear already. When a pair is added to L, 
      -- the corresponding minor is added to "dets"
       while ( 
         rows := sort (random rowlist)_ds ;
         cols := sort (random collist)_ds ;
         for p in L do (if (rows,cols) == p then break true);
         false)
        do();
       L = L|{(rows,cols)};
       dets = dets | {det (M^rows_cols)}
       );
     dets
     )

nonzeroMinor = method(Options => {Limit => 100})
nonzeroMinor(ZZ,Matrix) :=  opts -> (d,M) -> (
     --produces one d x d nonzero minor, making up to 100 random tries.
     r := numrows M;
     c := numcols M;
     if d > min(r,c) then return null;
     candidate := 0_(ring M);
     rowlist := toList(0..r-1);
     collist := toList(0..c-1);
     ds := toList(0..d-1);
     for i from 1 to opts.Limit do(
      -- choose a random set of rows and of columns, test the determinant.
         rows := sort (random rowlist)_ds ;
         cols := sort (random collist)_ds ;
         candidate = det (M^rows_cols);
	 if candidate != 0 then return(candidate);
       );
     error("no nonzero minors found");
     )

-------------------------------------------
-- Rings of fractions, finding fractions --
-------------------------------------------
findSmallGen = (J) -> (
     a := toList((numgens ring J):1);
     L := sort apply(J_*, f -> ((weightRange(a,f))_1, size f, f));
     --<< "first choices are " << netList take(L,3) << endl;
     L#0#2
     )

idealizer = method(Options=>{Variable => "w", 
	                Index => 0, Strategy => {}})

idealizer (Ideal, RingElement) := o -> (J, g) ->  (
     -- J is an ideal in a ring R
     -- f is a nonzero divisor in J
     -- compute R1 = Hom(J,J) = (f*J:J)/f
     -- returns a sequence (F,G), where 
     --   F : R --> R1 is the natural inclusion
     --   G : frac R1 --> frac R, 
     -- optional arguments:
     --   o.Variable: base name for new variables added
     --   o.Index: the first subscript to use for such variables
     R := ring J;
     --(Hv,fv) := vasconcelos(J,g);
     (He,fe) := endomorphisms(J,g);
     --<< "vasconcelos  fractions:" << netList prepend(fv,flatten entries Hv) << endl;
     if verbosity >= 5 then << "endomorphism fractions:" << netList prepend(fe,flatten entries He) << endl;
{*
     if member("vasconcelos", set o.Strategy) then (
	  print "Using vasconcelos";
     	  (H,f) := vasconcelos (J,g))
     else (H,f) = endomorphisms (J,g);
*}     
     (H,f) := (He,fe);
--     idJ := mingens(f*J : J);
     if H == 0 then 
	  (id_R, map(frac R, frac R, vars frac R)) -- in this case R is isomorphic to Hom(J,J)
     else ringFromFractions(H,f,Variable=>makeVariable o,Index=>o.Index)
	  )


endomorphisms = method()
endomorphisms(Ideal,RingElement) := (I,f) -> (
     --computes generators in frac ring I of
     --Hom(I,I)
     --assumes that f is a nonzerodivisor.
     --NOTE: f must be IN THE CONDUCTOR; 
     --else we get only the intersection of Hom(I,I) and f^(-1)*R.
     --returns the answer as a sequence (H,f) where
     --H is a matrix of numerators
     --f = is the denominator.
     if not fInIdeal(f,I) then error "Proposed denominator was not in the ideal.";
     timing(H1 := (f*I):I);
     H := compress ((gens H1) % f);
     (H,f)
     )

vasconcelos = method()
vasconcelos(Ideal,RingElement) := (I,f) -> (
     --computes generators in frac ring I of
     --(I^(-1)*I)^(-1) = Hom(I*I^-1, I*I^-1),
     --which is in general a larger ring than Hom(I,I)
     --(though in a 1-dim local ring, with a radical ideal I = mm,
     --they are the same.)
     --assumes that f is a nonzerodivisor (not necessarily in the conductor).
     --returns the answer as a sequence (H,f) where
     --H is a matrix of numerators
     --f  is the denominator. MUST BE AN ELEMENT OF I.
     if f%I != 0 then error "Proposed denominator was not in the ideal.";
     m := presentation module I;
     timing(n := syz transpose m);
     J := trim ideal flatten entries n;
     timing(H1 := ideal(f):J);
     H := compress ((gens H1) % f);
     (H,f)
     )

debug Core -- for R.generatorSymbols

ringFromFractions = method(Options=>{
	  Variable => "w", 
	  Index => 0,
	  Verbosity => 0})
ringFromFractions (Matrix, RingElement) := o -> (H, f) ->  (
     -- f is a nonzero divisor in R
     -- H is a (row) matrix of numerators, elements of R
     -- Forms the ring R1 = R[H_0/f, H_1/f, ..].
     -- returns a sequence (F,G), where 
     --   F : R --> R1 is the natural inclusion
     --   G : frac R1 --> frac R, 
     -- optional arguments:
     --   o.Variable: base name for new variables added, defaults to w
     --   o.Index: the first subscript to use for such variables, defaults to 0
     --   so in the default case, the new variables produced are w_{0,0}, w_{0,1}...
          R := ring H;
       	  fractions := apply(first entries H,i->i/f);
          Hf := H | matrix{{f}};
     	  -- Make the new polynomial ring.
     	  n := numgens source H;
     	  newdegs := degrees source H - toList(n:degree f);
     	  degs := join(newdegs, (monoid R).Options.Degrees);
     	  MO := prepend(GRevLex => n, (monoid R).Options.MonomialOrder);
          kk := coefficientRing R;
	  var := makeVariable o;
     	  A := kk(monoid [var_(o.Index,0)..var_(o.Index,n-1), R.generatorSymbols,
		    MonomialOrder=>MO, Degrees => degs]);
     	  I := ideal presentation R;
     	  IA := ideal ((map(A,ring I,(vars A)_{n..numgens R + n-1})) (generators I));
     	  B := A/IA; -- this is sometimes a slow op

     	  -- Make the linear and quadratic relations
     	  varsB := (vars B)_{0..n-1};
     	  RtoB := map(B, R, (vars B)_{n..numgens R + n - 1});
     	  XX := varsB | matrix{{1_B}};
     	  -- Linear relations in the new variables
     	  lins := XX * RtoB syz Hf; 
     	  -- linear equations(in new variables) in the ideal
     	  -- Quadratic relations in the new variables
     	  tails := (symmetricPower(2,H) // f) // Hf;
     	  tails = RtoB tails;
     	  quads := matrix(B, entries (symmetricPower(2,varsB) - XX * tails));
	  both := ideal lins + ideal quads;
	  gb both; -- sometimes slow
	  Bflat := flattenRing (B/both); --sometimes very slow
	  R1 := trim Bflat_0; -- sometimes slow

     	  -- Now construct the trivial maps
     	  F := map(R1, R, (vars R1)_{n..numgens R + n - 1});
	  G := map(frac R, frac R1, matrix{fractions} | vars frac R);
--error();
	  (F, G)
     )


fInIdeal = (f,I) -> (
     -- << "warning: fix fInIdeal" << endl;
     if isHomogeneous I -- really want to say: is the ring local?
       then f%I == 0
       else substitute(I:f, ultimate(coefficientRing, ring I)) != 0
     )


///
restart
load "IntegralClosure.m2"
kk=ZZ/101
S=kk[a,b,c,d]
I=monomialCurveIdeal(S, {3,5,6})
R=S/I
K = ideal(b,c)
f=b*d
vasconcelos(K, f)
endomorphisms(K, f)
codim K
R1=ringFromFractions vasconcelos(K,f)
R2=ringFromFractions endomorphisms(K,f)
betti res I -- NOT depth 2.
time integralClosure(R, Strategy => {"vasconcelos"})
time integralClosure(R, Strategy => {})
makeS2 R
///

///

restart
load "integralClosure.m2"
kk=ZZ/101
S=kk[a,b,c,d]
I=monomialCurveIdeal(S, {3,5,6})
M=jacobian I
D = randomMinors(2,2,M)
R=S/I
J = trim substitute(ideal D ,R)
vasconcelos (J, J_0)
codim((J*((ideal J_0):J)):ideal(J_0))
endomorphisms (J,J_0)
vasconcelos (radical J, J_0)
endomorphisms (radical J,J_0)
codim J
syz gens J

///

-- PURPOSE: check if an affine domain is normal.  
-- INPUT: any quotient ring.  
-- OUTPUT:  true if the ring is normal and false otherwise. 
-- COMMENT: This computes the jacobian of the ring which can be expensive.  
-- However, it first checks the less expensive S2 condition and then 
-- checks R1.  
isNormal = method()     
isNormal(Ring) := Boolean => (R) -> (
     -- 1 argument:  A ring - usually a quotient ring. 
     -- Return: A boolean value, true if the ring is normal and false
     -- otherwise. 
     -- Method:  Check if the Jacobian ideal of R has
     -- codim >= 2, if true then check the codimension 
     -- of Ext^i(S/I,S) where R = S/I and i>codim I. If 
     -- the codimensions are >= i+2 then return true.
     I := ideal (R);
     M := cokernel generators I;
     n := codim I;         
     test := apply((dim ring I)-n-1,i->i);
     if all(test, j -> (codim Ext^(j+n+1)(M,ring M)) >= j+n+3) 
     then ( 
	  Jac := minors(n,jacobian R);
	  d := dim Jac;
	  if d < 0 then d = -infinity;
	  dim R - d >=2)
     else false
     )

--------------------------------------------------------------------
conductor = method()
conductor RingMap := Ideal => (F) -> (
     --Input:  A ring map where the target is finitely generated as a 
     --module over the source.
     --Output: The conductor of the target into the source.
     --Assumption: if R = source F, then R.icFractions is set
     --  or R is homogeneous
     R := source F;
     if false and R.?icFractions
       then (
	    -- here we have a set of fractions which generate the integral closure
	    L := R.icFractions;
	    L = apply(L, h -> {numerator h, denominator h});
	    L = select(L, h -> h#1 != 1);
	    ans := ideal(1_R);
	    scan(L, h -> (
		      L1 := (ideal h#1) : h#0;
		      ans = trim intersect(ans, L1);
		      ));
	    ans
	    )
     else if isHomogeneous (source F)
     	  then(M := presentation pushForward(F, (target F)^1);
     	       P := target M;
     	       intersect apply((numgens P)-1, i->(
	       m:=matrix{P_(i+1)};
	       I:=ideal modulo(m,matrix{P_0}|M))))
	  else error "conductor: expected a homogeneous ideal in a graded ring"
     )
conductor Ring := (R) -> conductor icMap R

icMap = method()
icMap(Ring) := RingMap => R -> (
     -- 1 argument: a ring.  May be a quotient ring, or a tower ring.
     -- Returns: The map from R to the integral closure of R.  
     -- Note:  This is a map where the target is finitely generated as
     -- a module over the source, so it can be used as the input to
     -- conductor and other methods that require this. 
     if R.?icMap then R.icMap
     else if isNormal R then id_R
     else (
	  integralClosure R;
	  R.icMap
	  )
     )

     

///
restart
loadPackage"IntegralClosure"
S = QQ [(symbol Y)_1, (symbol Y)_2, (symbol Y)_3, (symbol Y)_4, symbol x, symbol y, Degrees => {{7, 1}, {5, 1}, {6, 1}, {6, 1}, {1, 0}, {1, 0}}, MonomialOrder => ProductOrder {4, 2}]
J =
ideal(Y_3*y-Y_2*x^2,Y_3*x-Y_4*y,Y_1*x^3-Y_2*y^5,Y_3^2-Y_2*Y_4*x,Y_1*Y_4-Y_2^2*y^3)
T = S/J       
J = integralClosure T
KF = frac(ring ideal J)
M1 = first entries substitute(vars T, KF)
M2 = apply(T.icFractions, i -> matrix{{i}})

assert(icFractions T == substitute(matrix {{(Y_2*y^2)/x, (Y_1*x)/y,
Y_1, Y_2, Y_3, Y_4, x, y}}, frac T))
///

--------------------------------------------------------------------
icFractions = method()
icFractions(Ring) := Matrix => (R) -> (
     if R.?icFractions then R.icFractions
     else if isNormal R then vars R
     else (
	  integralClosure R;
     	  R.icFractions	  
     )
)

--------------------------------------------------------------------

icFracP = method(Options=>{ConductorElement => null, Limit => infinity, Verbosity => 0})
icFracP Ring := List => o -> (R) -> (
     -- 1 argument: a ring whose base field has characteristic p.
     -- Returns: Fractions
     -- MES:
     --  ideal presentation R ==== ideal R
     --  dosn't seem to handle towers
     --  this ring in the next line can't really be ZZ?
     if ring ideal presentation R === ZZ then (
	  D := 1_R;
	  U := ideal(D);
	  if o.Verbosity > 0 then print ("Number of steps: " | toString 0 | ",  Conductor Element: " | toString 1_R);
	  )    
     else if coefficientRing(R) === ZZ or coefficientRing(R) === QQ then error("Expected coefficient ring to be a finite field")
     else(
	  if o.ConductorElement === null then (
     	       P := ideal presentation R;
     	       c := codim P;
     	       S := ring P;
	       J := promote(jacobian P,R);
	       n := 1;
	       det1 := ideal(0_R);
	       while det1 == ideal(0_R) do (
		    det1 = minors(c, J, Limit => n);
		    n = n+1
		    );
	       D = det1_0;
	       D = (mingens(ideal(D)))_(0,0);
	       )
     	  else D = o.ConductorElement;
     	  p := char(R);
     	  K := ideal(1_R);
     	  U = ideal(0_R);
     	  F := apply(generators R, i-> i^p);
     	  n = 1;
     	  while (U != K) do (
	       U = K;
	       L := U*ideal(D^(p-1));
	       f := map(R/L,R,F);
	       K = intersect(kernel f, U);
	       if (o.Limit < infinity) then (
	       	    if (n >= o.Limit) then U = K;
	       	    );
               n = n+1;
     	       );
     	  if o.Verbosity > 0 then print ("Number of steps: " | toString n | ",  Conductor Element: " | toString D);
     	  );
     U = mingens U;
     if numColumns U == 0 then {1_R}
     else apply(numColumns U, i-> U_(0,i)/D)
     )

icPIdeal = method()
icPIdeal (RingElement, RingElement, ZZ) := Ideal => (a, D, N) -> (
     -- 3 arguments: An element in a ring of characteristic P that
     -- generates the principal ideal we are intersted in, a
     -- non-zerodivisor of $ in the conductor, and the number of steps
     -- in icFracP to compute the integral closure of R using the
     -- conductor element given.  
     -- Returns: the integral closure of the ideal generated by the
     -- first argument.  
     n := 1;
     R := ring a;
     p := char(R);
     J := ideal(1_R);
     while (n <= N+1) do (
	F := apply(generators R, i-> i^(p^n));
	U := ideal(a^(p^n)) : D;
        f := map(R/U, R, F);
        J = intersect(J, kernel f);
	n = n+1;
     );
     J
     )

----------------------------------------
-- Integral closure of ideal -----------
----------------------------------------
extendIdeal = (I,f) -> (
     --input: f: (module I) --> M, a map from an ideal to a module that is isomorphic
     --to a larger ideal
     --output: generators of an ideal J isomorphic to M, so that f becomes
     --the inclusion map.
     M:=target f;
     iota:= matrix f;
     psi:=syz transpose presentation M;
     beta:=(transpose gens I)//((transpose iota)*psi);
     trim ideal(psi*beta))

TEST ///
  assert isNormal (QQ[x]/(x^2+1))
  assert not isNormal (QQ[x,y,z]/( ideal(x*y, z) * ideal (z-1) ))
  assert not isNormal (QQ[x,y,z]/( ideal(x*y)    * ideal (x-1,y-1) ))
  assert not isNormal (QQ[x,y,z]/( ideal(x*y, z) * ideal (x-1,y-1) ))
  assert not isNormal (QQ[x,y,z]/( ideal(x*y)    * ideal (z-1) ))
  assert not isNormal (QQ[x,y,z]/( ideal(x*y)    * ideal (z-1) ))
  assert isNormal (QQ[x,y,z,t]/( ideal (x^2+y^2+z^2,t) ))
///

  -- here is an example of why the ring has to be equidimensional:
  -- assert isNormal (QQ[x,y,z,t]/( ideal (x^2+y^2+z^2,t) * ideal(t-1) ))

TEST ///
debug IntegralClosure
kk=ZZ/101
S=kk[a,b,c]
I =ideal"a3,ac2"
M = module ideal"a2,ac"
f=inducedMap(M,module I)
extendIdeal(I,f)     
///

integralClosure(Ideal, ZZ) := opts -> (I,D) ->(
     S:= ring I;
     z:= local z;
     w:= local w;
     Reesi := (flattenRing reesAlgebra(I,Variable =>z))_0;
     Rbar := integralClosure(Reesi, opts, Variable => w);
     zIdeal := ideal(map(Rbar,Reesi))((vars Reesi)_{0..numgens I -1});
     zIdealD := module zIdeal^D;
     RbarPlus := ideal(vars Rbar)_{0..numgens Rbar - numgens S-1};
     RbarPlusD := module RbarPlus^D;
     gD := matrix inducedMap(RbarPlusD, zIdealD);
     --     MM=(RbarPlus^D/(RbarPlus^(D+1)));
     mapback := map(S,Rbar, matrix{{numgens Rbar-numgens S:0_S}}|(vars S));
     M := coker mapback presentation RbarPlusD;
     ID := I^D;
     f := map(M, module ID, mapback gD);
     extendIdeal(ID,f)
     )
integralClosure(Ideal) := opts -> I -> integralClosure(I,1,opts)

integralClosures = method (Options => options integralClosure)
integralClosures(Ideal) := opts -> I -> (
    -- input: ideal I in an affine ring A
    -- output: 
     S := ring I;
     z := local z;
     w := local w;
     t := local t;
     A := tensor(coefficientRing(S)[t],S,Join=>false);
     IA := sub(I,A);
     ReesI := (flattenRing reesAlgebra(IA,Variable =>z))_0;
     fracs := icFractions ReesI;
     phi := map(frac(A),frac(ReesI),gens(A_0*IA)|vars A);
     newfracs := delete((frac A)_0, fracs/phi);
     -- The following two lines remove powers of t, and returns a hashtable
     L := partition(f -> degree(A_0, numerator f), newfracs);
     toFracS := map(frac S, frac A, {0} | gens frac S);
     hashTable apply(keys L, d -> d => apply(L#d, f -> toFracS(f // (A_0)^d)))
    )
----------------------------------------
-- Canonical ideal, makeS2 --------
----------------------------------------
parametersInIdeal = method()

parametersInIdeal Ideal := I -> (
     --first find a maximal system of parameters in I, that is, a set of
     --c = codim I elements generating an ideal of codimension c.
     --assumes ring I is affine. 
     --routine is probabilistic, often fails over ZZ/2, returns null when it fails.
     R := ring I;
     c := codim I;
     G := sort(gens I, DegreeOrder=>Ascending);
     s := 0; --  elements of G_{0..s-1} are already a sop (codim s)
     while s<c do(
     	  t := s-1; -- elements of G_{0..t} generate an ideal of codim <= s
     	  --make t maximal with this property, and then add one
     	  while codim ideal(G_{0..t})<=s and t<rank source G -1 do t=t+1;
     	  G1 := G_{s..t};
	  coeffs := random(source G1, R^{-last flatten degrees G1});
	  lastcoef := lift(last last entries coeffs,ultimate(coefficientRing, R));
	  --coeffs = (1/lastcoef)*coeffs;
     	  newg := G1*coeffs;
     	  if s<c-1 then G = G_{0..s-1}|newg|G_{t..rank source G-1}
	       else G = G_{0..s-1}|newg;
	  if codim ideal G <s+1 then (
	       return null;
--	       error ("random coeffs not general enough at step ",s);
	       );
	  s = s+1);
      ideal G)
///
restart
--uninstallPackage "IntegralClosure"
--loadPackage "IntegralClosure"
kk=ZZ/2
S=kk[a,b,c,d]
PP = monomialCurveIdeal(S,{1,3,4})
betti res PP
for count from 1 to 10 list parametersInIdeal PP
for count from 1 to 10 list canonicalIdeal (S/PP)
betti res oo
///     

canonicalIdeal1 = method()
canonicalIdeal1 Ring := R -> (
     --tries to find a canonical ideal in R. Note that if R is
     --not generically Gorenstein, then it has no canonical ideal!
     --This routine is
     --guaranteed to work if R is a domain; if R is merely reduced,
     --or just generically Gorenstein, it may fail. If it fails, 
     --it returns null
     (S,f) := flattenRing R;
     P := ideal S;
     SS := ring P;
     n :=numgens SS;
     c := codim P;
     WSS := prune Ext^c(SS^1/P, SS);
     WS := prune coker (map(S,SS)) (presentation WSS);
     H := Hom(WS, S^1);
     toIdeal := homomorphism H_{0};
     if ker toIdeal != 0 then return null;
     trim ideal f^-1 (image toIdeal))

///
restart
loadPackage "IntegralClosure"
debug IntegralClosure
A = ZZ/101[a..e]
I = ideal"ab,bc,cd,de,ea"
R = reesAlgebra I
describe I
describe R
canonicalIdeal1 R
canonicalIdeal R
R1 = first flattenRing R
canonicalIdeal1 R1
canonicalIdeal R1
///

canonicalIdeal = method()
canonicalIdeal Ring := R -> (
     --try to find a canonical ideal in R by the probabilistic method.
     --If you fail, try the method of canonicalIdeal1
     if degreeLength R =!= 1 or any(degrees R, x -> first x <= 0) then
     return canonicalIdeal1 R; -- problems with parametersInIdeal prevent its use yet
     (S,f) := flattenRing R;
     P := ideal S;
     J := parametersInIdeal P;
     if J === null then return canonicalIdeal1 R;
     Jp := J:P;
     trim (f^-1) promote(Jp,S)
     )
///
kk=ZZ/101
S=kk[a,b,c,d]
canonicalIdeal S
PP = monomialCurveIdeal(S,{1,3,4})
betti res PP
R = S/PP
w=canonicalIdeal R
debug IntegralClosure
w1 = canonicalIdeal1 R -- a different, somewhat less leasing answer...
F = homomorphism (Hom(w,w1))_{0}
ker F
prune coker F
isIsomorphism F
///     

makeS2 = method()
makeS2 Ring := R -> (
     --try to find the S2-ification of a domain (or more generally an
     --unmixed, generically Gorenstein ring) R.
     --    Input: R, an affine ring
     --    Output: (like "idealizer") a sequence whose 
     -- first element is a map of rings from R to its S2-ification,
     --and whose second element is a list of the fractions adjoined 
     --to obtain the S2-ification.
     --    Uses the method of Vasconcelos, "Computational Methods..." p. 161, taking the idealizer
     --of a canonical ideal.
     --Assumes that first element of canonicalIdeal R is a nonzerodivisor; else returns error.
     --CAVEAT:
          --If w_0 turns out to be a zerodivisor
	  --then we should replace it with a general element of w. But if things
	  --are multiply graded this might involve finding a degree with maximal heft 
	  --or some such. How should this be done?? There should be a "general element"
	  --routine...
     w := canonicalIdeal R;
     if w === null then return null;
     if ideal(0_R):w_0 == 0 then idealizer(w,w_0)
     else (
	  return null;
	  error"first generator of the canonical ideal was a zerodivisor"
	  )
     )

///
kk=ZZ/101
S=kk[a,b,c,d]
PP = monomialCurveIdeal(S,{1,3,4})
betti res PP
integralClosure(S/PP)
integralClosure(target (makeS2(S/PP))_0)
///     

TEST ///
   setRandomSeed 2342351
   S = QQ[a..d]

   I = monomialCurveIdeal(S,{1,3,4})
   R = S/I
   R' = integralClosure R

///

--------------------------------------------------------------------

beginDocumentation()

doc ///
  Key
    IntegralClosure
  Headline
    routines for integral closure of affine domains and ideals
  Description
    Text
      This package contains several algorithms for computing the
      integral closure (i.e. normalization) of an affine domain,
      and also of an ideal.
      
      The basic use of this package is shown in the following example.
    Example
      R = QQ[x,y,z]/(x^3-x^2*z^5-z^2*y^5)
      S = integralClosure R
    Text
      Use @TO icFractions@ to see what fractions have been added.
    Example
      icFractions R
    Text
      Look at the ideal of S or the generators of S to see the structure of the
      integral closure.
    Example
      gens S
      trim ideal S
    Text
      If the characteristic is positive, yet small compared to the degree, but the
      fraction ring is still separable over a subring, then use
      @TO icFracP@, which is an implementation of an algorithm due to
      Leonard-Pellikaan, and modified by Singh-Swanson (see arXiv:0901.0871).
      However, the interface to this routine is likely to change in future
      releases to more closely match the functions above.
      
      The method used by integralClosure is a modification of the basic 
      algorithm explained in Theo De Jong's paper {\em An Algorithm for 
	  Computing the Integral Closure}, J. Symbolic Computation, 
	  (1998) 26, 273-277.
///

doc ///
  Key
    integralClosure
  Headline
    integral closure of an ideal or a domain
///

doc ///
  Key
    isNormal
    (isNormal, Ring)
  Headline
    determine if a reduced ring is normal
  Usage
    isNormal R
  Inputs
    R:Ring
      a reduced equidimensional ring
  Outputs
    :Boolean
      whether {\tt R} is normal, that is, whether it satisfies
      Serre's conditions S2 and R1
  Description
   Text
     This function computes the jacobian of the ring which can be costly for 
     larger rings.  Therefore it checks the less costly S2 condition first and if 
     true, then tests the R1 condition using the jacobian of {\tt R}.
   Example
     R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);
     isNormal R
     isNormal(integralClosure R)
  Caveat
    The ring {\tt R} must be an equidimensional ring.  If {\tt R} is a domain,
    then sometimes computing the integral closure of {\tt R} can be faster than
    this test.
  SeeAlso
    integralClosure
    makeS2
///

doc ///
  Key
    (integralClosure, Ring)
  Headline
    compute the integral closure (normalization) of an affine domain
  Usage
    R' = integralClosure R
  Inputs
    R:Ring
      a quotient of a polynomial ring over a field
  Outputs
    R':Ring
      the integral closure of {\tt R}
  Consequences
   Item
    The inclusion map $R \rightarrow R'$
      can be obtained with @TO icMap@.  
   Item
    The fractions corresponding to the variables
      of the ring {\tt R'} can be found with @TO icFractions@
  Description
   Text
     The integral closure of a domain is the subring of the fraction field
     consisting of all fractions integral over the domain.  For example,
   Example
     R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4-z^3);
     R' = integralClosure R
     gens R'
     icFractions R
     icMap R
     I = trim ideal R'
   Text
     Sometimes using @TO trim@ provides a cleaner set of generators.
   Text
   
     If $R$ is not a domain, first decompose it, and collect all of the 
     integral closures.
   Example
     S = ZZ/101[a..d]/ideal(a*(b-c),c*(b-d),b*(c-d));
     C = decompose ideal S
     Rs = apply(C, I -> (ring I)/I);
     Rs/integralClosure
     oo/prune
   Text
     This function is roughly based on
     Theo De Jong's paper, {\em An Algorithm for 
     Computing the Integral Closure}, J. Symbolic Computation, 
     (1998) 26, 273-277. This algorithm is similar to the round two
     algorithm of Zassenhaus in algebraic number theory.
   Text
     There are several optional parameters which allows the user to control
     the way the integral closure is computed.  These options may change
     in the future.
  Caveat
    This function requires that the degree of the field extension 
    (over a pure transcendental subfield) be greater 
    than the characteristic of the base field.  If not, use @TO icFracP@.
    This function requires that the ring be finitely generated over a ring.  If not (e.g. 
    if it is f.g. over the integers), then the result is integral, but not necessarily 
    the entire integral closure. Finally, if the ring is not a domain, then
    the answers will often be incorrect, or an obscure error will occur.
  SeeAlso
    icMap
    icFractions
    conductor
    icFracP
///

doc ///
  Key
    [integralClosure, Keep]
  Headline
    list ring generators which should not be simplified away
  Usage
    integralClosure(R, Keep=>L)
  Inputs
    L:List
      a list of variables in the ring R, or {\tt null} (the default).
  Consequences
   Item
    The given list of variables (or all of the outer generators, if L is null)
    will be generators of the integral closure
  Description
   Text
     Consider the cuspidal cubic, and three different possibilities for {\tt Keep}.
   Example
     R = QQ[x,y]/ideal(x^3-y^2);
     R' = integralClosure(R, Variable => symbol t)
     trim ideal R'
   Example
     R = QQ[x,y]/ideal(x^3-y^2);
     R' = integralClosure(R, Variable => symbol t, Keep => {x})
     trim ideal R'
   Example
     R = QQ[x,y]/ideal(x^3-y^2);
     integralClosure(R, Variable => symbol t, Keep => {})
///

doc ///
  Key
    [integralClosure, Variable]
  Headline
    set the base letter for the indexed variables introduced while computing the integral closure
  Usage
    integralClosure(R, Variable=>x)
  Inputs
    x:Symbol
  Consequences
   Item
    The new variables will be subscripted using {\tt x}.
  Description
   Example
     R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4-z^3);
     R' = integralClosure(R, Variable => symbol t)
     trim ideal R'
   Text
     The algorithm works in stages, each time adding new fractions to the ring.
     A variable {\tt t_(3,0)} represents the first (zero-th) variables added at stage 3.
     In the future, the variables added will likely just be {\tt t_1, t_2, ...}.
  Caveat
    The base name should be a symbol
///

doc ///
  Key
    [integralClosure,Limit]
  Headline
    do a partial integral closure
  Usage
    integralClosure(R, Limit => n)
  Inputs
    n:ZZ
      how many steps to perform
  Description
   Text
     The integral closure algorithm proceeds by finding a suitable ideal $J$,
     and then computing $Hom_R(J,J)$, and repeating these steps.  This
     optional argument limits the number of such steps to perform.
     
     The result is an integral extension, but is not necessarily integrally closed.
   Example
     R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4-z^3);
     R' = integralClosure(R, Variable => symbol t, Limit => 2)
     trim ideal R'
     icFractions R
///

doc ///
  Key
    [integralClosure,Verbosity]
  Headline
    display a certain amount of detail about the computation
  Usage
    integralClosure(R, Verbosity => n)
  Inputs
    n:ZZ
      The higher the number, the more information is displayed.  A value
      of 0 means: keep quiet.
  Description
   Text
     When the computation takes a considerable time, this function can be used to 
     decide if it will ever finish, or to get a feel for what is happening
     during the computation.
   Example
     R = QQ[x,y,z]/ideal(x^8-z^6-y^2*z^4-z^3);
     time R' = integralClosure(R, Verbosity => 2)
     trim ideal R'
     icFractions R
  Caveat
    The exact information displayed may change.
///

doc ///
  Key
    [integralClosure,Strategy]
  Headline
    control the algorithm used
  Usage
    integralClosure(R, Strategy=>L)
  Inputs
    L:List
      of a subset of the following: {\tt RadicalCodim1, AllCodimensions}
  Description
   Text
     {\tt RadicalCodim1} chooses an alternate, often much faster, sometimes much slower,
     algorithm for computing the radical of ideals.  This will often produce a different
     presentation for the integral closure.
     
     {\tt AllCodimensions} tels the algorithm to bypass the computation of the
     S2-ification, but in each iteration of the algorithm, use the radical of
     the extended Jacobian ideal from the previous step, instead of using only the
     codimension 1 components of that.  This is useful when for some reason the
     S2-ification is hard to compute, or if the probabilistic algorithm for 
     computing it fails.  In general though, this option slows down the computation
     for many examples.
   Example
     R = QQ[x,y,z]/ideal(x^8-z^6-y^2*z^4-z^3);
     time R' = integralClosure(R, Strategy=>{RadicalCodim1})
     R = QQ[x,y,z]/ideal(x^8-z^6-y^2*z^4-z^3);
     time R' = integralClosure(R)
     R = QQ[x,y,z]/ideal(x^8-z^6-y^2*z^4-z^3);
     time R' = integralClosure(R, Strategy=>{AllCodimensions})
     R = QQ[x,y,z]/ideal(x^8-z^6-y^2*z^4-z^3);
     time R' = integralClosure(R, Strategy=>{RadicalCodim1, AllCodimensions})
///

doc ///
  Key
    (integralClosure,Ideal,ZZ)  
    (integralClosure,Ideal)
  Headline
    integral closure of an ideal in an affine domain
  Usage
    integralClosure J
    integralClosure(J, d)
  Inputs
    J:Ideal
    d:ZZ
      optional, default value 1
  Outputs
    :Ideal
      the integral closure of $I^d$
  Description
   Text
     The method used is described in Vasconcelos' book, 
     {\em Computational methods in commutative algebra and algebraic
	  geometry}, Springer, section 6.6.  Basically, one first
     computes the Rees Algebra of the ideal, and then one reads off
     the integral closure of any of the powers of the ideal, using
     linear algebra.
   Example
     S = ZZ/32003[a,b,c];
     F = a^2*b^2*c+a^3+b^3+c^3
     J = ideal jacobian ideal F
     time integralClosure J
     time integralClosure(J, Strategy=>{RadicalCodim1})
     integralClosure(J,2)
  Caveat
    It is usally much faster to use {\tt integralClosure(J,d)}
    rather than {\tt integralClosure(J^d)}
  SeeAlso
    (integralClosure,Ring)
    reesAlgebra
///

doc ///
  Key
    idealizer
    (idealizer, Ideal, RingElement)
  Headline
    compute Hom(I,I) as a quotient ring
  Usage
    (F,G) = idealizer(I,f)
  Inputs
    I:Ideal
      in a domain $R$
    f:RingElement
      an element of the ideal $I$
    Variable:Symbol
    Index:ZZ
  Outputs
    F:RingMap
      The inclusion map from $R$ into $S = Hom_R(I,I)$
    G:RingMap
      $frac S \rightarrow frac R$, giving the fractions
      corresponding to each generator of $S$.
  Description
   Text
     This is a key subroutine used in the computation of 
     integral closures.
   Example
     R = QQ[x,y]/(y^3-x^7)
     I = ideal(x^2,y^2)
     (F,G) = idealizer(I,x^2);
     target F
     first entries G.matrix
  SeeAlso
    ringFromFractions
    integralClosure
///

document {
     Key => [idealizer,Variable],
     Headline=> "Sets the name of the indexed variables introduced in computing 
     the endomorphism ring Hom(J,J)."
     }

document {
     Key => Index,
     Headline => "Optional input for idealizer",
     PARA{},
     "This option allows the user to select the starting index for the
     new variables added in computing Hom(J,J) as a ring.  The default
     value is 0 and is what most users will use.  The option is needed
     for the iteration implemented in integralClosure."
}


document {
     Key => [idealizer, Index],
     Headline=> "Sets the starting index on the new variables used to
     build the endomorphism ring Hom(J,J). If the program idealizer is
     used independently, the user will generally want to use the
     default value of 0.  However, when used as part of the
     integralClosure computation the number needs to start higher
     depending on the level of recursion involved. "
     }

doc ///
  Key
    icMap
    (icMap,Ring)
  Headline
    natural map from an affine domain into its integral closure
  Usage
    icMap R
  Inputs
    R:Ring
      an affine domain
  Outputs
    :RingMap
      from {\tt R} to its integral closure
  Description
   Text
     If the integral closure of {\tt R} has not yet been computed, 
     that computation is performed first.  No extra computation
     is involved.  If {\tt R} is integrally closed, then the identity
     map is returned.
   Example
     R = QQ[x,y]/(y^2-x^3)
     icMap R
   Text     
   
     This finite ring map can be used to compute the conductor,
     that is, the ideal of elements of {\tt R} which are 
     universal denominators for the integral closure (i.e. those d \in R
	  such that d R' \subset R).
   Example
     S = QQ[a,b,c]/ideal(a^6-c^6-b^2*c^4);
     F = icMap S;
     conductor F
  Caveat
    If you want to control the computation of the integral closure via optional
    arguments, then make sure you call @TO (integralClosure,Ring)@ first, since
    {\tt icMap} does not have optional arguments.
  SeeAlso
    (integralClosure,Ring)
    conductor
///

doc ///
  Key
    icFractions
    (icFractions, Ring)
  Headline
    fractions integral over an affine domain
  Usage
    icFractions R
  Inputs
    R:Ring
      an affine domain
  Outputs
    :List
      a list of fractions over {\tt R}, generating the
      integral closure of {\tt R}, as an {\tt R}-algebra.
  Description
   Text
     If the integral closure of {\tt R} has not yet been computed, 
     that computation is performed first.  No extra computation
     is then involved to find the fractions.
   Example
     R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);
     icFractions R
     R' = integralClosure R
     gens R'
     netList (ideal R')_*
   Text
     Notice that the $i$-th fraction corresponds to the $i$-th generator 
     of the integral closure.  For instance, the variable $w_(3,0) = {x^2 \over z}$.
  Caveat
     (a) Currently in Macaulay2, fractions over quotients of polynomial rings
     do not have a nice normal form.  In particular, sometimes
     the fractions are 'simplified' to give much nastier looking fractions.
     We hope that in the near future, this misfeature will be corrected.
     (b)
     If you want to control the computation of the integral closure via optional
     arguments, then make sure you call @TO (integralClosure,Ring)@ first, since
     {\tt icFractions} does not have optional arguments.
  SeeAlso
    integralClosure
    icMap
///

doc ///
  Key
    conductor
    (conductor,RingMap)
    (conductor,Ring)
  Headline
    the conductor of a finite ring map
  Usage
    conductor F
    conductor R
  Inputs
    F:RingMap
      $R \rightarrow S$, a finite map with $R$ an affine reduced ring
    R:Ring
      an affine domain.  In this case, $F : R \rightarrow S$ is the 
      inclusion map of $R$ into the integral closure $S$
  Outputs
    :Ideal
      of $R$ consisting of all $d \in R$ such that $dS \subset F(R)$
  Description
   Text
     Suppose that the ring map $F : R \rightarrow S$ is finite: i.e. $S$ is a finitely 
     generated $R$-module.  The conductor of $F$ is defined to be 
     $\{ g \in R \mid g S \subset F(R) \}$.
     One way to think
     about this is that the conductor is the set of universal denominators
     of {\tt S} over {\tt R}, or as the largest ideal of {\tt R}
     which is also an ideal in {\tt S}. An important case is the
     conductor of the map from a ring to its integral closure.
   Example
     R = QQ[x,y,z]/ideal(x^7-z^7-y^2*z^5);
     icFractions R
     F = icMap R
     conductor F
   Text
     If an affine domain (a ring finitely generated over a field) is given as input,
     then the conductor of $R$ in its integral closure is returned.
   Example
     conductor R
   Text
   
     If the map is not {\tt icFractions(R)}, then @TO pushForward@ is called to compute
     the conductor.
  Caveat
    Currently this function only works if {\tt F} comes from a
    integral closure computation, or is homogeneous
  SeeAlso
    integralClosure
    icFractions
    icMap
    pushForward
///

doc ///
  Key
    ringFromFractions
    (ringFromFractions,Matrix,RingElement)
  Headline
    find presentation for f.g. ring  
  Usage
    (F,G) = ringFromFractions(H,f)
  Inputs
    H:Matrix
      a one row matrix over a ring $R$
    f:RingElement
  Outputs
    F:RingMap
      $R \rightarrow S$, where $S$ is the extension ring
      of $R$ generated by the fractions $1/f H$
    G:RingMap
      $frac S \rightarrow frac R$, the fractions
  Description
   Text
     Serious restriction: It is assumed that this ring R[1/f H] is an endomorphism ring
     of an ideal in $R$.  This means that the Groebner basis, in a product order,
     will have lead terms all quadratic monomials in the new variables,
     together with other elements which are degree 0 or 1 in the new variables.
   Example
     R = QQ[x,y]/(y^2-x^3)
     H = (y * ideal(x,y)) : ideal(x,y)
     (F,G) = ringFromFractions(((gens H)_{1}), H_0);
     S = target F
     F
     G
///

doc ///
  Key
    makeS2
    (makeS2,Ring)
  Headline
    compute the S2ification of a reduced ring
  Usage
    (F,G) = makeS2 R
  Inputs
    R:Ring
      an equidimensional reduced (or just unmixed and genericaly Gorenstein) affine ring
  Outputs
    F:RingMap
      $R \rightarrow S$, where $S$ is the so-called S2-ification of $R$
    G:RingMap
      $frac S \rightarrow frac R$, giving the corresponding fractions
  Description
   Text
     A ring $S$ satisfies Serre's S2 condition if every codimension 1 ideal
     contains a nonzerodivisor and every principal ideal generated by a nonzerodivisor
     is equidimensional of codimension one.  If $R$ is an affine reduced ring, 
     then there is a unique smallest extension $R\subset S\subset {\rm frac}(R)$ satisfying S2, 
     and $S$ is finite as an $R$-module.

     There are several methods to compute $S$.  Currently, only two of these methods
     is implemented in this package.  Stay tuned, or help write the other
     methods!

     One simple example is the rational quartic curve.
   Example
     A = ZZ/101[a..d];
     I = monomialCurveIdeal(A,{1,3,4})
     R = A/I;
     (F,G) = makeS2 R
   Text
     This function is probabilistic, and can fail if $R$ is not a domain.  
     If it fails and the characteristic is not too small,
     then simply rerun it.  If the characteristic is small, 
     then another method may be needed.  This
     might mean that you need to either write it or ask us to do so!
  Caveat
     The return value of this function is likely to change in the future
  SeeAlso
    integralClosure
///

document {
     Key => {icFracP, (icFracP, Ring)},
     Headline => "compute the integral closure in prime characteristic",
     Usage => "icFracP R, icFracP(R, ConductorElement => D), icFracP(R, Limit => N), icFracP(R, Verbosity => ZZ)",
     Inputs => {
	"R" => {"that is reduced, equidimensional,
           finitely and separably generated over a field of characteristic p"},
	ConductorElement => {"optionally provide a non-zerodivisor conductor element ",
               TT "ConductorElement => D", ";
               the output is then the module generators of the integral closure.
               A good choice of ", TT "D", " may speed up the calculations?"},
	Limit => {"if value N is given, perform N loop steps only"},
	Verbosity => {"if value is greater than 0, report the conductor element and number of steps in the loop"},
	},
     Outputs => {{"The module generators of the integral closure of ", TT "R",
               " in its total ring of fractions.  The generators are
               given as elements in the total ring of fractions."}
          },
     "Input is an equidimensional reduced ring in characteristic p
     that is finitely and separably generated over the base field.
     The output is a finite set of fractions that generate
     the integral closure as an ", TT "R", "-module.
     An intermediate step in the code
     is the computation of a conductor element ", TT "D",
     " that is a non-zerodivisor;
     its existence is guaranteed by the separability assumption.
     The user may supply ", TT "D",
     " with the optional ", TT "ConductorElement => D", ".
     (Sometimes, but not always, supplying ", TT "D", " speeds up the computation.)
     In any case, with the non-zero divisor ", TT "D", ",
     the algorithm starts by setting the initial approximation of the integral closure
     to be the finitely generated ", TT "R", "-module
     ", TT "(1/D)R", ",
     and in the subsequent loop the code recursively constructs submodules.
     Eventually two submodules repeat;
     the repeated module is the integral closure of ", TT "R", ".
     The user may optionally provide ", TT "Limit => N", " to stop the loop
     after ", TT "N", " steps,
     and the optional ", TT "Verbosity => 1", " reports the conductor
     element and the number of steps it took for the loop to stabilize.
     The algorithm is based on the
     Leonard--Pellikaan--Singh--Swanson algorithm.",
     PARA{},
     "A simple example.",
     EXAMPLE {
          "R = ZZ/5[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
          "icFracP R"
     },
     "The user may provide an optional non-zerodivisor conductor element ",
     TT "D",
     ".  The output generators need not
     be expressed in  the form with denominator ", TT "D", ".",
     EXAMPLE {
          "R = ZZ/5[x,y,u,v]/ideal(x^2*u-y^2*v);",
          "icFracP(R)",
          "icFracP(R, ConductorElement => x)",
     },
     "In case ", TT "D", " is not in the conductor, the output is ",
     TT "V_e = (1/D) {r in R | r^(p^i) in (D^(p^i-1)) ", "for ",
     TT "i = 1, ..., e}",
     " such that ", TT "V_e = V_(e+1)", " and ", TT "e",
     " is the smallest such ", TT "e", ".",
     EXAMPLE {
	  "R=ZZ/2[u,v,w,x,y,z]/ideal(u^2*x^3+u*v*y^3+v^2*z^3);",
          "icFracP(R)",
          "icFracP(R, ConductorElement => x^2)"
     },
     "The user may also supply an optional limit on the number of steps
     in the algorithm.  In this case, the output is a finitely generated ",
     TT "R", "-module contained in ", TT "(1/D)R",
     " which contains the integral closure (intersected with ", TT "(1/D)R",
     ".",
     EXAMPLE {
          "R=ZZ/2[u,v,w,x,y,z]/ideal(u^2*x^3+u*v*y^3+v^2*z^3);",
          "icFracP(R, Limit => 1)",
          "icFracP(R, Limit => 2)",
          "icFracP(R)"
     },
     "With the option above one can for example determine how many
     intermediate modules the program should compute or did compute
     in the loop to get the integral closure.  A shortcut for finding
     the number of steps performed is to supply the ",
     TT "Verbosity => 1", " option.",
     EXAMPLE {
          "R=ZZ/3[u,v,w,x,y,z]/ideal(u^2*x^4+u*v*y^4+v^2*z^4);",
          "icFracP(R, Verbosity => 1)"
     },
     "With this extra bit of information, the user can now compute
     integral closures of principal ideals in ", TT "R", " via ",
     TO icPIdeal, ".",
     SeeAlso => {"icPIdeal", "integralClosure", "isNormal"},
     Caveat => "The interface to this algorithm will likely change in Macaulay2 1.4"
--     Caveat => "NOTE: mingens is not reliable, neither is kernel of the zero map!!!"
}

document {
     Key => ConductorElement,
     Headline => "Specifies a particular non-zerodivisor in the conductor."
}

document {
     Key => [icFracP,ConductorElement],
     Headline => "Specifies a particular non-zerodivisor in the conductor.",
     "A good choice can possibly speed up the calculations.  See ",
     TO icFracP, "."
}


document {
     Key => [icFracP,Limit],
     Headline => "Limits the number of computed intermediate modules."
--     Caveat => "NOTE: How do I make M2 put icFracP on the list of all functions that use Limit?"
}

doc ///
  Key
    Verbosity
  Headline
    optional argument describing how verbose the output should be
  Description
   Text
     Specifying the optional argument {\tt Verbosity => n}, where $n$ is an integer
     tells the routine how much output should be given.  A value of 0 means be silent.
     The larger the value $n$, the more output one might see.
  SeeAlso
    integralClosure
    icFracP
///

document {
     Key => [icFracP,Verbosity],
     Headline => "Prints out the conductor element and
           the number of intermediate modules it computed.",
     Usage => "icFracP(R, Verbosity => ZZ)",
     "The main use of the extra information is in computing the
     integral closure of principal ideals in ", TT "R",
     ", via ", TO icPIdeal,
     ".",
     EXAMPLE {
          "R=ZZ/3[u,v,x,y]/ideal(u*x^2-v*y^2);",
          "icFracP(R, Verbosity => 1)",
	  "S = ZZ/3[x,y,u,v];",
          "R = S/kernel map(S,S,{x-y,x+y^2,x*y,x^2});",
	  "icFracP(R, Verbosity => 1)"
     },
}

document {
     Key => {icPIdeal,(icPIdeal, RingElement, RingElement, ZZ)},
     Headline => "compute the integral closure
                  in prime characteristic of a principal ideal",
     Usage => "icPIdeal (a, D, N)",
     Inputs => {
	"a" => {"an element in ", TT "R"},
        "D" => {"a non-zerodivisor of ", TT "R",
                " that is in the conductor"},
        "N" => {"the number of steps in ", TO icFracP,
                " to compute the integral closure of ", TT "R",
                ", by using the conductor element ", TT "D"}},
     Outputs => {{"the integral closure of the ideal ", TT "(a)", "."}},
     "The main input is an element ", TT "a",
     " which generates a principal ideal whose integral closure we are
     seeking.  The other two input elements,
     a non-zerodivisor conductor element ", TT "D",
     " and the number of steps ", TT "N", 
     " are the pieces of information obtained from ",
     TT "icFracP(R, Verbosity => true)",
     ".  (See the Singh--Swanson paper, An algorithm for computing
     the integral closure, Remark 1.4.)",
     EXAMPLE {
          "R=ZZ/3[u,v,x,y]/ideal(u*x^2-v*y^2);",
          "icFracP(R, Verbosity => 1)",
          "icPIdeal(x, x^2, 3)"
     },
     SeeAlso => {"icFracP"},
     Caveat => "The interface to this algorithm will likely change in Macaulay2 1.4"     
}

doc ///
  Key
    Keep
  Headline
    an optional argument for various functions
  SeeAlso
    (integralClosure,Ring)
    (integralClosure,Ideal,ZZ)
///

doc ///
  Key
    RadicalCodim1
  Headline
    a symbol denoting a strategy element usable with integralClosure(...,Strategy=>...)
  SeeAlso
    (integralClosure,Ring)
///

TEST ///
S = ZZ/32003[a,b,c,d,x,y,z,u]
I = ideal(
   a*x-b*y,
   b*u^7+b*u^6-2*b*z*u^4+b*u^5-2*b*z*u^3-2*b*z*u^2+3*b*z^2+c*x,
   a*u^7+a*u^6-2*a*z*u^4+a*u^5-2*a*z*u^3-2*a*z*u^2+3*a*z^2+c*y,
   b*z*u^6+9142*b*z*u^5+13715*b*z^2*u^3-9143*b*z*u^4-9145*b*u^5-13716*b*z^2*u^2-13712*b*z^2*u-13713*b*z*u^2+4568*b*z^2+9145*c*x*u-9145*c*x+4572*d*x,
   a*z*u^6+9142*a*z*u^5+13715*a*z^2*u^3-9143*a*z*u^4-9145*a*u^5-13716*a*z^2*u^2-13712*a*z^2*u-13713*a*z*u^2+4568*a*z^2+9145*c*y*u-9145*c*y+4572*d*y,
   c*u^8+7111*c*z*u^6+3556*d*u^7+10667*c*z*u^5+3556*d*u^6+14224*c*z^2*u^3+14223*c*z*u^4-7112*d*z*u^4+3556*d*u^5+10668*c*z^2*u^2-7112*d*z*u^3+7112*c*z^2*u-7112*d*z*u^2+10668*d*z^2);
R = S/I
time R' = integralClosure(R, Strategy=>{RadicalCodim1})
use R
--assert(conductor icMap R == ideal"x,y,z-u,u2-u") -- MES: get conductor working on these...
///

-- integrally closed test
TEST ///
R = QQ[u,v]/ideal(u+2)
time J = integralClosure (R,Variable => symbol a) 
use ring ideal J
assert(ideal J == ideal(u+2))
icFractions R  -- NOT GOOD?
///

-- degrees greater than 1 test
TEST ///
R = ZZ/101[symbol x..symbol z,Degrees=>{2,5,6}]/(z*y^2-x^5*z-x^8)
time J = integralClosure (R,Variable => symbol b) 
use ring ideal J
answer = ideal(b_(1,0)*x^2-y*z, x^6-b_(1,0)*y+x^3*z, -b_(1,0)^2+x^4*z+x*z^2)
assert(ideal J == answer)
use R
assert(conductor(R.icMap) == ideal(x^2,y))
assert((icFractions R) == first entries substitute(matrix {{y*z/x^2, x, y, z}},frac R))
///

-- multigraded test
TEST ///
R = ZZ/101[symbol x..symbol z,Degrees=>{{1,2},{1,5},{1,6}}]/(z*y^2-x^5*z-x^8)
time J = integralClosure (R,Variable=>symbol a) 
use ring ideal J
assert(ideal J == ideal(-x^6+a_(1,0)*y-x^3*z,-a_(1,0)*x^2+y*z,a_(1,0)^2-x^4*z-x*z^2))
use R
assert(0 == matrix{icFractions R} - matrix {{y*z/x^2, x, y, z}})
///

-- multigraded homogeneous test
TEST ///
R = ZZ/101[symbol x..symbol z,Degrees=>{{4,2},{10,5},{12,6}}]/(z*y^2-x^5*z-x^8)
time J = integralClosure (R,Variable=>symbol a) 
use ring ideal J
assert(ideal J == ideal(a_(1,0)*x^2-y*z,a_(1,0)*y-x^6-x^3*z,a_(1,0)^2-x^4*z-x*z^2))
use R
assert(0 == matrix {icFractions R} - matrix {{y*z/x^2, x, y, z}})
assert(conductor(R.icMap) == ideal(x^2,y))
///

-- Reduced not a domain test
TEST ///
S=ZZ/101[symbol a,symbol b,symbol c, symbol d]
I=ideal(a*(b-c),c*(b-d),b*(c-d))
R=S/I                              
compsR = apply(decompose ideal R, J -> S/J)
ansR = compsR/integralClosure
compsR/icFractions
apply(decompose ideal R, J -> integralClosure(S/J))
assert all(compsR/icMap, f -> f == 1)
///

--Craig's example as a test
TEST ///
S=ZZ/101[symbol x,symbol y,symbol z,MonomialOrder => Lex]
I=ideal(x^6-z^6-y^2*z^4)
Q=S/I
time J = integralClosure (Q, Variable => symbol a)
use ring ideal J
assert(ideal J == ideal (x^2-a_(3,0)*z, a_(3,0)*x-a_(4,0)*z, a_(3,0)^2-a_(4,0)*x, a_(4,0)^2-y^2-z^2))
use Q
assert(conductor(Q.icMap) == ideal(z^3,x*z^2,x^3*z,x^4))
assert(matrix{icFractions Q} == substitute(matrix{{x^3/z^2,x^2/z,x,y,z}},frac Q))
///

--Mike's inhomogenous test
TEST ///
R = QQ[symbol a..symbol d]
I = ideal(a^5*b*c-d^2)
Q = R/I
L = time integralClosure(Q,Variable => symbol x, Keep=>{})
use ring ideal L
assert(ideal L == ideal(x_(1,0)^2-a*b*c))
use Q
matrix{icFractions Q} == matrix{{d/a^2,a,b,c}}
///

TEST ///
-- rational quartic, to make sure S2 is not being forgotten!
S = QQ[a..d]
I = monomialCurveIdeal(S,{1,3,4})
R = S/I
R' = integralClosure R
assert(numgens R' == 5)
///
--Ex from Wolmer's book - tests longer example and published result.
TEST ///
R = ZZ/101[symbol a..symbol e]
I = ideal(a^2*b*c^2+b^2*c*d^2+a^2*d^2*e+a*b^2*e^2+c^2*d*e^2,a*b^3*c+b*c^3*d+a^3*b*e+c*d^3*e+a*d*e^3,a^5+b^5+c^5+d^5-5*a*b*c*d*e+e^5,a^3*b^2*c*d-b*c^2*d^4+a*b^2*c^3*e-b^5*d*e-d^6*e+3*a*b*c*d^2*e^2-a^2*b*e^4-d*e^6,a*b*c^5-b^4*c^2*d-2*a^2*b^2*c*d*e+a*c^3*d^2*e-a^4*d*e^2+b*c*d^2*e^3+a*b*e^5,a*b^2*c^4-b^5*c*d-a^2*b^3*d*e+2*a*b*c^2*d^2*e+a*d^4*e^2-a^2*b*c*e^3-c*d*e^5,b^6*c+b*c^6+a^2*b^4*e-3*a*b^2*c^2*d*e+c^4*d^2*e-a^3*c*d*e^2-a*b*d^3*e^2+b*c*e^5,a^4*b^2*c-a*b*c^2*d^3-a*b^5*e-b^3*c^2*d*e-a*d^5*e+2*a^2*b*c*d*e^2+c*d^2*e^4)
S = R/I
icFractions S
time Sbar = integralClosure S
M:=pushForward (icMap S, Sbar^1);
assert(degree (M/(M_0)) == 2)
assert(# icFractions S == 7)
///

///  -- this is part of the above example.  But what to really place into the test?
time integralClosure (target((makeS2(S))_0), Verbosity => 3)
StoSbar = (makeS2(S))_0;
M:=pushForward (StoSbar, (target StoSbar)^1);
gens M
N=prune(M/M_0)
assert(degree N == 2)


integralClosure(S)
time V = integralClosure (S, Variable => X)
degree S
codim singularLocus S
use ring ideal V

oldanswer = ideal(a^2*b*c^2+b^2*c*d^2+a^2*d^2*e+a*b^2*e^2+c^2*d*e^2,
	   a*b^3*c+b*c^3*d+a^3*b*e+c*d^3*e+a*d*e^3,
	   a^5+b^5+c^5+d^5-5*a*b*c*d*e+e^5,
	   a*b*c^4-b^4*c*d-X_0*e-a^2*b^2*d*e+a*c^2*d^2*e+b^2*c^2*e^2-b*d^2*e^3,
	   a*b^2*c^3+X_1*d+a*b*c*d^2*e-a^2*b*e^3-d*e^5,
	   a^3*b^2*c-b*c^2*d^3-X_1*e-b^5*e-d^5*e+2*a*b*c*d*e^2,
	   a^4*b*c+X_0*d-a*b^4*e-2*b^2*c^2*d*e+a^2*c*d*e^2+b*d^3*e^2,
	   X_1*c+b^5*c+a^2*b^3*e-a*b*c^2*d*e-a*d^3*e^2,
	   X_0*c-a^2*b^2*c*d-b^2*c^3*e-a^4*d*e+2*b*c*d^2*e^2+a*b*e^4,
	   X_1*b-b*c^5+2*a*b^2*c*d*e-c^3*d^2*e+a^3*d*e^2-b*e^5,
	   X_0*b+a*b*c^2*d^2-b^3*c^2*e+a*d^4*e-a^2*b*c*e^2+b^2*d^2*e^2-c*d*e^4,
	   X_1*a-b^3*c^2*d+c*d^2*e^3,X_0*a-b*c*d^4+c^4*d*e,
	   X_1^2+b^5*c^5+b^4*c^3*d^2*e+b*c^2*d^3*e^4+b^5*e^5+d^5*e^5,
	   X_0*X_1+b^3*c^4*d^3-b^2*c^7*e+b^2*c^2*d^5*e-b*c^5*d^2*e^2-
	     a*b^2*c*d^3*e^3+b^4*c*d*e^4+a^2*b^2*d*e^5-a*c^2*d^2*e^5-b^2*c^2*e^6+b*d^2*e^7,
	   X_0^2+b*c^3*d^6+2*b^5*c*d^3*e+c*d^8*e-b^4*c^4*e^2+a^3*c^3*d^2*e^2+
	     2*a^2*b^3*d^3*e^2-5*a*b*c^2*d^4*e^2+4*b^3*c^2*d^2*e^3-3*a*d^6*e^3+
	     5*a^2*b*c*d^2*e^4-b^2*d^4*e^4-2*b*c^3*d*e^5-a^3*b*e^6+3*c*d^3*e^6-a*d*e^8)

-- We need to check the correctness of this example!
newanswer = ideal(
  a^2*b*c^2+b^2*c*d^2+a^2*d^2*e+a*b^2*e^2+c^2*d*e^2,
    a*b^3*c+b*c^3*d+a^3*b*e+c*d^3*e+a*d*e^3,
    a^5+b^5+c^5+d^5-5*a*b*c*d*e+e^5,
    X_1*e-a^3*b^2*c+b*c^2*d^3,
    X_1*d+a*b^2*c^3-b^5*d-d^6+3*a*b*c*d^2*e-a^2*b*e^3-d*e^5,
    X_1*c-c*d^5+a^2*b^3*e+a*b*c^2*d*e-a*d^3*e^2,
    X_1*b-b^6-b*c^5-b*d^5+4*a*b^2*c*d*e-c^3*d^2*e+a^3*d*e^2-b*e^5,
    X_1*a-a*b^5-b^3*c^2*d-a*d^5+2*a^2*b*c*d*e+c*d^2*e^3,
    X_0*e-a*b*c^4+b^4*c*d,
    X_0*d+a^4*b*c-a^2*b^2*d^2+a*c^2*d^3-a*b^4*e-b^2*c^2*d*e+a^2*c*d*e^2,
    X_0*c-2*a^2*b^2*c*d+a*c^3*d^2-a^4*d*e+b*c*d^2*e^2+a*b*e^4,
    X_0*b-a^2*b^3*d+2*a*b*c^2*d^2+a*d^4*e-a^2*b*c*e^2-c*d*e^4,
    X_0*a-a^3*b^2*d+a^2*c^2*d^2-b*c*d^4+a*b^2*c^2*e+c^4*d*e-a*b*d^2*e^2,
    X_1^2-b^10-b^5*c^5+2*a*b^2*c^3*d^4-2*b^5*d^5-d^10-5*b^4*c^3*d^2*e+6*a*b*c*d^6*e-6*a^3*b^4*d*e^2-4*b^3*c*d^4*e^2+2*a^2*b*d^4*e^3-4*a*b^3*d^2*e^4+b*c^2*d^3*e^4-b^5*e^5-d^5*e^5,
    X_0*X_1-a^2*b^7*d+b^3*c^4*d^3+a^4*b*c*d^4-a^2*b^2*d^6+a*c^2*d^7+4*b^2*c^2*d^5*e+b^6*d^2*e^2+b*c^5*d^2*e^2+3*a^2*c*d^5*e^2+b*d^7*e^2+a^4*b^3*e^3+4*c^3*d^4*e^3-2*a^3*d^3*e^4+b*d^2*e^7,
    X_0^2-a^4*b^4*d^2-a^2*c^4*d^4+7*b*c^3*d^6-2*b^5*c*d^3*e-2*c^6*d^3*e+2*a^3*b*d^5*e+5*c*d^8*e+a^3*c^3*d^2*e^2-6*a^2*b^3*d^3*e^2-a*b*c^2*d^4*e^2-2*a*b^5*d*e^3-2*b^3*c^2*d^2*e^3+5*a*d^6*e^3-a^2*b*c*d^2*e^4+a^3*b*e^6+c*d^3*e^6+a*d*e^8)

assert(ideal V == newanswer)   
icFractions S
///

-- Test of icFractions
--TEST 
--///
--S = QQ [(symbol Y)_1, (symbol Y)_2, (symbol Y)_3, (symbol Y)_4, symbol x, symbol y, Degrees => {{7, 1}, {5, 1}, {6, 1}, {6, 1}, {1, 0}, {1, 0}}, MonomialOrder => ProductOrder {4, 2}]
--J = ideal(Y_3*y-Y_2*x^2,Y_3*x-Y_4*y,Y_1*x^3-Y_2*y^5,Y_3^2-Y_2*Y_4*x,Y_1*Y_4-Y_2^2*y^3)
--T = S/J       
--assert(icFractions T == substitute(matrix {{(Y_2*y^2)/x, (Y_1*x)/y, Y_1, Y_2, Y_3, Y_4, x, y}}, frac T))
--///

-- Test of isNormal
TEST ///
S = ZZ/101[x,y,z]/ideal(x^2-y, x*y-z^2)
assert(isNormal(S) == false)
assert(isNormal(integralClosure(S)) == true)
///

-- Test of icMap and conductor
TEST ///
R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4)
J = integralClosure(R);
F = R.icMap
assert(conductor F == ideal((R_2)^3, (R_0)*(R_2)^2, (R_0)^3*(R_2), (R_0)^4))
icFractions R
///

TEST ///
R = QQ[x,y]/(y^2-x^3)
R' = integralClosure(R, Keep=>{})
assert(numgens R' == 1)
assert(numgens ideal R' == 0)
assert(icFractions R == {y/x})
F = icMap R
assert(target F === R')
assert(source F === R)
///

TEST ///
--huneke2
kk = ZZ/32003
S = kk[a,b,c]
F = a^2*b^2*c+a^4+b^4+c^4
J = ideal jacobian ideal F
substitute(J:F, kk) -- check local quasi-homogeneity!
I=ideal first (flattenRing reesAlgebra J)
betti I
R = (ring I)/I
--time R'=integralClosure(R, Strategy => {StartWithOneMinor}, Verbosity =>3 ) -- this is bad in the first step!
time R'=integralClosure(R, Verbosity =>3) -- this one takes perhaps too long for a test
assert(numgens R' == 13)
assert(numgens ideal gens gb ideal R' == 54) -- this is not an invariant...!
R = (ring I)/I
time R'=integralClosure(R, Verbosity =>3, Strategy=>{RadicalCodim1})
assert(numgens R' == 13)
assert(numgens ideal gens gb ideal R' == 54) -- this is not an invariant!
icFractions R
///

end 

TEST ///
  S = QQ[y,x,MonomialOrder=>Lex]
  F = poly"y5-y2+x3+x4"
  factor discriminant(F,y)
  R=S/F
  R' = integralClosure R
  icFractions R
  describe R'
///

TEST ///
  -- of idealizer
  S = QQ[y,x,MonomialOrder=>Lex]
  F = poly"y4-y2+x3+x4"
  factor discriminant(F,y)
  R=S/F
  L = trim radical ideal(x_R)
  (f1,g1,fra) = idealizer(L,L_0)
  U = target f1
  K = frac R
  f1
  g1
  fra

  L = trim ideal jacobian R

  R' = integralClosure R
  icFractions R
  icMap R
///

















---- Homogeneous Ex
loadPackage"IntegralClosure"
R = ZZ/101[x,y, z]
I1 = ideal(x,y-z)
I2 = ideal(x-3*z, y-5*z)
I3 = ideal(x,y)
I4 = ideal(x-5*z,y-2*z)

I = intersect(I1^3, I2^3, I3^3, I4^3)
f = I_0 + I_1 + I_2+ I_3
S = R/f
V = integralClosure(S)
ring(presentation V)

installPackage "IntegralClosure"

-- Tests that Mike has added:
loadPackage "IntegralClosure"
S = ZZ/101[a..d]
I = ideal(b^2-b)
R = S/I
integralClosure(R)

-- M2 crash:
kk = QQ
R = kk[x,y,z, MonomialOrder => Lex]
p1 = ideal"x,y,z"
p2 = ideal"x,y-1,z-2"
p3 = ideal"x-2,y,5,z"
p4 = ideal"x+1,y+1,z+1"
D = trim intersect(p1^3,p2^3,p3^3,p4^3)
betti D
B = basis(4,D)
F = (super(B * random(source B, R^{-4})))_(0,0)
ideal F + ideal jacobian matrix{{F}}
decompose oo

factor F
A = R/F
loadPackage "IntegralClosure"

ideal F + ideal jacobian matrix{{F}}
decompose oo
-------------------

kk = ZZ/101
R = kk[x,y,z]
p1 = ideal"x,y,z"
p2 = ideal"x,y-1,z-2"
p3 = ideal"x-2,y,5,z"
p4 = ideal"x+1,y+1"
D = trim intersect(p1^3,p2^3,p3^3,p4^2)
betti D
B = basis(5,D)
F = (super(B * random(source B, R^{-5})))_(0,0)
factor F
A = R/F
JF = trim(ideal F + ideal jacobian matrix{{F}})
codim JF
radJF = radical(JF, Strategy=>Unmixed)
NNL = radJF
NNL = substitute(NNL,A)
(phi,fracs) = idealizer(NNL,NNL_0)
phi
#fracs

----------------------
random(ZZ,Ideal) := opts -> (d,J) -> random({d},J,opts)
random(List,Ideal) := opts -> (d,J) -> (
     R := ring J;
     B := basis(6,J);
     (super(B * random(source B, R^(-d), opts)))_(0,0)
     )

kk = ZZ/101
R = kk[x,y,z]
p1 = ideal"x,y,z"
p2 = ideal"x,y-1,z-2"
p3 = ideal"x-2,y,5,z"
p4 = ideal"x+1,y+1"
D = trim intersect(p1^3,p2^3,p3^3,p4^3)
betti D
F = random(6,D)
factor F
A = R/F
JF = trim(ideal F + ideal jacobian matrix{{F}})
codim JF
radJF = radical(JF, Strategy=>Unmixed)
decompose radJF
integralClosure A

---------------------- Birational Work

R = ZZ/101[b_1, x,y,z, MonomialOrder => {GRevLex => {7}, GRevLex=>{2,5,6}}]
R = ZZ/101[x,y,z]
S = R[b_1, b_0]
I = ideal(b_1*x^2-42*y*z, x^6+12*b_1*y+ x^3*z, b_1^2 - 47*x^4*z - 47*x*z^2)
I = ideal(b_1*x-42*b_0, b_0*x-y*z, x^6+12*b_1*y+ x^3*z, b_1^2 -47*x^4*z - 47*x*z^2, b_0^2-x^6*z - x^4*z^2)
leadTerm gens gb I

R = ZZ/101[x,y,z]/(z*y^2-x^5*z-x^8)
J = integralClosure(R)
R.icFractions
describe J


S=ZZ/101[symbol x,symbol y,symbol z,MonomialOrder => Lex]
I=ideal(x^6-z^6-y^2*z^4)
Q=S/I
time J = integralClosure (Q, Variable => symbol a)


S = ZZ/101[a_7,a_6,x,y,z, MonomialOrder => {GRevLex => 2, GRevLex => 3}]
Inew = ideal(x^2-a_6*z,a_6*x-a_7*z,a_6^2-a_7*x,a_7^2-y^2-z^2)
leadTerm gens gb Inew
radical ideal oo


--- Recent tests and experiments for integral closure.
///
restart
loadPackage"IntegralClosure"
R=ZZ/2[x,y,Weights=>{{8,9},{0,1}}]
I=ideal(y^8+y^2*x^3+x^9) -- eliminates x and y at some point. 
R=ZZ/2[x,y,Weights=>{{31,12},{0,1}}]
I=ideal"y12+y11+y10x2+y8x9+x31" -- really long, should it really be this bad?
A = R/I
time A' = integralClosure(A, Verbosity => 1)
transpose gens ideal S
///



--------------
-- Examples --
--------------

R = QQ[y,x]/(y^2-x^4-x^7)
integralClosure R
icFractions R
icMap R

--from Eisenbud-Neumann p.11: simplest poly with 2 characteristic pairs. 
R = QQ[y,x]/(y^4-2*x^3*y^2-4*x^5*y+x^6-x^7)
time R' = integralClosure R
icFractions R
icMap R

R = QQ[x,y]/(y^4-2*x^3*y^2-4*x^5*y+x^6-x^7)
time R' = integralClosure R
icFractions R
icMap R

R = ZZ/32003[x,y,z]/(z^3*y^4-2*x^3*y^2*z^2-4*x^5*y*z+x^6*z-x^7)
isHomogeneous R
time R' = integralClosure R
icFractions R
icMap R

kk = ZZ/32003
S = kk[v,u]
I=ideal(5*v^6+7*v^2*u^4+6*u^6+21*v^2*u^3+12*u^5+21*v^2*u^2+6*u^4+7*v^2*u)
R = S/I
L = frac R
time R' = integralClosure R
ideal R'
icFractions R
conductor icMap R -- can't do it since not homogeneous

-- Doug Leonard example ----------------------
restart
S=ZZ/2[z19,y15,y12,x9,u9,MonomialOrder=>{Weights=>{19,15,12,9,9},Weights=>{12,9,9,9,0},1,2,2}]

I = ideal(
     y15^2+y12*x9*u9,
     y15*y12+x9^2*u9+x9*u9^2+y15,
     y12^2+y15*x9+y15*u9+y12,
     z19^3+y12*x9^3*u9^2+z19*y15*x9*u9+y15*x9^3*u9+y15*x9^2*u9^2
       +z19*y12*x9*u9+z19*y15*u9+z19*y12*u9+y12*x9^2*u9)

isHomogeneous I
R = S/I;

time icFractions R
errorDepth=0
time A = icFracP R
time A = integralClosure R;
----------------------------------------------
-- Another example from Doug Leonard
S = ZZ/2[z19,y15,y12,x9,u9,MonomialOrder=>{Weights=>{19,15,12,9,9},Weights=>{12,9,9,9,0},1,2,2}]
I = ideal(y15^3+x9*u9*y15+x9^3*u9^2+x9^2*u9^3,y15^2+y12*x9*u9,z19^3+(y12+y15)*(x9+1)*u9*z19+(y12*(x9*u9+1)+y15*(x9+u9))*x9^2*u9)
R = S/I

time A = integralClosure R;


S = ZZ/2[z19,y15,y12,x9,u9,MonomialOrder=>{Weights=>{19,15,12,9,9},Weights=>{12,9,9,9,0},1,2,2}]

I = ideal(
     y15^2+y12*x9*u9,
     y15*y12+x9^2*u9+x9*u9^2+y15,
     y12^2+y15*x9+y15*u9+y12,
     z19^3+y12*x9^3*u9^2+z19*y15*x9*u9+y15*x9^3*u9+y15*x9^2*u9^2
       +z19*y12*x9*u9+z19*y15*u9+z19*y12*u9+y12*x9^2*u9)

isHomogeneous I
R = S/I;

errorDepth=0
time A = icFracP R
time A = integralClosure R;
icFractions R
----------------------------------------------
restart
load "IntegralClosure.m2"
S = ZZ/32003[x,y];
F = (y^2-3/4*y-15/17)^3-9*y*(y^2-3/4*y-15/17*x)-27*x^11
R = S/F
time R' = integralClosure R
use ring F
factor discriminant(F,y)
factor discriminant(F,x)
----------------------------------------------

restart
load "IntegralClosure.m2"

S=ZZ/2[x,y,Weights=>{{8,9},{0,1}}]
I=ideal(y^8+y^2*x^3+x^9) -- eliminates x and y at some point. 
R=S/I
time R'=integralClosure(R, Strategy => {StartWithOneMinor})--, Verbosity =>3 )
time R'=integralClosure(R)--, Verbosity =>3)
icFractions R

S=ZZ/2[x,y,Weights=>{{31,12},{0,1}}]
I=ideal"y12+y11+y10x2+y8x9+x31" 
R = S/I
time R'=integralClosure(R) -- really long?
transpose gens ideal S

S=ZZ/2[x,y]
I=ideal"y12+y11+y10x2+y8x9+x31" 
R = S/I
time R'=integralClosure(R) -- really long?
transpose gens ideal S
icFracP R -- very much faster!
----------------------------------------------

restart
-- in IntegralClosure dir:
load "runexamples.m2"
runExamples(H,10,Verbosity=>3)


restart
load "IntegralClosure.m2"
kk = QQ
S = kk[x,y,u]
R = S/(u^2-x^3*y^3)
time integralClosure R

-- another example from Doug Leonard ----------------------------
S=ZZ/2[z,y,x,MonomialOrder=>{Weights=>{32,21,14}}];
I=ideal(z^7+x^5*(x+1)^5*(x^2+x+1)^3,y^2+y*x+x*(x^2+x+1));
R=S/I;
time P=presentation(integralClosure(R));    -- used 4.73 seconds
toString(gens gb P)

loadPackage "FractionalIdeals"
S=ZZ/2[z,y,x,MonomialOrder=>{2,1}];
I=ideal(z^7+x^5*(x+1)^5*(x^2+x+1)^3,y^2+y*x+x*(x^2+x+1));
R=S/I;
time integralClosureHypersurface(R) -- doesn't work yet
use R
time integralClosureDenominator(R,x^16+x^14+x^13+x^11+x^10+x^8+x^7+x^5)
-----------------------------------------------------------------