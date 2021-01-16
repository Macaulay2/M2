newPackage(
	"RandomSpaceCurves",
    	Version => "0.5",
    	Date => "March 1, 2011",
    	Authors => {
	        {Name => "Hans-Christian Graf v. Bothmer",
	         Email => "bothmer@uni-math.gwdg.de",
		 HomePage => "http://www.crcg.de/wiki/User:Bothmer"},

		{Name=> "Florian Geiss",
		 Email=> "fg@math.uni-sb.de",
		 HomePage=>"http://www.math.uni-sb.de/ag/schreyer/"},

	        {Name => "Frank-Olaf Schreyer",
		 Email => "schreyer@math.uni-sb.de",
		 HomePage => "http://www.math.uni-sb.de/ag/schreyer/"}
                   },
    	Headline => "random smooth space curves",
	Keywords => {"Examples and Random Objects"},
     	PackageExports => {"RandomObjects"},
    	DebuggingMode => false
        )

if not version#"VERSION" >= "1.8" then
  error "this package requires Macaulay2 version 1.8 or newer"

export{"randomSpaceCurve",
     "hartshorneRaoModule",
     "constructHartshorneRaoModule",
     "certifyHartshorneRaoModule",
     "knownUnirationalComponentOfSpaceCurves",
     "hilbertNumerator",
     "expectedBetti",
     "certifyRandomSpaceCurve",
     "spaceCurve"
     }

------------------------------------
-- Hilbert Function and Numerator --
------------------------------------

-- calculate the numerator of a Hilbert function
-- from the first d+r+1 values where
-- d is the regularity of the corresponding module
-- and r is the dimension of the ambient space
--
-- L = a list of dimensions
-- r = the dimension of the ambient space
-- t = the variable to be used in the numerator
hilbertNumerator=method()
hilbertNumerator(List,ZZ,RingElement):=(L,r,t)->(
     -- the beginning of the hilbert series
     p:=sum(#L,i->L#i*t^i);
     -- the numerator
     p*(1-t)^(r+1)%t^(#L)
     )

TEST ///
   T = QQ[t];
   assert (hilbertNumerator({1,3,0,0,0,0},3,t) == 3*t^5-11*t^4+14*t^3-6*t^2-t+1)
///

TEST ///
    T = QQ[t];
    assert (hilbertNumerator({1,4,10,15,20,25,30,35,40},3,t) == -t^5+5*t^4-5*t^3+1)
///



-----------------------------
-- Expected Betti Tableaus --
-----------------------------

-- convert c*t^d to (c,({d},d))
-- assumes only one term c*t^d
-- ring of t must be over ZZ or QQ
-- and singly graded
--
-- this function is needed to construct
-- expected betti tables from
-- a HilberNumerator
termToBettiKey = (mon) -> (
     -- the coefficient of the monomial
     c := lift((last coefficients mon)_0_0,ZZ);
     -- the degree of the monmial
     d := sum degree mon;
     (c,({d},d))
     )

--TEST ///
--  T = QQ[t];
--  assert (termToBettiKey(-4*t^3,T)==(-4,({3},3)))
--///


-- construct a minimal free resolution with expected betti tableau
expectedBetti=method()


-- calculates the expected betti tableau
-- from a hilbert Numerator
--
-- For this every term a_i*t^i will represent a summand R^{abs(a_i):-i}
-- in the ChainComplex represented by the desired BettiTableau
-- The step where this summand is used depends on the number of
-- sign switches that occur in the hilbert numerator before this monomial
--
-- the ring of the hilbert numerator is expected to singly graded
-- and contain only one variable
expectedBetti(RingElement):= (hilbNum) ->(
     -- find terms of hilbert Numerator
     -- smallest degree first
     termsHilbNum := reverse terms hilbNum;
     -- convert terms into pairs (coefficient, ({d},d))
     bettiKeys := apply(termsHilbNum,m->termToBettiKey(m));
     -- put the summands into the appropriate step of F
     -- j contains the current step
     j := -1;
     -- previous Coefficient is needed to detect sign changes
     previousCoefficient := -(first bettiKeys)#0;
     -- step through all keys and calculate which step a
     -- given entry must go based on the number of sign-changes
     L := for b in bettiKeys list (
	  -- has a sign change occurred?
     	  if (b#0*previousCoefficient) < 0 then (
	       -- sign change => next step in the resolution
	       j = j+1;
	       );
	  -- store previous coefficient
     	  previousCoefficient = b#0;
	  -- make entry for the betti Tally
	  (prepend(j,b#1) => abs(b#0))
     	  );
     -- return the complex
     new BettiTally from L
     )

TEST ///
    T = QQ[t];
    e = expectedBetti(t^5-5*t^4+5*t^3-1)
    b = new BettiTally from {
	 (0,{0},0) => 1,
	 (1,{3},3) => 5,
      	 (2,{4},4) => 5,
	 (3,{5},5) => 1
    	 }
    assert(e == b)
///



-- calculate the expected betti tableau
-- from a given hilbert function.
-- hilb = {h0,...,h_(d+r+1)}
-- where d is the regularity of the variety described
-- and r is the dimension of the ambient space
expectedBetti(List,ZZ) := (L,r)->(
     t := local t;
     T := QQ[t];
     expectedBetti(hilbertNumerator(L,r,t))
     )

TEST ///
    T = QQ[t];
    e = expectedBetti({1,3,0,0,0,0},3)
    b = new BettiTally from {
	 (0,{0},0) => 1,
	 (1,{1},1) => 1,
      	 (1,{2},2) => 6,
	 (2,{3},3) => 14,
	 (3,{4},4) => 11,
	 (4,{5},5) => 3};
    assert(e == b)
///



-- calculate the expected betti tableau
-- for a curve of degree d, genus g in IP^r.
-- we assume C non-degenerate, O_C(2) nonspecial and maximal rank
expectedBetti(ZZ,ZZ,ZZ) := (g,r,d)->(
     b := d+r+1;
     L := apply(b,i->(if i>1 then
	       min(d*i+1-g,binomial(r+i,r))
	       else binomial(r+i,r)));
     expectedBetti(L,r)
     )

TEST ///
    e = expectedBetti(1,3,5)
    b = new BettiTally from {
	 (0,{0},0) => 1,
	 (1,{3},3) => 5,
      	 (2,{4},4) => 5,
	 (3,{5},5) => 1
    	 };
    assert(e == b)
///





-- given a betti Table b and a Ring R make a chainComplex
-- with zero maps over R  that has betti diagram b.
--
-- negative entries are ignored
-- rational entries produce an error
-- multigraded R's work only if the betti Tally
-- contains degrees of the correct degree length
Ring ^ BettiTally := (R,b) -> (
     F := new ChainComplex;
     F.ring = R;
     --apply(pDim b,i->F_i = null);
     for k in keys b do (
	  -- the keys of a betti table have the form
	  -- (homological degree, multidegree, weight)
	  (i,d,h) := k;
	  -- use F_i since it gives 0 if F#0 is not defined
	  F#i = F_i ++ R^{b#k:-d};
	  );
     F
     )

TEST ///
     R = QQ[x_0..x_3];
     b = betti (random(R^{1,2},R^{0,0,1}))
     assert (b == betti (R^b))
///

--------------------
-- Finite Modules --
--------------------

-- calculate the number of expected syzygies of a
-- random a x b matrix with linear entries in R
expectedLinearSyzygies = (a,b,R) -> (
     n := dim R;
     b*n-a*binomial(n+1,2)
     )

--TEST ///
--    setRandomSeed("I am feeling lucky");
--    R = ZZ/101[x_0..x_3];
--    assert(expectedLinearSyzygies(2,6,R) ==
--	 (betti res coker random(R^{2:0},R^{6:-1}))#(2,{2},2)
--	 )
--///

-- Try to construct a random HartshorneRao module of
-- length 3 starting at the beginning of the
-- minimal free resolution.
--
-- The main difficulty is in getting the number of
-- linear syzygies of the first matrix in the resolution right
--
-- HRau = {h1,h2,h3} the Hilbertfunction of the desired module
-- R the ring where the module should live. It is assumed, that
-- this ring has 4 variables and is singly graded.
randomHartshorneRaoModuleDiameter3oneDirection = (HRao,R) -> (
     -- construct a chain complex with expected betti tableau
     -- and 0 differentials
     --
     -- calculate the expected betti diagram to find out whether linear syzygies
     -- are required (this is the difficult part in the construction)
     e := expectedBetti(HRao|{0,0,0,0},3);
     F := R^e;
     -- find betti Numbers of the linear strand
     linearStrand := for i from 0 list (if e#?(i,{i},i) then e#(i,{i},i) else break);
     -- construction depends on length of linear strand.
     if #linearStrand == 0 then error"linear Stand has length 0. This should never happen";
     if #linearStrand == 1 then (
	  -- first matrix can neither have nor be required to have linear syzygies
	  -- choose first matrix randomly
     	  return coker random (F_0,F_1)
	  );
     if #linearStrand == 2 then (
	  -- no linear syzygies of the first matrix are required
	  -- check if first matrix always has unwanted syzygies
	  if expectedLinearSyzygies(linearStrand#0,linearStrand#1,R) <= 0 then (
	       -- no unwanted syzygies
	       -- choose first matrix randomly
     	       return coker random (F_0,F_1)
	       );
     	  );
     if #linearStrand == 3 then (
	  -- is the number of expected syzygies == the number of required syzygies?
	  if expectedLinearSyzygies(linearStrand#0,linearStrand#1,R) == linearStrand#2 then (
	       -- choose first matrix randomly
     	       return coker random (F_0,F_1)
	       );
	  -- too many syzygies?
	  if expectedLinearSyzygies(linearStrand#0,linearStrand#1,R) > linearStrand#2 then (
	       -- in this case the construction method will not work
	       return null
	       );
	  -- too few syzygies?
	  if expectedLinearSyzygies(linearStrand#0,linearStrand#1,R) < linearStrand#2 then (
	       -- try to choose the syzygies first
	       -- this will work if the transpose of a generic map between
	       -- 1. and 2. module of the linear strand has more expected syzygies
	       -- than required in the 0. step
     	       if expectedLinearSyzygies(linearStrand#2,linearStrand#1,R) >= linearStrand#0 then (
	       	    -- syzygies of the transpose of second step in linear strand
	       	    s := syz random(R^{linearStrand#2:2},R^{linearStrand#1:1});
	       	    -- choose linearStrand#0 syzygies randomly among those and transpose again
	       	    return coker (transpose (s*random(source s,R^{linearStrand#0:0})));
	       	    );
	       )
      	   );
      -- if we arrive here there were either to few or to many linear
      -- syzygies required
      return null
      );



-- Try to construct a random Hartshorne-Rau module of
-- length 3 by starting at both ends of the expected
-- minimal free resolution.
--
-- HRau = {h1,h2,h3} the Hilbertfunction of the desired module
-- R the ring where the module should live. It is assumed, that
-- this ring singly graded. It is checked that the ring has 4 variables
randomHartshorneRaoModuleDiameter3 = (HRao,R)->(
     if #HRao != 3 then error"Hilbert function has to have length 3";
     -- start at the beginning of the resolution
     M := randomHartshorneRaoModuleDiameter3oneDirection(HRao,R);
     -- did this direction work?
     if M =!= null and apply(3,i->hilbertFunction(i,M)) == HRao then return M;
     -- start at the end of the resolution
     Mdual := randomHartshorneRaoModuleDiameter3oneDirection(reverse HRao,R);
     Fdual := res Mdual;
     M = (coker transpose Fdual.dd_4)**R^{ -6};
     return M
     )


-- Try to construct a random Hartshorne-Rau module of
-- length 2. Here the only problem is, that the
-- generic module may not have expected syzgies
--
-- HRau = {h1,h2} the Hilbertfunction of the desired module
-- R the ring where the module should live. It is assumed, that
-- this ring has 4 variables and is singly graded.
randomHartshorneRaoModuleDiameter2 = (HRao,R)->(
     if #HRao != 2 then error"Hilbert function has to have length 2";
     -- some special cases with non expected resolution
     --
     --if HRao == {1,1} then return coker random(R^{0},R^{3:-1,1:-2});
     --if HRao == {1,2} then return coker random(R^{0},R^{2:-1,3:-2});
     --if HRao == {2,1} then return coker random(R^{2:0},R^{7:-1});
     --
     -- the standard construction still works since the unexpected
     -- part is not in the first 2 steps.
     --
     -- now assume expected resolution
     --
     -- always start at the beginning of the resolution
     F := R^(expectedBetti(HRao|{0,0,0,0},3));
     M := coker random(F_0,F_1)
     )

-- Construct a random Hartshorne-Rau module of
-- length 1. This always works
--
-- HRau = {h1} the Hilbertfunction of the desired module
-- R the ring where the module should live. It is assumed, that
-- this ring has 4 variables and is singly graded.
randomHartshorneRaoModuleDiameter1 = (HRao,R)->(
     if #HRao != 1 then error"Hilbert function has to have length 1";
     return coker (vars R**R^{HRao#0:0})
     )

--randomHartshorneRaoModule=method()
constructHartshorneRaoModule=method(Options=>{Certify=>false})

constructHartshorneRaoModule(ZZ,List,PolynomialRing):=opt->(e,HRao,R)->(
     if dim R != 4 then error "expected a polynomial ring in 4 variables";
     if degrees R !={{1}, {1}, {1}, {1}} then error "polynomial ring is not standard graded";
     if #HRao > 3 then error "no method implemented for Hartshorne Rao module of diameter >3";
     M := null;
     if #HRao == 1 then M = randomHartshorneRaoModuleDiameter1(HRao,R);
     if #HRao == 2 then M = randomHartshorneRaoModuleDiameter2(HRao,R);
     if #HRao == 3 then M = randomHartshorneRaoModuleDiameter3(HRao,R);
     if M === null then return null else return M**R^{ -e};
     )

undocumented constructHartshorneRaoModule

certifyHartshorneRaoModule=method()
certifyHartshorneRaoModule(Module,ZZ,List,PolynomialRing):=(M,e,HRao,R)->(
       (betti res (M**R^{e})) == expectedBetti(HRao|{0,0,0,0},3)
       )

undocumented certifyHartshorneRaoModule

hartshorneRaoModule = new RandomObject from {
    Construction  => constructHartshorneRaoModule,
    Certification => certifyHartshorneRaoModule}

------------------
-- Space Curves --
------------------



-- the Harshorne Rao module of a curve is defined as
-- M = \oplus_i H^1(I_C(-i)) is can also be obtained as
-- the cokernel of the transpose of the last map
-- in a minimal free resolution of a curve
--
-- conversely one can construct a curve, by first
-- constructing the Harshorne Rao Module an therefore
-- the last matrix in the minimal free resolution of
-- the curve
randomSpaceCurve=method(TypicalValue=>Ideal,Options=>{Certify=>false})

randomSpaceCurve(ZZ,ZZ,PolynomialRing) := opt->(d,g,R)->(
     if not knownUnirationalComponentOfSpaceCurves(d,g) then return null;
     G:=R^(expectedBetti(g,dim R-1,d));
     -- calculate values of h^1 that are forced by the maximal rank assumption
     h1 := for i from 0 when ((i<4) or(d*i+1-g)>binomial(i+3,3)) list max(d*i+1-g-binomial(3+i,3),0);
     -- calculate offset (i.e. number of leading 0's in h1)
     e := 0; for i in h1 when i==0 do e=e+1;
     -- calculate support of Hartshorne Rao Moduole
     HRao := select(h1,i->i!=0);
     -- if the Hartshorne Rao Module is zero, the curve is ACM
     -- and it can be defined by the minors of an appropriate
     -- Hilbert-Birch-Matrix
     if #HRao==0 then (
	  if length G !=2
	  then error "cannot be ACM"
	  else return minors(rank G_2,random(G_1,G_2))
	  );
     M:=(random hartshorneRaoModule)(e,HRao,R);
     if M === null then return null;
     F :=res M;
     -- detect syzygies in the second step, that do not
     -- come from the HR-Module
     H := R^((betti G_2)-(betti F_3));
     -- calculate a presentation matrix of
     -- the ideal of the curve
     N := random(G_1,F_2++H_0)*(F.dd_3++id_(H_0));
     -- calculate the ideal presented by this matrix
     return ideal syz transpose N
     )

undocumented randomSpaceCurve

certifyRandomSpaceCurve=method()

-- old certification for SpaceCurves
certifyRandomSpaceCurve(Ideal,ZZ,ZZ,PolynomialRing) := (J,d,g,R)->(
     singJ := minors(2,jacobian J)+J;
     (dim singJ==0) and (g == genus J) and (d == degree J) and (2 == codim J)
)

undocumented certifyRandomSpaceCurve

knownUnirationalComponentOfSpaceCurves=method()
knownUnirationalComponentOfSpaceCurves(ZZ,ZZ) := (d,g)->(
     x := local x;
     R := QQ[x_0..x_3];
     n:=4;
     while
     d*n+1-g>binomial(n+3,3)
     do n=n+1;
     HRao1:=select(apply(toList(1..n),n->(n,max(d*n+1-g-binomial(3+n,3),0))), i-> i_1 !=0);
     G:=R^(expectedBetti(g,3,d));
     if length G >3 then return false;
     if #HRao1 >3 then return false;
     if #HRao1 <=1 then return true;
     HRao:=apply(HRao1,i->i_1);
     if #HRao <=2 then if HRao=={1,1} or HRao=={2,1} or HRao=={1,2} then return false else return true;
     a:=HRao_0,b:=HRao_1,c:=HRao_2;
     b>=4*a or b>=4*c
     or
     b<4*a and -6*a+4*b-c>=0
     or
     b<4*c and -6*c+4*b-a>=0
     or
     b<4*a and 6*c-4*b+a>0 and 4*(4*c-b)-10*(6*c-4*b+a)>=c
     or
     b<4*c and 6*a-4*b+c>0 and 4*(4*a-b)-10*(6*a-4*b+c)>=a
     )

--- interface for (random spaceCurves)

spaceCurve = new RandomObject from {
     Construction => randomSpaceCurve,
     Certification => certifyRandomSpaceCurve
     }


beginDocumentation()

doc ///
  Key
    "spaceCurve"
  Headline
    Generates the ideal of a random space curve of genus g and degree d
  Usage
    (random spaceCurve)(d,g,R)
  Inputs
    d:ZZ
        the desired degree
    g:ZZ
        the desired genus
    R:PolynomialRing
    	 homogeneous coordinate ring of $\PP^{ 3}$
  Outputs
    :Ideal
          of R
  Description
   Text
     Creates the ideal of a random curve of degree d and genus g via the construction of its expected
     Hartshorne-Rao module, which should have diameter $\le 3$. The construction is implemented for non-degenerate,
     linearly normal curves C of maximal rank with O_C(2) non-special, where moreover
     both C and its Hartshorne-Rao module
     have a "natural" free resolution.
   Text
     There are the following options:

     * {\tt Attempts => ... } a nonnegative integer or {\tt infinity} (default) that limits the maximal number
     of attempts for the construction of the curve

     * {\tt Certify => ... } {\tt true} or {\tt false} (default) checks whether the output is of correct
     dimension and the constructed curve is smooth and actually has the desired degree d and genus g

   Text
     There are 63 possible families satisfying the four conditions above.
     Our method can provide random curves in 60 of these families, simultaneously proving the unirationality of each of these 60 components of the
     Hilbert scheme.

     If there is a construction can be checked with @ TO "knownUnirationalComponentOfSpaceCurves" @.

   Example
     setRandomSeed("alpha");
     R=ZZ/20011[x_0..x_3];
     d=10;g=7;
     betti res (J=(random spaceCurve)(d,g,R))
--     betti res randomHartshorneRaoModule(d,g,R)
     degree J==d and genus J == g
   Text
     We verify that the Hilbert scheme has (at least) 60 components consisting of smooth non-degenerate curves
     with $h^1 O_C(2)=0$. The degree d, genus g and Brill-Noether number $\rho$ of these families and the generic Betti tables
     are given below.
   Example
     setRandomSeed("alpha");
     kk=ZZ/20011;
     R=kk[x_0..x_3];
     L=flatten apply(toList(0..40),g->apply(toList(3..30),d->(d,g)));
     halpenBound = d ->(d/2-1)^2;
     L = select(L,(d,g) ->
	  g <= halpenBound d
	  and
	  knownUnirationalComponentOfSpaceCurves(d,g));
     #L
     hashTable apply(L,(d,g) -> (
	       J = (random spaceCurve)(d,g,R);
	       assert (degree J == d and genus J == g);
	       (d,g) => g-4*(g+3-d) => betti res J))
  SeeAlso
    knownUnirationalComponentOfSpaceCurves
    hartshorneRaoModule
///


doc ///
  Key
    knownUnirationalComponentOfSpaceCurves
    (knownUnirationalComponentOfSpaceCurves,ZZ,ZZ)
  Headline
    check whether there is a unirational construction for a component of the Hilbert scheme of space curves
  Usage
    knownUnirationalComponentOfSpaceCurves(d,g)
  Inputs
    d: ZZ
    g: ZZ
  Outputs
     : Boolean
	  whether there is a component of maximal rank curves of degree d
	  and genus g in $\PP^{ 3}$ with O_C(2) non-special and Hartshorne-Rao module of diameter $\le 3$
	  that have a natural free resolution
  Description
    Text
      * diameter = 1. All modules can be constructed

      * diameter = 2. The modules can be constructed if the resolution of the generic module is minimal. This is for instance not the case for
      {\tt (d,g) } being among {\tt (2,1), (1,2), (1,1) }.

      * diameter = 3. The construction is possible
      unless the expected Betti table of the Hartshorne-Rao module has shape

     {\tt a b c_1 - - }

     {\tt - - c_2 - - }

     {\tt - - c_3 d e }

     with both {\tt 4b-10c_1 < a} and {\tt 4d-10c_3 < e}.


     diameter {\ge} 4. he routine returns false, although we actually do know a couple of constructions which work in a few further cases.

     The following example prints an overview table for the constructable cases:
   Example
     matrix apply(toList(2..18),d-> apply(toList(0..26),g->
	  if knownUnirationalComponentOfSpaceCurves(d,g) then 1 else 0))
  SeeAlso
    spaceCurve
    hartshorneRaoModule
///

doc ///
  Key
    hartshorneRaoModule
--    (randomHartshorneRaoModule,ZZ,ZZ,PolynomialRing)
--    (randomHartshorneRaoModule,ZZ,List,PolynomialRing)
  Headline
    Compute a random Hartshorne-Rao module
  Usage
--    randomHartshorneRaoModule(d,g,R)
    (random hartshorneRaoModule)(e,HRao,R)
  Inputs
    e: ZZ
       smallest degree of the Hartshorne-Rao module
    HRao: List
       desired dimensions of $H^1(\PP^3,I_C(n))$
    R: PolynomialRing
       coordinate ring of $\PP^{ 3}$
  Outputs
     : Module
  Description
    Text
      Returns the Hartshorne-Rao Module over {\tt R} with Hilbert function {\tt HRao} and
      expected betti table. The constructions works only for many modules with
      diameter {\le} 3.
    Example
      setRandomSeed("alpha");
      R = ZZ/101[x_0..x_3];
      betti res (random hartshorneRaoModule)(0,{1},R)
      betti res (random hartshorneRaoModule)(0,{1,4},R)
      betti res (random hartshorneRaoModule)(0,{1,4,1},R)
      betti res (random hartshorneRaoModule)(0,{1,4,2},R)
    Text
      There are the following options:

      * {\tt Attempts => ... } a nonnegative integer or {\tt infinity} (default) that limits the maximal number of attempts for the construction of the module

      * {\tt Certify => ... } {\tt true} or {\tt false} (default) checks whether the constructed module has the expected betti Table

    Example
      setRandomSeed("alpha");
      betti res (random hartshorneRaoModule)(0,{1,3,2},R)
      expectedBetti({1,3,2,0,0,0,0},3)
      null =!= (random hartshorneRaoModule)(0,{1,3,2},R)
      null =!= (random hartshorneRaoModule)(0,{1,3,2},R,Certify=>true,Attempts=>1)
    Text

      if Certify => true and Attempts=>infinity (the default!) are given in this example, the construction never stops.
  Caveat
    The list {\tt HRao} needs only to contain the non-zero values of the Hilbert function.
  SeeAlso
    spaceCurve
    knownUnirationalComponentOfSpaceCurves
///

doc ///
  Key
    expectedBetti
    (expectedBetti,RingElement)
  Headline
    compute the expected betti table from the Hilbert numerator
  Usage
    B=expectedBetti q
  Inputs
    q: RingElement
       a polynomial in ZZ[t]
  Outputs
    B: BettiTally
       a Betti table that has Hilbert numerator q,
       assuming that each sign change in the coefficients of q corresponds to a step
  Description
    Text
      calculates the expected betti table  from a given hilbert Numerator.

    Example
      T=ZZ[t]
      q=1-3*t^2+2*t^3
      expectedBetti q
      q=1-5*t^2+5*t^3-t^5
      expectedBetti q
///

doc ///
  Key
    (expectedBetti,ZZ,ZZ,ZZ)
  Usage
    B=expectedBetti(g,r,d)
  Inputs
    g: ZZ
       the genus
    r: ZZ
       dimension of $\PP^{ r}$
    d: ZZ
       the degree
  Outputs
    B: BettiTally
       a Betti table that has Hilbert numerator the same as
       for a nondegenerate maximal-rank curve of genus g and degree d in $\PP^{ r}$, with O_C(2) non-special.
  Description
    Example
      betti expectedBetti(0,4,4)
      betti expectedBetti(16,3,15)
///

doc ///
 Key
  (expectedBetti,List,ZZ)
 Usage
  B=expectedBetti(h,r)
 Inputs
  h: List
      values of the hilbert function
  r: ZZ
       dimension of ambient protective space
 Outputs
  B: BettiTally
       expected Betti table of module with Hilbert function h
 Description
  Example
    betti expectedBetti({0,0,4,6,3,0,0,0,0},3)
 Caveat
  The hilbert function has to be given at positions {\tt 0} to {\tt d+r+1} where {\tt d} is the regularity of the considered variety
///


doc ///
 Key
   hilbertNumerator
   (hilbertNumerator,List,ZZ,RingElement)
 Headline
   calculate Hilbert numerator from Hilbert function
 Usage
   p=hilbertNumerator(L,r,t)
 Inputs
   L: List
   	values of the hilbert function
   r: ZZ
       dimension of ambient projective space
   t: RingElement
       variable in which the hilbertNumerator is given
 Description
  Example
    T=QQ[t];
    hilbertNumerator({0,0,4,6,3,0,0,0,0},3,t)
 Caveat
  The hilbert function has to be given at positions {\tt 0} to {\tt d+r+1} where {\tt d} is the regularity of the considered variety

///

-- calculate the numerator of a Hilbert function
-- from the first d+r+1 values where
-- d is the regularity of the corresponding module
-- and r is the dimension of the ambient space
--
-- L = a list of dimensions
-- r = the dimension of the ambient space
-- t = the variable to be used in the numerator


doc ///
  Key
    "RandomSpaceCurves"
  Headline
    Construction of random space curves of various kinds.
  Description
    Text
     This package provides the construction of random curves $C \subset \mathbb{P}^{ 3}$ for various values for its degree $d$ and genus $g$.
     A space curve $C \subset \mathbb{P}^{ 3}$ is constructed via its Hartshorne-Rao module $M= H^1_*(\mathcal{I}_C(n))$.
     In particular, there are constructions for random points in $M_g$ for $g=11,12,13$.

     For a algorithms and theoretical background see
     @ HREF("http://www.math.uiuc.edu/Macaulay2/Book/", "Needles in a Haystack") @

  ///


-------------- TESTS --------------------

TEST ///
     setRandomSeed("alpha");
     R=ZZ/32003[x_0..x_3];
     d=12,g=11;
     betti(J=(random spaceCurve)(d,g,R,Certify=>true))
     assert (degree J==d and genus J == g)
///

TEST ///
     setRandomSeed("alpha");
     R=(ZZ/32003)[x_0..x_3]
     HRao = {1,4,2};
     e = 1;
     betti res (M=(random hartshorneRaoModule)(1,HRao,R))
     assert(apply(toList(e..e+#HRao-1),i->hilbertFunction(i,M))==HRao)
///


end

restart
uninstallPackage("RandomSpaceCurves")
installPackage("RandomSpaceCurves",RerunExamples=>true,RemakeAllDocumentation=>true);

check("RandomSpaceCurves")

viewHelp"RandomSpaceCurves"

matrix apply(toList(2..18),d-> apply(toList(0..26),g->
	  if knownUnirationalComponentOfSpaceCurves(d,g) then 1 else 0))

restart
needsPackage("RandomSpaceCurves")

R = (ZZ/7)[x_0..x_3]
betti res (random spaceCurve)(12,11,R)

time tally apply(10,i->null === (random spaceCurve)(12,11,R,Certify=>true,Attempts=>1))

time tally apply(10,i->time certifyRandomSpaceCurve(randomSpaceCurve(12,11,R),12,11,R))

R = ZZ[]/49
(matrix{{11_R,12_R},{13_R,14_R}})^-1
--
R=ZZ/101[x_0..x_3];


