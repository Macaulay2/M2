newPackage(
	"RandomCurves",
    	Version => "1.0",
    	Date => "March 1, 2011, updated and combined on June 8, 2026 by D Eisenbud",
    	Authors => {
	        {Name => "Hans-Christian Graf v. Bothmer",
	         Email => "hcvbothmer@gmail.com",
		 HomePage => "https://www.math.uni-hamburg.de/personen/bothmer/"},

		{Name=> "Florian Geiss",
		 Email=> "fg@math.uni-sb.de",
		 HomePage=>"https://www.math.uni-sb.de/ag/schreyer/"},

	        {Name => "Frank-Olaf Schreyer",
		 Email => "schreyer@math.uni-sb.de",
		 HomePage => "https://www.math.uni-sb.de/ag/schreyer/"}
                   },
    	Headline => "random smooth curves up to genus 14",
	Keywords => {"Projective Algebraic Geometry", "Examples and Random Objects"},
	PackageImports => {"Complexes"},
	PackageExports => {"RandomObjects"},
    	DebuggingMode => false,
        )

if not version#"VERSION" >= "1.8" then
  error "this package requires Macaulay2 version 1.8 or newer"

-*
compilattion by David Eisenbud, May 2026,
of functions, documentation and tests from
RandomSpaceCurves,
RandomPlaneCurves,
RandomGenus14Curves,
RandomCanonicalCurves
*-  
export{"randomSpaceCurve",
     "hartshorneRaoModule",
     "knownUnirationalComponentOfSpaceCurves",
     "hilbertNumerator",
     "expectedBetti",
     "spaceCurve",
     "constructHartshorneRaoModule",
     "certifyHartshorneRaoModule",
     "certifyRandomSpaceCurve",
     --from RandomPlaneCurves
     "distinctPlanePoints",
      "nodalPlaneCurve",
      "completeLinearSystemOnNodalPlaneCurve",
      "imageUnderRationalMap",
     --from RandomGenus14Curves
     "randomCurveGenus14Degree18inP6",
     "randomCurveGenus8Degree14inP6",
     "randomCanonicalCurveGenus8with8Points",
     "curveGenus14Degree18inP6",
     "canonicalCurveGenus14",
     --From RandomCanonicalCurves
      "canonicalCurve",
}


undocumented {
    --And UNEXPORTED
     "constructDistinctPlanePoints",
     "certifyDistinctPlanePoints",
     "constructNodalPlaneCurve",
     "certifyNodalPlaneCurve",
     "randomCanonicalModelOfPlaneCurve",
     "randomCanonicalModelOfSpaceCurve",
     "randomCanonicalCurve",
     "certifyCanonicalCurve"
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
     -- the beginning of the Hilbert series
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
-- Expected Betti Tableaux --
-----------------------------

-- convert c*t^d to (c,({d},d))
-- assumes only one term c*t^d
-- ring of t must be over ZZ or QQ
-- and singly graded
--
-- this function is needed to construct
-- expected betti tables from
-- a HilbertNumerator
termToBettiKey = (mon) -> (
     -- the coefficient of the monomial
     c := lift((last coefficients mon)_0_0,ZZ);
     -- the degree of the monomial
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
-- from a Hilbert Numerator
--
-- For this every term a_i*t^i will represent a summand R^{abs(a_i):-i}
-- in the ChainComplex represented by the desired BettiTableau
-- The step where this summand is used depends on the number of
-- sign switches that occur in the Hilbert numerator before this monomial
--
-- the ring of the Hilbert numerator is expected to singly graded
-- and contain only one variable
expectedBetti(RingElement):= (hilbNum) ->(
     -- find terms of Hilbert Numerator
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

Ring ^ BettiTally := Complex => (R,b) -> (
    -- direct sum of complexes
    if #keys b === 0 then return complex R^0;
    directSum for k in keys b list (
	  (i,d,h) := k;
      complex(R^{b#k:-d}, Base => i)
      )
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
-- generic module may not have expected syzygies
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
     -- calculate support of Hartshorne Rao Module
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



-- returns the next prime number of
-- a given number of ANY type
-- (for complex numbers c this is next
-- prime number of ceiling(Re(c))


-- construction of general points in the plane
-- via their Hilbert-Burch matrix that occurs
-- in the free resolution of their vanishing ideal
-- 0 <-- R[Points] <-- R <-- F <--B-- G <-- 0
-- with free modules F and G

constructDistinctPlanePoints=method(TypicalValue=>Ideal,Options=>{Certify=>false})
  -- Certify is only dummy option here
constructDistinctPlanePoints(ZZ,PolynomialRing):=opt->(k,R)->(
     -- catch wrong inputs:
     if dim R != 3 then error "expected a polynomial ring in three variables";
     if degrees R !={{1}, {1}, {1}} then error "polynomial ring is not standard graded";
     if k<0 then error "expected a non negative degree";
     n := ceiling((-3+sqrt(9.0+8*k))/2);
     eps := k-binomial(n+1,2);
     -- choose a random Hilbert-Burch matrix
     B := random(R^{n+1-eps:0,2*eps-n:-1},R^{n-2*eps:-1,eps:-2});
     minors(rank source B,B))

-- the certification tests that the
-- scheme of the points is smooth, i.e. that
-- there are no infinitesimally close points

certifyDistinctPlanePoints=method(TypicalValue=>Boolean)
certifyDistinctPlanePoints(Ideal,ZZ,PolynomialRing):= (I,k,R)->
   dim I==1 and dim (I+minors(2,jacobian I))<=0

distinctPlanePoints=new RandomObject from {
     Construction  => constructDistinctPlanePoints,
     Certification => certifyDistinctPlanePoints
     }

-- construction of a general point in the linearsystem
-- L(d;2p_1,..,2p_delta) of plane curves of degree d
-- having double points in p_1, ... , p_delta

constructNodalPlaneCurve=method(TypicalValue=>Ideal,Options=>{Certify=>false})
constructNodalPlaneCurve(ZZ,ZZ,PolynomialRing):=opt->(d,delta,R)->(
     -- catch wrong inputs:
     if dim R != 3 then error "expected a polynomial ring in three variables";
     if degrees R !={{1}, {1}, {1}} then error "polynomial ring is not standard graded";
     if d<0 then error "expected a non negative degree";

     -- choose delta distinct random plane points.
     -- The Certify option is passed from top level
     Ipts:=(random distinctPlanePoints)(delta,R,Certify=>opt.Certify,Attempts=>1);
     -- return null if the construction of points did not work
     if Ipts===null then return null;

     -- choose (if possible) a curve of deg d with double points in the given points
     I2:=gens saturate(Ipts^2);
     -- if there is no form of desired degree then return null
     if all(degrees source I2,c->c_0 > d) then return null;
     -- if not, find a nonzero form
     ideal(I2*random(source I2,R^{-d})))

-- the certification checks that the curve is
-- nodal of degree d with delta singular points

certifyNodalPlaneCurve=method(TypicalValue=>Boolean)
certifyNodalPlaneCurve(Ideal,ZZ,ZZ,PolynomialRing):=(F,d,delta,R)->(
     -- compute the singular locus of F
     singF:=F+ideal jacobian F;
     degree F == d and degree singF == delta and dim singF <= 1)

nodalPlaneCurve = new RandomObject from {
     Construction  => constructNodalPlaneCurve,
     Certification => certifyNodalPlaneCurve
     }

completeLinearSystemOnNodalPlaneCurve=method()
completeLinearSystemOnNodalPlaneCurve(Ideal,List):=(J,D)->(
     singJ:=saturate(ideal jacobian J+J);
        -- adjoint ideal
     H:=ideal (mingens ideal(gens intersect(singJ,D_0)%J))_(0,0);
        -- a curve passing through singJ and D_0
     E0:=((J+H):D_0):(singJ^2); -- residual divisor
     if not(degree J *degree H - degree D_0 -2*degree singJ==degree E0)
        then error"residual divisor of has wrong degree";
     L1:=mingens ideal (gens truncate(degree H, intersect(E0,D_1,singJ)))%J;
     h0D:=(tally degrees source L1)_{degree H}; -- h^0 O(D)
     L:=L1_{0..h0D-1}; -- matrix of homogeneous forms, L/H =L(D) subset K(C)
     (L,(gens H)_(0,0)))


imageUnderRationalMap=method()
imageUnderRationalMap(Ideal,Matrix):=(J,L)->(
     if not same degrees source L then error "expected homogeneous forms of a single degree";
     kk:=coefficientRing ring J;
     x := getSymbol "x";
     S:=kk(monoid [x_0..x_(rank source L-1)]);
     RJ:=ring J/J;
     ideal mingens ker map(RJ,S,sub(L,RJ))
     )

randomCanonicalCurveGenus8with8Points = method()

randomCanonicalCurveGenus8with8Points PolynomialRing := R ->(
     --Input: R a polynomial ring in 8 variables,
     --Output: a pair of an ideal of a canonical curve C
     --        together with a list of ideals of 8 points
     --Method: Mukai's structure theorem on genus 8 curves.
     --  Note that the curves are have general Clifford index.
     FF:=coefficientRing R;
     p:=symbol p;
     -- coordinate ring of the Plücker space:
     P:=FF[flatten apply(6,j->apply(j,i->p_(i,j)))];
     skewMatrix:=matrix table(6,6,
	  (i,j) -> (
	       if i<j then p_(i,j)
	       else if i>j then -p_(j,i)
	       else 0_P));
     -- ideal of the Grassmannian G(2,6):
     IGrass:=pfaffians(4,skewMatrix);
     points:=apply(8,k->exteriorPower(2,random(P^2,P^6)));
     ideals:=apply(points,pt->ideal( vars P*(syz pt**P^{-1})));
     -- linear span of the points:
     L1 := intersect ideals;
     if degree L1 != 8 then return (null,null);
     L:= super basis(1,L1);
     if dim ideal L != 8 then return (null,null);
     phi:=vars P%L; -- coordinates as function on the span
     -- actually the last 8 coordinates represent a basis
     phi2:= matrix{toList(7:0_R)}|vars R;
     -- matrix for map from R to P/IC
     IC:=ideal (gens IGrass%L); --the ideal of C on the span
     -- obtained as the reduction of the Grassmann equation mod L
     IC2:=ideal mingens substitute(IC,phi2);
     idealsOfPts:=apply(ideals,Ipt->
         ideal mingens ideal sub(gens Ipt%L,phi2));
     (IC2,idealsOfPts))

randomCurveGenus8Degree14inP6=method(TypicalValue=>Ideal)

randomCurveGenus8Degree14inP6 PolynomialRing :=  S -> (
     -- Input:  S coordinate ring of P^6
     -- Output: ideal of a curve in P^6
     x:=symbol x;
     FF:=coefficientRing S;
     R:=FF[x_0..x_7];
     (I,points):=randomCanonicalCurveGenus8with8Points(R);
     if I === null then return null;
     D1:=intersect apply(4,i->points_i); -- divisors of degree 4
     D2:=intersect apply(4,i->points_(4+i));
     -- compute the complete linear system |K+D1-D2|, note K=H1
     H1:=gens D1*random(source gens D1,R^{-1});
     E1:=(I+ideal H1):D1; -- the residual divisor
     L:=mingens ideal(gens intersect(E1,D2)%I);
     if source L != R^{7:-2} then return null;
     -- the complete linear system
     -- note: all generatore of the intersection have degree 2.
     RI:=R/I; -- coordinate ring of C' in P^7
     phi:=map(RI,S,substitute(L,RI));
     ideal mingens ker phi)

randomCurveGenus14Degree18inP6=method(TypicalValue=>Ideal,Options => {Certify => false})

randomCurveGenus14Degree18inP6 PolynomialRing :=  opt -> S-> (
     -- Input: S PolynomialRing in 7 variables
     -- Output: ideal of a curve of genus 14
     -- Method: Verra's proof of the unirationality of M_14
     IC':=randomCurveGenus8Degree14inP6(S);
     if IC'===null then return null;
     -- Choose a complete intersection:
     CI:=ideal (gens IC'*random(source gens IC',S^{5:-2}));
     IC:=CI:IC'; -- the desired residual curve
     return IC
     )

certifyCurveGenus14Degree18inP6 = method(TypicalValue => Boolean)

certifyCurveGenus14Degree18inP6 (Ideal,PolynomialRing) := (IC,S) -> (
     -- check degree, genus and codimension first
     if not (degree IC ==18 and codim IC == 5 and genus IC ==14)
        then return false;
     -- look at the quadrics first
     -- (they define a complete intersection by construction)
     CI := ideal select(flatten entries mingens IC,i->degree i == {2});
     someMinors :=minors(5, jacobian CI);
     singCI:=CI+someMinors;
     if not (degree singCI==28 and codim singCI==6)
        then return false;
     someMoreMinors:=minors(5, jacobian (gens IC)_{0..3,5});
     singC:=singCI+someMoreMinors;
     return (codim singC == 7)
     )


--- interface for (random curveGenus14Degree18inP6)
curveGenus14Degree18inP6 = new RandomObject from {
     Construction => randomCurveGenus14Degree18inP6,
     Certification => certifyCurveGenus14Degree18inP6
     }

---------------------------
--- canonical embedding ---
---------------------------

randomCanonicalCurveGenus14 = method(TypicalValue => Ideal,Options => {Certify => false})

-- S : a polynomial Ring with 14 variables
randomCanonicalCurveGenus14 (PolynomialRing) := opt -> (R) -> (
     	  y := local y;
     	  S := coefficientRing(R)[y_0..y_6];
	  RS := R**S;
     	  I := (random curveGenus14Degree18inP6)(S,Certify=>opt.Certify,Attempts=>1);
     	  fI:=res I;
	  omegaC:=presentation truncate(0,((coker transpose fI.dd_5)**S^{-7}));
     	  graph:=substitute(vars R,RS)*substitute(omegaC,RS);
	  J:=saturate(ideal graph,substitute(y_0,RS));
	  -- does this saturation always work???
          I=ideal mingens substitute(J,R);
     	  --genus I==g and degree I == 2*g-2
	  return I)

certifyCanonicalCurveGenus14 = method(TypicalValue => Boolean)

-- the canonical curve does not need to be certified,
-- since in the construction the smoothness gets already
-- certified by (random curveGenus14Degree18inP6).
certifyCanonicalCurveGenus14 (Ideal,PolynomialRing) := (I,R) -> true

--- interface for (random canonicalCurveGenus14)
canonicalCurveGenus14 = new RandomObject from {
     Construction => randomCanonicalCurveGenus14,
     Certification => certifyCanonicalCurveGenus14
     }
randomCanonicalModelOfPlaneCurve = method(Options => {Certify => false})

-- input:
--    d degree of plane nodal curve
--    g geometric genus of plane nodal curve
--    R ring with g variables
-- output:
--    I Ideal of R describing a canonical model
randomCanonicalModelOfPlaneCurve (ZZ,ZZ,Ring) := opt -> (d,g,R) -> (
     x -> (
	  S := (coefficientRing R)[x_0..x_2];
	  delta:=binomial(d-1,2)-g;
	  J:=(random nodalPlaneCurve)(d,delta,S,Certify=>opt.Certify,Attempts=>1);
	  -- the canonical linear system (assuming that all singularities are nodes)
	  KC:=(gens intersect(saturate(ideal jacobian J +J),(ideal vars S)^(d-3)))_{0..(g-1)};
	  SJ:=S/J;
	  phi:=map(SJ,R,substitute(KC,SJ));
	  I:=ideal mingens ker phi;
	  return I)
     ) (x := local x) -- this construction prevents a memory allocation cycle involving local frames for the interpreter

randomCanonicalModelOfSpaceCurve = method(Options => {Certify => false})

-- input:
--    d degree of space curve
--    g geometric genus of space curve
--    R ring with g variables
-- output:
--    I Ideal of R describing a canonical model
randomCanonicalModelOfSpaceCurve (ZZ,ZZ,Ring) := opt -> (d,g,R) -> (
     y := local y;
     S := (coefficientRing R)[y_0..y_3];
     RS := R**S;
     I := (random spaceCurve)(d,g,S,Certify=>opt.Certify,Attempts=>1);
     -- the canonical linear system
     omegaC := presentation prune truncate(0,Ext^1(I,S^{ -4}));
     graph := substitute(vars R,RS)*substitute(omegaC,RS);
     J := saturate(ideal graph,substitute(y_0,RS));
     Icanonical := ideal mingens substitute(J,R);
     return Icanonical);


randomCanonicalCurve=method(TypicalValue=>Ideal,Options=>{Certify=>false})

-- construct a random canonical curve of genus g
-- by using plane curves, space curves and verras construction for g=14
-- R a ring with g variables
randomCanonicalCurve(ZZ,PolynomialRing):= opt -> (g,R)->(
     if g>14 or g<4 then error "no method implemented";
     d := null;
     if g<=10 then (
	  s:=floor(g/3); -- the speciality of a plane model of minimal degree
	  d=g+2-s; -- the degree of the plane model
	  return randomCanonicalModelOfPlaneCurve(d,g,R,Certify=>opt.Certify));
     -- the following space curve models are chosen such that the
     -- Brill-Noether number is positive and the construction via
     -- Hartshorne-Rao-Modules works
     if g==11 then return randomCanonicalModelOfSpaceCurve(12,11,R,Certify=>opt.Certify);
     if g==12 then return randomCanonicalModelOfSpaceCurve(12,12,R,Certify=>opt.Certify);
     if g==13 then return randomCanonicalModelOfSpaceCurve(13,13,R,Certify=>opt.Certify);
     -- Verra's construction for g=14
     if g==14 then return (random canonicalCurveGenus14)(R,Certify=>opt.Certify,Attempts=>1);
     )




-- the canonical curve does not need to be certified,
-- since in the construction the smoothness gets already
-- certified by (randomPlaneCurve and randomSpaceCurve).
certifyCanonicalCurve = method(TypicalValue => Boolean)
certifyCanonicalCurve (Ideal,PolynomialRing) := (I,R) -> true

--- interface for (random canonicalCurveGenus14)
canonicalCurve = new RandomObject from {
     Construction => randomCanonicalCurve,
     Certification => certifyCanonicalCurve
     }
-- calculate the expected betti tableau
-- from a given Hilbert function.
-- hilb = {h0,...,h_(d+r+1)}
-- where d is the regularity of the variety described
-- and r is the dimension of the ambient space
expectedBetti(List,ZZ) := (L,r)->(
     t := local t;
     T := QQ[t];
     expectedBetti(hilbertNumerator(L,r,t))
     )
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

beginDocumentation()

doc ///
 Key
   RandomCurves
 Headline
   constructing random curves in various ways
 Description
   Text
     A RandomObject in this sense is a hashTable consisting of two functions, one that can construct a random point
     and the other that can certify that the thing constructed has the desired property, say being a smooth space
     curve of the desired genus and degree. This is employed through a call such as random spaceCurve, which
     returns the constructor function;
     thus to get a random curve of degree d and genus g one must do something like

     (random spaceCurve)(d,g,R)

     where R is the homogeneous coordinate ring of P^3.

     For a different approach, see the package SpaceCurves; there  special curves in P^3 of every  genus and degree that is
     allowed by Castelnuovo's theorem are constructed on surfaces of degree <=4, following the Theorem of Gruson and Peskine.
    
     This package provides the construction of random curves $C \subset \mathbb{P}^{ 3}$ for various values for its degree $d$ and genus $g$.
     A space curve $C \subset \mathbb{P}^{ 3}$ is constructed via its Hartshorne-Rao module $M= H^1_*(\mathcal{I}_C(n))$.
     In particular, there are constructions for random points in $M_g$ for $g=11,12,13$.

     For a algorithms and theoretical background see
     @ HREF("https://macaulay2.com/Book/", "Needles in a Haystack") @
   
     This package also generates random nodal plane curves and provides related
     methods.

     Also
     In this package the unirationality construction of the moduli space $M_{14}$ of curves of genus 14 due to Verra is implemented.
     The main references are

     \ \ \ \ \ [Mu] S. Mukai, Curves, $K3$ surfaces and Fano $3$-folds of genus $\leq 10$. Algebraic geometry and commutative algebra, Vol. I, 357-377, Kinokuniya, Tokyo, 1988.

     \ \ \ \ \ [Ve] A. Verra, The unirationality of the moduli spaces of curves of genus 14 or lower. Compos. Math. 141 (2005), no. 6, 1425-1444.

    Also
    This package bundles the constructions for random points in the moduli spaces of curves $M_g$ for $g \leq 14$ based on
    the proofs of unirationality of $M_g$ by Severi, Sernesi, Chang-Ran and Verra.

    Further, it provides for random canonical
    curves made from nodal plane curves.

    For random smooth curves defined over very
    small fields, see the package
    RandomCurvesOverVerySmallFiniteFields
///
doc ///
  Key
    "RandomSpaceCurves"
  Headline
    Construction of random space curves of various kinds.
  Description
    Text

  ///


doc ///
  Key
    "spaceCurve"
  Headline
    constructs a RandomObject that can be used to construct a space curve.
  Usage
    (random spaceCurve)(d,g,R)
  Inputs
    d:ZZ
        the desired degree
    g:ZZ
        the desired genus, 
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
      calculates the expected betti table  from a given Hilbert Numerator.

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
      values of the Hilbert function
  r: ZZ
       dimension of ambient protective space
 Outputs
  B: BettiTally
       expected Betti table of module with Hilbert function h
 Description
  Example
    betti expectedBetti({0,0,4,6,3,0,0,0,0},3)
 Caveat
  The Hilbert function has to be given at positions {\tt 0} to {\tt d+r+1} where {\tt d} is the regularity of the considered variety
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
   	values of the Hilbert function
   r: ZZ
       dimension of ambient projective space
   t: RingElement
       variable in which the hilbertNumerator is given
 Description
  Example
    T=QQ[t];
    hilbertNumerator({0,0,4,6,3,0,0,0,0},3,t)
 Caveat
  The Hilbert function has to be given at positions {\tt 0} to {\tt d+r+1} where {\tt d} is the regularity of the considered variety

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
    distinctPlanePoints
  Headline
    Generates the ideal of k random points in the coordinate ring $R$ of $\\P^{ 2}$
  Usage
    (random distinctPlanePoints)(k,R)
  Inputs
    k : ZZ
          the number of points
    R : PolynomialRing
          the homogeneous coordinate ring of $\mathbb{P}^2$
  Outputs
     : Ideal
          the vanishing ideal of the points
  Description
    Text
       Creates the ideal of the points via a random choice of their
       Hilbert-Burch matrix, which is taken to be of generic shape.
    Example
       setRandomSeed("alpha");
       R=ZZ/32003[x_0..x_2];
       Ipts=(random distinctPlanePoints)(10,R);
       betti res Ipts
///

doc ///
 Key
   nodalPlaneCurve
 Headline
   get a random nodal plane curve
 Usage
   (random nodalPlaneCurve)(d,delta,R)
 Inputs
   d : ZZ
         the degree of the curve
   delta : ZZ
         the number of nodes
   R : PolynomialRing
         homogeneous coordinate ring of $\mathbb{P}^2$.
 Outputs
     : Ideal
         the vanishing ideal of the curve
 Description
   Text
      The procedure starts by choosing

      \ \ \  1) an ideal I of delta random points in $\PP^2$, and then returns

      \ \ \  2) the principal ideal generated by an random element in the saturated
                square J=saturate(I^2) of degree d.

      If the procedure fails, for example if J_d=0, then the {\tt null} is returned.

      Under the option {\tt Certified=>true}, the result is certified by establishing
      that

      \ \ \  1) the points are distinct nodes, and that

      \ \ \  2) the curve has ordinary nodes at these points

      by using the Jacobian criterion applied to the singular locus of the curve.

      Under the option {\tt Attempts=>n}, the program makes {\tt n} attempts in both
      steps to achieve the desired goal.
      Here {\tt n} can be infinity. The default value is {\tt n=1}.

   Example
      setRandomSeed("alpha");
      R=ZZ/32003[x_0..x_2];
      F=(random nodalPlaneCurve)(8,5,R);
      (dim F, degree F)
      singF = F + ideal jacobian F;
      (dim singF,degree singF)

   Text
    Over very small fields the curves are often singular:

   Example
      R=ZZ/3[x_0..x_2];
      tally apply(3^4,i-> null===((random nodalPlaneCurve)(8,5,R,Certify=>true, Attempts=>1)))
///

doc ///
  Key
    completeLinearSystemOnNodalPlaneCurve
    (completeLinearSystemOnNodalPlaneCurve,Ideal,List)
  Headline
    Compute the complete linear system of a divisor on a nodal plane curve
  Usage
    (L,h)=completeLinearSystemOnNodalPlaneCurve(I,D)
  Inputs
    I:Ideal
        of a nodal plane curve C,
    D: List
        \{D_0,D_1\}\ of ideals representing effective divisors on C
  Outputs
    L:Matrix
      of homogeneous forms with 1 row and with number of columns equal to $h^0(D_0-D_1)$
    h:RingElement
      such that L_{(0,i)}/h represents a basis of $H^0 O(D_0-D_1)$
  Description
   Text
     Compute the complete linear series of D_0-D_1 on the normalization of C
     via adjoint curves and double linkage.
   Example
     setRandomSeed("alpha");
     R=ZZ/32003[x_0..x_2];
     J=(random nodalPlaneCurve)(6,3,R);
     D={J+ideal random(R^1,R^{1:-3}),J+ideal 1_R};
     l=completeLinearSystemOnNodalPlaneCurve(J,D)
     C=imageUnderRationalMap(J,l_0);
     (dim C, degree C, genus C)
  SeeAlso
     nodalPlaneCurve
     imageUnderRationalMap
///

doc ///
  Key
    imageUnderRationalMap
    (imageUnderRationalMap,Ideal,Matrix)
  Headline
    Compute the image of the scheme under a rational map
  Usage
    I = imageUnderRationalMap(J,L)
  Inputs
    J: Ideal
       in a polynomial ring
    L: Matrix
       of homogeneous polynomials of equal degrees
  Outputs
    I: Ideal
       of the image of the scheme defined by J under the rational map defined by L
  Description
     Example
       setRandomSeed("alpha");
       p=nextPrime 10000
       kk=ZZ/p
       R=kk[t_0,t_1]
       I=ideal 0_R
       L=matrix{{t_0^4,t_0^3*t_1,t_0*t_1^3,t_1^4}}
       J=imageUnderRationalMap(I,L)
       betti J
///




doc ///
  Key
    canonicalCurveGenus14
  Headline
    compute a random curve of genus 14 in its canonical embedding
  Usage
   (random canonicalCurveGenus14)(R)
  Inputs
    R:PolynomialRing
       coordinate ring of $\mathbb{P}^13$
  Outputs
    :Ideal
      in R, ideal of the canonical curve
  Description
   Example
     setRandomSeed("alpha");
     R=ZZ/101[x_0..x_13];
     C=(random canonicalCurveGenus14)(R);
     (dim C, degree C, genus C)
///


doc ///
  Key
    curveGenus14Degree18inP6
  Headline
    compute a random curve of genus 14 and degree 18 in $\mathbb{P}^6$
  Usage
   (random curveGenus14Degree18inP6)(R)
  Inputs
   R:PolynomialRing
      coordinate ring of $\PP^6$
  Outputs
    :Ideal
       in R, ideal of the curve
  Description
   Example
     setRandomSeed("alpha");
     R=ZZ/101[x_0..x_6];
     C=(random curveGenus14Degree18inP6)(R);
     (dim C, degree C, genus C)
///

doc ///
  Key
    randomCanonicalCurveGenus8with8Points
    (randomCanonicalCurveGenus8with8Points,PolynomialRing)
  Headline
    Compute a random canonical curve of genus 8 with 8 marked point
  Usage
    (I,idealsOfPts)=randomCanonicalCurveGenus8with8Points S
  Inputs
    S: PolynomialRing
       homogeneous coordinate ring of $\PP^7$
  Outputs
    I: Ideal
       a canonical curve C of genus 8
    idealsOfPts: List
       8 ideals of K-rational points on C
  Description
    Text
      According to Mukai [Mu] any smooth curve of genus 8 and Clifford index 3
      is the transversal intersection $C=\PP^7 \cap\ G(2,6) \subset \ \PP^{15}$.
      In particular this is true for the general curve of genus 8.
      Picking 8 points in the Grassmannian $G(2,6)$ at random and \PP^7 as their span
      gives the result.

    Example
      setRandomSeed("alpha");
      FF=ZZ/10007;
      S=FF[x_0..x_7];
      (I,points)=randomCanonicalCurveGenus8with8Points S;
      betti res I
      points
///

doc ///
  Key
    randomCurveGenus8Degree14inP6
    (randomCurveGenus8Degree14inP6,PolynomialRing)
  Headline
    Compute a random normal curve of genus g=8 and degree 14 in \PP^6
  Usage
    I=randomCurveGenus8Degree14inP6 S
  Inputs
    S: PolynomialRing
       in 7 variables
  Outputs
    I: Ideal
       of a curve of geometric genus 8 and degree 14 in \PP^6
  Description
    Text
      The construction is based on Mukai's unirational description of $M_{8,8}$
      of the moduli space of genus 8 with 8 marked points (see [Mu]).

    Example
      setRandomSeed("alpha");
      FF=ZZ/10007;
      S=FF[x_0..x_6];
      I=randomCurveGenus8Degree14inP6 S;
      betti res I
///


doc ///
  Key
    randomCurveGenus14Degree18inP6
    (randomCurveGenus14Degree18inP6,PolynomialRing)
  Headline
    Compute a random curve of genus 14 of Degree 18 in \PP^6
  Usage
    randomCurveGenus14Degree18inP6 S
  Inputs
    S: PolynomialRing
       homogeneous coordinate ring of \PP^6
  Outputs
    : Ideal
        the ideal of a curve C of genus 14 and degree 18 in \PP^6
  Description
    Text
      According to Verra [Ve], a general genus 14 curve $C$ arizes as the residual
      intersection of the 5 quadrics in the homogeneous ideal of a general
      normal curve $E$ of genus 8 and degree 14 in \PP^6. These in turn can be
      constructed using Mukai's Theorem on genus 8 curves: Every smooth
      genus 8 curve with general Clifford index arizes as the intersection
      of the Grassmannian $G(2,6) \subset \PP^{14}$ with a transversal $\PP^7$.
      Taking $\PP^7$ as the span of general or random $8$ points
      $$p_1,\ldots, p_8 \in{} G(2,6)$$ gives  $E$ together with a general divisor
      $ H=K_E+D_1-D_2$ of degree 14 where $D_1=p_1+\ldots+p_4$ and $D_2=p_5+\ldots+p_8$.

      The fact that the example below works can be seen as computer aided proof of the
      unirationality of $M_{14}$. It proves the unirationality of $M_{14}$ for
      fields of the chosen finite characteristic 10007, for fields of characteristic 0
      by semi-continuity, and, hence, for all but finitely many primes $p$.

    Example
      setRandomSeed("alpha");
      FF=ZZ/10007;
      S=FF[x_0..x_6];
      time I=randomCurveGenus14Degree18inP6(S);
      betti res I
///


doc ///
  Key
    canonicalCurve
  Headline
    Compute a random canonical curve of genus less or equal to 14
  Usage
    I=(random canonicalCurve)(g,S)
  Inputs
    g: ZZ
       the genus
    R: PolynomialRing
       homogeneous coordinate ring of $\PP^{ g-1}$
  Outputs
    I: Ideal
       of a canonical curve $C$ of genus $g$
  Description
    Text
      Compute a random canonical curve of genus $g \le{} 14$, based on the proofs of unirationality of
      $M_g$ by Severi, Sernesi, Chang-Ran and Verra.
    Example
      setRandomSeed "alpha";
      g=14;
      FF=ZZ/10007;
      R=FF[x_0..x_(g-1)];
      time betti(I=(random canonicalCurve)(g,R))
      genus I == g and degree I ==2*g-2
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


-- tests for distinct plane curves
TEST ///
setRandomSeed("alpha");
R=ZZ/32003[x_0..x_2];
Ipts=(random distinctPlanePoints)(10,R,Certify=>true);
assert(Ipts=!=null)
assert(betti res Ipts==new BettiTally from {(0,{0},0) => 1, (1,{4},4) => 5, (2,{5},5) => 4})
///

-- tests for nodalPlaneCurve
TEST ///
setRandomSeed("alpha");
R=ZZ/32003[x_0..x_2];
F=(random nodalPlaneCurve)(8,5,R);
assert(F=!=null)
assert(dim F==2)
assert(degree F==8)
singF=F+ideal jacobian F;
assert(dim singF==1)
assert(degree singF==5)
///

-- tests for image under rational map
TEST ///
R=QQ[y_0,y_1];
I=ideal 0_R;
L=basis(5,R)
C=imageUnderRationalMap(I,L);
assert(dim C == 2 and genus C==0 and degree C == 5)
///

TEST ///
  -- check that there are not to many non-detected problems in the construction.
  -- This code finds errors in codimension 4 with high probability
  -- since 3^4 \approx 100
  --works with about 15% of random seeds
  setRandomSeed("alpha")
  Fq= ZZ/3
  T = Fq[t_0..t_6]
  setRandomSeed 16
  assert(
      (random curveGenus14Degree18inP6)(T, Attempts => 1)
      =!= null)
///

TEST ///
  -- check that the certification sometimes works
  -- (only errors in codim 1 are detected)
  --Works with about 10% of random seeds
  setRandomSeed("alpha")
  Fq= ZZ/11
  T = Fq[t_0..t_6]
  setRandomSeed 4
  assert((random curveGenus14Degree18inP6)(T,Attempts=>1,Certify=>true) =!= null)
    ///

-- check that the number of generators of the constructed
-- canonical curve is as expected
TEST ///
setRandomSeed("alpha");
apply(5..14,g->(
	  assert (binomial(g-2,2) == rank source mingens (I=(random canonicalCurve)(g,(ZZ/101)[x_0..x_(g-1)])))
     ))
///

end--

restart
uninstallPackage("RandomCurves1")
installPackage("RandomCurves1",RerunExamples=>true,RemakeAllDocumentation=>true);

check("RandomCurves1")

viewHelp"RandomCurves1"

matrix apply(toList(2..18),d-> apply(toList(0..26),g->
	  if knownUnirationalComponentOfSpaceCurves(d,g) then 1 else 0))

restart
needsPackage("RandomCurves1")

R = (ZZ/7)[x_0..x_3]
betti res (random spaceCurve)(12,11,R)

time tally apply(10,i->null === (random spaceCurve)(12,11,R,Certify=>true,Attempts=>1))

time tally apply(10,i->time certifyRandomSpaceCurve(randomSpaceCurve(12,11,R),12,11,R))

R = ZZ[]/49
(matrix{{11_R,12_R},{13_R,14_R}})^-1
--
R=ZZ/101[x_0..x_3];

needsPackage "DGAlgebras"
kk = ZZ/101
S = kk[a..d]
monomialCurveIdeal(S,{1,2,3})
monomialCurve = method()
monomialCurve List := L ->(
kk = ZZ/101;
S = kk[x_0..x_(#L)];
S/monomialCurveIdeal(S,L))

R = monomialCurve {2,4,7}
RS = map(R,S)
netList for i from 1 to 10 list (
    L :={1,random(1+10),random(1+10)}
    isGolod (R= monomialCurve L)
    numgens ideal R
    RS := map(R,S)
    Rmod =pushForward(RS, R^1)
   pdim Rmod
))
dim singularLocus R

pushForward(RS,R^1)
