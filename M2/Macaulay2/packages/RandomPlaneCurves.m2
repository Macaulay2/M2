newPackage(
	"RandomPlaneCurves",
    	Version => "0.6",
    	Date => "June 20, 2011",
    	Authors => {
	     {Name => "Hans-Christian Graf v. Bothmer",
	      Email => "bothmer@uni-math.gwdg.de",
	      HomePage => "http://www.crcg.de/wiki/User:Bothmer"},

	     {Name=> "Florian Geiss",
	      Email=> "fg@math.uni-sb.de",
	      HomePage=> "http://www.math.uni-sb.de/ag/schreyer/"},

	     {Name => "Frank-Olaf Schreyer",
	      Email => "schreyer@math.uni-sb.de",
	      HomePage => "http://www.math.uni-sb.de/ag/schreyer/"}
                   },
    	Headline => "random plane curves",
	Keywords => {"Examples and Random Objects"},
	PackageExports => {"RandomObjects"},
	PackageImports => {"Truncations"},
    	DebuggingMode => false
        )

if not version#"VERSION" >= "1.8" then
  error "this package requires Macaulay2 version 1.8 or newer"

export{"distinctPlanePoints",
       "constructDistinctPlanePoints",
       "certifyDistinctPlanePoints",
       "nodalPlaneCurve",
       "constructNodalPlaneCurve",
       "certifyNodalPlaneCurve",
       "completeLinearSystemOnNodalPlaneCurve",
       "imageUnderRationalMap"
       }

undocumented {
     constructNodalPlaneCurve,
     certifyNodalPlaneCurve,
     constructDistinctPlanePoints,
     certifyDistinctPlanePoints}

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
-- there are no infinitisimally close points

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

beginDocumentation()

-- authors: add some text to this documentation node:
doc ///
 Key
   RandomPlaneCurves
///

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


------------- TESTS --------------

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
end

restart
uninstallPackage("RandomPlaneCurves")
installPackage("RandomPlaneCurves",RerunExamples=>true,RemakeAllDocumentation=>true);
check"RandomPlaneCurves"
viewHelp"RandomPlaneCurves"
end



