newPackage(
          "PlaneCurveLinearSeries",
          Version => "1.0",
          Date => "February 4, 2024",
          Headline => "Linear series on the normalization of a plane curve",
          Authors => {{ Name => "David Eisenbud", 
		  Email => "de@berkeley.edu", 
		  HomePage => "https://eisenbud.github.io/"}},
	  PackageExports => {"IntegralClosure","PrimaryDecomposition"},
          AuxiliaryFiles => false,
          DebuggingMode => false,
	  Keywords => {"Projective Algebraic Geometry"}
          )
      export {
	  "canonicalSeries",
	  "geometricGenus",
	  "linearSeries",
	  "projectiveImage",
	  "canonicalImage",
	  "fromCoordinates",
	  "toCoordinates",
	  "addition",
	  "negative",
	  
	  --Options
	  "Conductor",
	  "ShowBase"
	  }
      
toCoordinates = method()
toCoordinates Ideal := List => I -> (
    --I should be a complete intersection of linear
    --forms, defining a point.
    R := ring I;
    D := diff(transpose gens I, vars R);
    (entries transpose syz D)_0)

fromCoordinates = method()
   -- construct ideal of point on a curve in P3 from its coordinates;
   -- or the ideal of a linear series, given a list of coordinate lists

fromCoordinates (List, Ring) := Ideal =>  (P, C) ->(
    if class P_0 =!= List then P' := {P} else P' = P;
    I := product apply(P', L ->(
    PC := sub (matrix {L}, C);
    ideal(vars C * (syz PC))));
    if dim I == 0 then error"point does not lie on curve";
    I)

fromCoordinates (ZZ,ZZ,ZZ, Ring) := Ideal => (x,y,z, R) -> 
    fromCoordinates (toList(x,y,z),R)

geometricGenus = method(Options => {Conductor=>0})
geometricGenus Ring := ZZ => o -> R -> (
    cond := o.Conductor;
    if dim singularLocus R <= 0 then cond = ideal 1_R;
    if cond == 0 then cond = conductor R;
    c := canonicalSeries (R, Conductor => cond);
    if c == 0 then 0 else numcols c)

geometricGenus Ideal := ZZ => o-> I -> geometricGenus((ring I)/I, Conductor => o.Conductor)

linearSeries = method(Options => {Conductor=>null, 
	                          ShowBase => false})
linearSeries (Ideal,Ideal) := Matrix => o-> (D0,Dinf)  ->(
    -- returns a matrix whose elements span the complete linear series 
    --|D0-Dinf|+base points,
    -- where D_0, Dinf \subset R
    -- are the ideals of effective divisors in the ring R = S of an ACM curve C0,
    -- with normalization C, eg a plane curve
    R := ring D0;
    --
    dsing := dim singularLocus R;
    if dsing <= 0 then 
	cond := ideal 1_R else
          if o.Conductor === null then 
	    cond = conductor R else
	         cond = o.Conductor;

    --at this point cond == conductor R
    base := saturate(D0*cond);
--    base := saturate(intersect(D0,cond));    
    F := ideal(base_*_0);
    --Now  F~ D0 + conductor + A
    A := F:base;
    f := degree F_0;
    baseplus := saturate(A*Dinf);
--    baseplus := saturate intersect(A,Dinf);
    ls := gens image basis(f, baseplus);
--error();
    if o.ShowBase == false then ls else (ls, baseplus)
)

linearSeries Ideal := Matrix => o -> D0 ->  (
    Dinf := ideal(1_(ring D0));
    linearSeries(D0, Dinf, o)
    )

linearSeries(List, List, Ring) := Matrix => o -> (D0List, DinfList, C) ->(
    (D0, Dinf) := apply({D0List, DinfList}, D -> fromCoordinates(D,C));
    linearSeries(D0, Dinf))

linearSeries(List, Ring) := Matrix => o -> (D0List, C) ->(
    D0 := fromCoordinates(D0,C);
    linearSeries D0)

///--case of a nodal cubic over a finite field
restart
loadPackage("PlaneCurveLinearSeries", Reload => true)
needsPackage "RandomPoints"
   setRandomSeed 1
   kk = ZZ/7
   S = kk[x,y,z]
   (o,p,sing) = ({1,1,1}, {-1,1,0},{1,0,-1})
   oS = fromCoordinates(o, S)
   pS = fromCoordinates(p, S)
   singS = fromCoordinates(sing, S)

   I = random(3, intersect(oS,pS,singS^2))
   E = S/I
   q' = o
   netList ({q'}|apply(6, i->(
      -- <<(i,q')<<endl;
       q' = addition(o,p,q',E, Conductor => 1_E)
       )))
///



///
--here--
restart
debug loadPackage( "PlaneCurveLinearSeries", Reload => true)
--two characteristic pairs
kk = ZZ/32003
S = kk[a,b,c]; T = kk[s,t];
I = ker map(T,S, {s^7, s^6*t, s^3*t^4+s*t^6+t^7});I
R = S/I
assert (geometricGenus R == 0)
use S
p' = ideal(a-c,b+c)
isSubset(I,p')
radical ideal singularLocus R
--p' is a smooth point of the curve.
p = sub(p', R)
linearSeries p
--p should have 2 elements
L = for i from 0 to 6 list (rank source linearSeries (p^i)) 
assert(L == {1, 2, 3, 4, 5, 6, 7})
--

   kk = ZZ/19
   S = kk[x,y,z]
   setRandomSeed 0
   --
   I = kernel map(kk[s,t], S, {s^3, s^2*t,t^3})
   p = {1,0,0}; 
   o = {1,1,1}; 
   q = o
   C = S/I
   genus C
   geometricGenus C
   pC = fromCoordinates(p,C)
   linearSeries(pC^3) 
   --
   I = ideal"x3+y3+z3"
   p = {1,0,-1}
   q = {1,-1,0}
   o = {0,1,-1}
   C = S/I
   --
   for  i from 0 to 3 do << (q = addition(o,p,q,C))<<endl;
  Text
   so 9p ~ o.   
   --I don't like this!
   more primitively,

  Example
   pC = fromCoordinates(p,C)
   oC = fromCoordinates(o,C)
   qC = oC
   i = 2
   (ls,B) = linearSeries(pC^i,oC^(i-1),ShowBase =>true);
   ls--error-- this linear series has dim 4, should have dim 2
   
   netList   for i from 1 to 10 list(
   (ls,B) = linearSeries(pC^i,oC^(i-1),ShowBase =>true);
   p'C = select(primaryDecomposition ideal ls, J -> J:B != 1)
       )

///


addition = method(Options => {Conductor => null})
addition(Ideal, Ideal,Ideal) := Ideal => opt -> (origin,p,q) ->(
    --Given the ideal of a plane curve of arithmetic genus 1,
    --with assigned origin o,
    --and two (smooth) points p,q, compute their sum.
    --the points are given as codimension 1, linear
    --ideals in the ring of the curve.
    E := ring p;
    if genus E != 1 or numgens E != 3 then
    	error"Needs points on a plane curve of arithmetic genus 1";
    I := ideal E;
    if codim origin != 1 then error"first point not on curve";
    if codim p != 1 then error"second point not on curve";
    if codim q != 1 then error"third point not on curve";    
    
    (ls, B) := linearSeries(p*q,origin, Conductor => opt.Conductor, ShowBase => true);
    (ideal ls):B
    )

addition (List, List, List, Ring) := List => opt -> (o, p , q, E) ->(
    --same but with numerical coordinates
    oE := fromCoordinates(o, E);    
    pE := fromCoordinates(p, E);
    qE := fromCoordinates(q, E);
    toCoordinates addition(oE,pE,qE, Conductor => opt.Conductor)
    )

negative = method(Options => {Conductor=> null})
negative(Ideal, Ideal) := Ideal => opt -> Ideal => (origin, p) -> (
    addition(p, origin, origin, Conductor => opt.Conductor))

negative(List, List, Ring) := List => opt -> (origin, p, E) -> (
    addition(p, origin, origin, E, Conductor => opt.Conductor))


canonicalSeries = method(Options => {Conductor=>null})
canonicalSeries Ring := Matrix => o-> R ->(
    --input: homogeneous coordinate ring of a plane curve
    --output: canonical ideal of the desingularization, as an ideal of R
    cond := o.Conductor;
    if dim singularLocus R <= 0 then cond = ideal 1_R;
    if cond === null then cond = conductor R;

    d := degree R;
    if d-3<0 then ideal 0_R else
        gens image basis(d-3, cond)
    )
canonicalSeries Ideal := Matrix => o-> I-> 
       canonicalSeries((ring I)/I, Conductor => o.Conductor)

projectiveImage = method(Options =>{Conductor => null})
projectiveImage(Ideal, Ideal) := Ring => o -> (D0,Dinfty) ->(
--Produce the ideal of the image under the linear series |D0-Dinfty|
    D := linearSeries(D0, Dinfty, Conductor => o.Conductor);
    R := ring D;
    kk := coefficientRing R;
    s := numcols D;
        X := symbol X;
    SS := kk[X_0..X_(s-1)];
    SS/ker map(R, SS, D)
    )

projectiveImage Ideal := Ring => o -> D0 ->(
    projectiveImage(D0, ideal(1_(ring D0)), 
	              Conductor => o.Conductor))

projectiveImage(List, List, Ring) := Matrix => o -> (D0List, DinfList, C) ->(
    (D0, Dinf) := apply({D0List, DinfList}, D -> fromCoordinates(D,C));
    projectiveImage (D0, Dinf))

projectiveImage(List, Ring) := Matrix => o -> (D0List, C) ->(
    D0 := fromCoordinates(D0,C);
    projectiveImage D0)


projectiveImage Matrix := Ring => o -> M -> (
 -- in this case M is a 1-m matrix representing a
 --linear series.
    R := ring M;
    kk := coefficientRing R;
    s := numcols M;
        X := symbol X;
    SS := kk[X_0..X_(s-1)];
    SS/ker map(R, SS, M)
    )

canonicalImage = method(Options => {Conductor => null})
canonicalImage Ring := Ring => o-> R ->(
    --this version takes the homog coord ring
    -- of a plane curve as input, outputs the
    --homogeneous coordinate ring of the canonical image
    projectiveImage canonicalSeries(R, Conductor => o.Conductor)
)
canonicalImage Ideal := Ring => o -> I -> canonicalImage((ring I)/I)


 -* Documentation section *-

beginDocumentation()

doc ///
Key
 PlaneCurveLinearSeries
Headline
 Linear series on the normalization of a plane curve
Description
  Text
   This package implements procedures described in chapters 4, 5, and 14
   of the book "The Practice of Curves", by David Eisenbud and Joe Harris.
   
   If C is a (possibly singular) irreducible plane curve, it is possible
   to compute the complete linear series of a given divisor on the normalization C' of C
   by computations using data from the plane curve together
   with the conductor ideal $ann_C(C/C)$, which can be computed by Macaulay
   or supplied by the user. 
   
   The main routine of the package is @TO linearSeries@. If D0' and Dinf'
   are effective divisors on C' whose ideals, as schemes, are pulled back
   from ideals D0 and Dinf of C, then 
   
   ell = linearSeries(D0,Dinf)
   
   returns a one-row matrix ell whose entries span a linear series with
   fixed point locus B on C' (including the conductor scheme) and form a basis
   of |D0'-Dinf'|+B.
   
   The routine @TO projectiveImage@ provides the image of the map to projective space given
   by |D0-Dinf|.  There are special routines for the most important case,
   @TO canonicalSeries@ and @TO canonicalImage@.
   
   The functions @TO addition@ and @TO negative@ implement the group law in the case
   of a curve of genus 1.
///

doc ///
Key
 addition
 (addition, List, List, List, Ring)
 (addition, Ideal, Ideal, Ideal)
 [addition, Conductor]
Headline
 addition of smooth points on a curve of genus 1
Usage
 L =  addition(o,p,q,C)
 I = addition(oC, pC, qC)
Inputs
 o: List
 oC: Ideal
 p: List
 pC: Ideal
 q: List
 qC: Ideal
 C: Ring
Outputs
 L: List
 I: Ideal
Description
  Text
   If E is an irreducible plane curve of degree 3 then the set of smooth
   points of C is a principal homogeneous space under the group Pic_0 E
   of invertible sheaves of degree 0. Thus if we choose a smooth
   point o the map p -> O_E(p-o) identifies the set of smooth points with
   such invertible sheaves. 
   
   This script computes the sum of smooth points p,q with respect to the
   group law in which o is the zero point, where the group operation
   makes p+q = r if, as divisors, r is linearly equivalent to p+q-o.
   The functions @TO addition@ and @TO negative@, based on  @TO linearSeries@, 
   allow us to implement the group law.

   The points o,p,q may be represented either by their homogeneous coordinates
   or by ideal in the ring E. The functions @TO fromCoordinates@
   and @TO toCoordinates@ pass between these two representations.
   
   Here is an example with a smooth plane cubic:
  Example
   kk = QQ
   S = kk[x,y,z]
   p = {0,1,0}; pS = fromCoordinates(p,S)
   q = {1,0,0}; qS = fromCoordinates(q,S)
   o = {1,1,1}; oS = fromCoordinates(o,S)

   I = ideal random(3, intersect(oS, pS, qS))
   E = S/I   

   r = addition(o,p,q, E)
   addition(o, negative(o, p, E), r, E)
  Text
   It is known that when one takes multiples of a point that is not torsion,
   the "height" - roughly the number of digits in the coordinates - is doubled
   with each iteration, that is, the number of digits doubles:
  Example
   q' := o;
   netList ({q}| for i from 0 to 3 list(
   q' = addition(o,p,q',E)))
   
  Text
   On the other hand, over a finite field, a curve has only finitely many
   points, so any subgroup of the Jacobian is finite:
   
  Example
   kk = ZZ/7
   S = kk[x,y,z]
   p = {0,1,0}; pS = fromCoordinates(p,S)
   o = {1,1,1}; oS = fromCoordinates(o,S)
   setRandomSeed 0
   I = ideal random(3, intersect(pS,oS))
   E = S/I   
   geometricGenus E
   q = o
   netList ({o} | apply(5, i-> q = addition(o,p,q,E)))
  Text
   A cubic with a node or cusp is also arithmetic genus 1; in the case
   of a node, the smooth points are in correspondence with P^1 minus {0, infinity}
   and the Jacobian is the multiplicative group of the field.
   To allow the program to consider this as a curve of arithmetic genus 1, 
   use the optional argument @TO Conductor@. 
  Example
     setRandomSeed 1
   kk = ZZ/7
   S = kk[x,y,z]
   (o,p,sing) = ({1,1,1}, {-1,1,0},{1,0,-1})
   oS = fromCoordinates(o, S)
   pS = fromCoordinates(p, S)
   singS = fromCoordinates(sing, S)

   I = random(3, intersect(oS,pS,singS^2))
   E = S/I
   q' = o
   netList ({q'}|apply(6, i->(
      -- <<(i,q')<<endl;
       q' = addition(o,p,q',E, Conductor => 1_E)
       )))
  Text
   In the case of rational curve with a cusp, the smooth points
   correspond to the additive group of the field
  Example
   I = kernel map(kk[s,t], S, {s^3, s^2*t,t^3})
   C = S/I
   genus C
   geometricGenus C
   geometricGenus (C, Conductor => ideal(1_C))
  Text
   the singular point is the image {0,0,1} of the point (0,1) in P^1,
   so we may take the origin to be the image {1,1,1} of (1,1) and take
   another smooth point p to be the image {1,0,0} of (1,0).
  Example
   setRandomSeed 0
   p = {1,0,0}; 
   o = {1,1,1}; 
   q = o
   netList ({o}|apply(7, i-> q = addition(o,p,q,C, Conductor => ideal 1_C)))
  Text
   so 7p ~ o.   
References
 "The Practice of Algebraic Curves" Ch. 4, by David Eisenbud and Joe Harris,
 American Mathematical Society
Caveat
  Some aspects of the program
  use random arguments, so one should be wary of computation over very
  small fields 
SeeAlso
 linearSeries
 geometricGenus
///
doc ///
Key
 negative
 (negative, List, List, Ring)
 (negative, Ideal, Ideal)
 [negative, Conductor]
Headline
 implements the inverse in the group law of a curve of genus 1
Usage
 q = negative(o,p,E)
 qE = negative(oE,pE)
Inputs
 o: List
 oE: Ideal
 p: List
 pE: Ideal
 E: Ring
Outputs
 q: List
 qE: Ideal
Description
  Text
   Implements the additive inverse in the group law on the smooth points of
   a plane curve E of genus 1, represented by its homogeneous coordinate ring,
   with chosen zero point o.
  Example
   S = QQ[x,y,z]
   E = S/ideal"x3+y3+z3"
   o = {1,-1,0}
   p = {0,1,-1}
   negative(o,p,E)
   q = addition(o,p,p,E)
   r = addition(o,q, negative(o,p,E), E)
   r == p
SeeAlso
 addition
///

doc///
Key
 fromCoordinates
 (fromCoordinates, List, Ring)
 (fromCoordinates, ZZ, ZZ, ZZ, Ring)
Headline
 Compute the ideal of a point from its coordinates
Usage
 I = fromCoordinates(L,C)
 I = fromCoordinates(x,y,z, C)
Inputs
 L: List
  of three integers or field elements OR a list of lists of that type
 x: RingElement
 y: RingElement
 z: RingElement
  integer or field coordinates of a point on C (assumed to be a plane curve)
 C: Ring
  the ring in which the ideal of the point will be created
Outputs
 I: Ideal 
  of C, defining the subscheme corresponding to the list L of points.
Description
  Text 
   Convenient way to compute the ideal of a point on a plane curve,
   when the point is given by a list of its coordinates.
   If the coordinates are given as integers, they are
   interpreted as elements of the coefficient field
   The script returns an error if the point is not on the curve.
  Example
   S = ZZ/101[a,b,c]
   C = S/ideal"a3+b3-c3"
   P = {0,1,1}
   Q = {1,1,0}
   fromCoordinates(P,C)
   fromCoordinates({P,P},C)
   -- fromCoordinates(Q,C) gives the error "point does not lie on curve"
SeeAlso
 toCoordinates
///


doc ///
Key
 toCoordinates
 (toCoordinates, Ideal)
Headline
 coordinates of a point from its ideal
Usage
 L  = toCoordinates I
Inputs
 I: Ideal
  defining a point
Outputs
 L: List
  of elements of the ground field
Description
  Text
   This is the inverse of @TO fromCoordinates@
  Example
   S = ZZ/5[x,y,z]
   I = ideal(x- y, z)
   L = toCoordinates I
   I == fromCoordinates(L,S)
   
   S = GF(5,2,Variable => a)[x,y,z]/(ideal "x3-y3+z3")
   a^24 == 1
   (a^8)^3
   (a^16)^3
   p = {a^8, a^16, 0}
   fromCoordinates (p,S)
   L = toCoordinates fromCoordinates ({a^8,1,0}, S)
SeeAlso
 fromCoordinates
///

doc ///
Key
 canonicalSeries
 (canonicalSeries, Ring)
 (canonicalSeries, Ideal)
 [canonicalSeries, Conductor]
Headline
 Canonical series of the normalization of a plane curve
Usage
 M = canonicalSeries R
Inputs
 R: Ring
  ring of a plane curve
Outputs
 M: Matrix
  a 1 x g matrix representing the canonical series
Description
  Text
   Computing the canonical linear series
  Example
   kk = QQ
   S = kk[x,y,z]
   C1 = ideal (y^3 - x^2*(x-z)) -- cubic with a node; geometric genus 0
   C2 = ideal(x^2+y^2+z^2) --nonsingular conic
   C3 = ideal (x^4+y^4+z^4) -- smooth curve of genus 3
   canonicalSeries(S/C1)
   canonicalSeries(S/C2)
   canonicalSeries(S/C3)
///

doc ///
Key
 geometricGenus
 (geometricGenus, Ring)
 (geometricGenus, Ideal)
 [geometricGenus, Conductor]
Headline
 Geometric genus of a (singular) plane curve
Usage
 g = geometricGenus R
 g = geometricGenus I
Inputs
 R: Ring
  homogeneous coordinate ring of a plane curve
 I: Ideal
  defining a plane curve
 Conductor: Ideal
  the conductor of R (if not given, it's computed)
Outputs
 g:ZZ
Description
 Text
   The geometric genus of a plane curve C0 is the genus of the normalization of C0
 Example
   kk = QQ
   S = kk[x,y,z]
   C1 = ideal (y^3 - x^2*(x-z)) -- cubic with a node; geometric genus 0
   C2 = ideal(x^2+y^2+z^2)
   C3 = ideal (x^4+y^4+z^4)
   geometricGenus C1
   geometricGenus C2
   geometricGenus C3
 Text
   Every hyperelliptic curve of genus
   g can be represented as a plane curve of degree
   g+2 with a g-fold ordinary singularity, and thus
   conductor equal to the (g-1)st power of the maximal
   ideal. As of 1/20/2024, Macaulay2 crashes on computing
   the conductor when g >= 6, but knowing the 
   conductor one can go much farther:
  
   We make a general hyperelliptic curve of genus
   g with singularity at q'.

  Example
   g = 20
   S = ZZ/101[a,b,c]
   q' = ideal(a,b);
  Text
  Example
   I = q'^g
   C = S/(ideal random(g+2, I));
   p = sub(p', C);
   q = sub(q', C);
   geometricGenus (C, Conductor => q'^(g-1))

SeeAlso
 canonicalSeries
      ///

doc ///
Key 
 linearSeries
 (linearSeries, Ideal)
 (linearSeries, Ideal, Ideal)
 (linearSeries, List, Ring)
 (linearSeries, List, List, Ring)
 [linearSeries, Conductor]
 [linearSeries, ShowBase] 
Headline
 compute a linear series
Usage
 D = linearSeries Dplus
 D = linearSeries (Dplus, Dminus)
 D = linearSeries (DplusList, Ring)
 D = linearSeries (DplusList, DminusList, Ring)
Inputs
 Dplus: Ideal
   in the homogeneous coordinate ring A of a plane curve C
 Dminus: Ideal   
   in A
 DplusList: List
 DminusList: List
  lists representing the coordinates of points in P^2.
Outputs
 D: Matrix
   of size 1 x dim H^0(Dplus-Dminus). Entries are a basis of |Dplus - Dminus|
Description

  Text
   Given C, a (possibly singular) irreducible plane curve, this routine computes
   the complete linear series of a given divisor on the normalization C' of C
   by computations using data from the plane curve together
   with the conductor ideal $cond =ann_C(C/C)$, which can be computed by Macaulay
   or supplied by the user using the optional argument Conductor => cond.
   
   If D0' and Dinf'
   are effective divisors on C' whose ideals, as schemes, are pulled back
   from ideals D0 and Dinf of C, then 
   
   ell = linearSeries(D0,Dinf)
   
   returns a one-row matrix ell whose entries span a linear series with
   fixed point locus B on C' (including the conductor scheme) and form a basis
   of |D0'-Dinf'|+B.

   As an example, consider a quintic plane curve with an ordinary triple point
   and two more marked points:
  Example
   S = ZZ/32003[a,b,c]
   p = ideal(a,b)
   p1' = ideal(b,c)
   p2' = ideal(a,c)
   marked = intersect (p^3, p1', p2')
   C = S/(random(5, marked))
   red = map(C,S)
   p1 = red p1' 
   p2 = red p2'
  Text
   Since the delta invariant of a triple point is 3, the
   geometric genus of C is 3 less than the arithmetic genus computed natively,
   and the conductor is the square of the maximal ideal:
  Example
   genus C
   g = geometricGenus C
   conductor C
  Text
   As another example, we compute the complete linear series m*p_2-e*p1 for values of
   m from 3 to 12. In the following chart, each column represents one value of the degree, m-2.
   The first row gives the degree, the second row gives the value of the Euler characteristic
   
   $\chi {\mathcal O}_C(m*p2 - 2*p1)$
   
   as computed by the Riemann-Roch Theorem, and the last row gives the dimension of the 
   complete linear series as computed by this program. Since the linear series is
   general The last two rows agree starting in degree since the series is nonspecial from
   there on.
  Example
   e=2
   netList{
    {"degree m*p2 - 2*p1: "} | for m from 3 to 12 list m-e,
    {"chi, from Riemann-Roch: "} | for m from 3 to 12 list m-e-g+1,
    {"computed dimension: "} | for m from 3 to 12 list numgens trim ideal linearSeries(p2^m, p1^e)
    }
References
 "The Practic of Algebraic Curves" by David Eisenbud and Joe Harris
Caveat
 A bit slower in characteristic 0
SeeAlso
 conductor
 geometricGenus
///

doc///
Key
 projectiveImage
 (projectiveImage, Ideal)
 (projectiveImage, Ideal, Ideal)
 (projectiveImage, List, Ring)
 (projectiveImage, List, List, Ring)
 (projectiveImage, Matrix) 
 [projectiveImage, Conductor]
Headline
 Projective image of the map defined by a divisor or matrix
Usage
 I = projectiveImage Dplus
 I = projectiveImage (Dplus, Dminus)
 I = projectiveImage (DplusList, DminusList, C)
 I = projectiveImage (DplusList, C)

Inputs
 Dplus: Ideal
   in the homogeneous coordinate ring A of a plane curve C
 Dminus: Ideal   
   in A
 DplusList: List
 DminusList: List
  lists representing the coordinates of points in P^2.
 C: Ring
  the homogeneous coordinate ring of a plane curve  
Outputs
 I: Ideal
   the ideal of the image curve
Description
  Text
   The output ideal is the ideal of polynomial relations
   among the generators of the linear series |Dplus-Dminus|.

   If C is a general curve of genus 6, then C can be represented
   as a plane sextic with 4 nodes. Its canonical embedding is
   then the projective image of C by the space of cubic forms
   vanishing at the 4 nodes. This lies on the surface that is the
   image of P2 under the linear series consisting of the 
   cubics vanishing at the 4 nodes, a del Pezzo surface of
   degree 5.
  Example
   P5 = ZZ/101[x_0..x_5]
   P2 = ZZ/101[a,b,c]
   fourpoints = {
       {0,0,1},
       {1,0,0},
       {0,1,0},
       {1,1,1}}
   
   fourPointsIdeals = apply (fourpoints, L -> fromCoordinates(L,P2))

   nodes = intersect apply(fourPointsIdeals, p -> p)
   sings' = intersect apply(fourPointsIdeals, p -> p^2)
   C0 = P2/(ideal random(6, sings'))
   sings = sub (sings', C0)
   conductor C0 == sub(nodes, C0)
   B' = gens image basis (3,nodes)
   B = sub(B',C0);
   canonicalSeries(C0) == B
  Text
   Now the image of C under B lies on the image of 
   P^2 under B'. Since "projective image defines a ring",
   we need to make sure the two ideals are in the same ring
   to compare them:
  Example
   X = projectiveImage B'
   C = projectiveImage B
   betti res ideal C
   betti res ideal X
   isSubset(sub(ideal X, ring ideal C), ideal C)
SeeAlso
 geometricGenus
 canonicalImage
///



doc///
Key
 canonicalImage
 (canonicalImage, Ring)
 (canonicalImage, Ideal)
 [canonicalImage, Conductor]
Headline
 canonical model of the normalization of a plane curve
Usage
 R' = canonicalImage R
 R' = canonicalImage I
Inputs
 R: Ring
   the homogeneous coordinate ring of a plane curve
 I: Ideal
   homogeneous ideal of a plane curve
Outputs
 R': Ring
   the homogeneous coordinate ring of the canonical image of the normalization
Description
  Text
   The output is 
   the homogeneous coordinate ring 
   of the canonical image of the normalization
   the given curve.
   
   For example,  a plane
   sextic with 4 nodes is a curve of genus 10-4 = 6,
   so its canonical image is a curve of degree 10 in P5

  Example
   P5 = ZZ/101[x_0..x_5]
   P2 = ZZ/101[a,b,c]
   fourPoints = {{1,0,0},{0,1,0},{0,0,1},{1,1,1}}
   nodes = fromCoordinates(fourPoints, P2)
   sings' = intersect apply(fourPoints, p -> (fromCoordinates(p,P2))^2)
   C0 = P2/(ideal random(6, sings'))
   sings = sub (sings', C0)
   conductor C0 == sub(nodes, C0)
   C = canonicalImage C0
   betti res ideal C
   B' = gens image basis (3,intersect nodes)
  Text
   The ideal of nodes is the conductor, so
   the canonical series on C is the restriction of
   the set of cubics containing the nodes.
  Example
   B = sub(B',C);
   canC = projectiveImage B
   delPezzo = P5/ker(map(P2, P5, gens image basis (3,intersect nodes)))
   betti res ideal canC
   betti res ideal delPezzo
   
SeeAlso
 geometricGenus
 canonicalImage
///

doc///
Key
 Conductor
Headline
 Option to avoid computation
Usage
 M = geometricGenus(C,Conductor => cond)
Inputs
 C: Ring
 cond: Ideal
  the conductor of C

Description
  Text
   Computing the conductor involves computing the
   normalization, which is potentially expensive.
   If it is known in advance (as in the case of an
   ordinary multiple point) the user can insert it
   to avoid the computation.
   
SeeAlso
 geometricGenus
///

doc///
Key
 ShowBase
Headline
 Option for @TO linearSeries@
Usage
 (ls,B) = geometricGenus(C,ShowBase => true)
Inputs
 C: Ring
Outputs
 ls: Matrix
  1xn matrix whose entres span the linear series plus basepoints
 B: Ideal 
  the base locus
Description
  Text
   The linear series computed for a plane curve is given as
   a matrix of forms of a certain degree in the ideal of the curve;
   Each form vanishes at a divisor PLUS a fixed locus, in common
   to all the forms but not necessarily equal to their intersection
   (unless the linear series to be computed is base-point free).
  Example
   C = ZZ/19[a,b,c]/(ideal"a5+b5-c5")
   geometricGenus C == 6
   p = fromCoordinates({0,1,1}, C)
   (ls, B) = linearSeries(p^6, p^3, ShowBase => true)
SeeAlso
 linearSeries
 fromCoordinates
///


-* Test section *-

TEST/// --test of negative
   S = QQ[x,y,z]
   E = S/ideal"x3+y3+z3"
   o = {1,-1,0}
   p = {0,-1,1}
   negative(o,p,E)
   q = addition(o,p,p,E)
   r = addition(o,q, negative(o,p,E), E)
   assert (r == p)
///

TEST///
--a point of order 3
   kk = QQ
   S = kk[x,y,z]
   I = ideal"x3+y3+z3"
   E = S/I

   (o,p,q) = ({0,-1,1}, {-1,1,0},{1,0,-1})
    oE = fromCoordinates(o, E);    
    pE = fromCoordinates(p, E);
    qE = fromCoordinates(q, E);

    q' := o;
L = apply(4, i->(
    <<(i,q')<<endl;
    q' = addition(o,p,q',E)
    ))
assert(L_2==o)
///

TEST///
--a random point over QQ; shows growth of height
restart
loadPackage("PlaneCurveLinearSeries", Reload => true)
needsPackage "RandomPoints"
   setRandomSeed 0
   kk = QQ
   S = kk[x,y,z]
   (o,p) = ({1,1,1}, {-1,1,0})
   oS = fromCoordinates(o, S);    
   pS = fromCoordinates(p, S);
    
   points = intersect(oS,pS)
   I = ideal random(3,points)
   E = S/I
   oE = fromCoordinates(o, E);    
   pE = fromCoordinates(p, E);

   q' := o;
   netList for i from 0 to 6 list(
   q' = addition(o,p,q',E)
    )
///
TEST///--a cycle of length 15 over a finite field
restart
loadPackage("PlaneCurveLinearSeries", Reload => true)
needsPackage "RandomPoints"
setRandomSeed 0
   kk = ZZ/19
   S = kk[x,y,z]
   I = ideal"x3+3y3+xyz+z3"
   (o,p) = toSequence randomPoints (2,I) -- works for finite field
   E = S/I

q' = o;
L = apply(16,i->(
    q' = addition(o,p,q',E)
    ));
assert(L_0 == L_15)
///

///--case of a nodal cubic over a finite field
restart
loadPackage("PlaneCurveLinearSeries", Reload => true)
needsPackage "RandomPoints"
   setRandomSeed 7
   kk = ZZ/32003
   S = kk[x,y,z]
   (o,p,sing) = ({1,1,1}, {-1,1,0},{1,0,-1})
   oS = fromCoordinates(o, S)
   pS = fromCoordinates(p, S)
   singS = fromCoordinates(sing, S)

   I = random(3, intersect(oS,pS,singS^2))
   E = S/I
   oE = fromCoordinates(o, E)
   pE = fromCoordinates(p, E);
   assert(radical ideal singularLocus E == singS)
   o
   p
   oE = fromCoordinates(o,E)
   pE = fromCoordinates(p,E)
   (ls, base) = linearSeries(pE^2,oE, ShowBase =>true)
--   assert numgens ideal ls == 1 this assertion fails! Why??
    )

///

TEST///
S = ZZ/101[a,b,c]
C = S/ideal"a3+b3-c3"
P = (0,1,1)
assert(fromCoordinates({0,1,1}, C) == ideal (a, - b + c))
///

TEST///
R = QQ[x,y,z]
I = fromCoordinates ({5,-3,9}, R)
assert({5,-3,9} == toCoordinates I)

///

TEST///
S = ZZ/32003[a,b,c]
I = ideal"a3+b3-c3"
p'= ideal(a,b-c)
assert(isSubset(I,p'))
R = S/I
p = sub(p',R)
assert (geometricGenus R == 1)
assert(canonicalSeries R == matrix{{1_R}})
linearSeries (p)
L = for d from 3 to 7 list rank source ((res ideal projectiveImage p^d).dd_(d-2))
assert(all(L, ell->ell == 1))
///

TEST///
setRandomSeed 27 -- with setRandomSeed 0 the generator of C6 factors!
S = QQ[x,y,z]
sing3 = (ideal(x,y))^3
sing1 = (ideal(x,z))^2
C4 = ideal random(5, sing3) -- quintic with ord 3-point; genus 3, hyperell.
C5 = ideal random(5, sing1) -- quintic with node, genus 5
C6 = ideal random(5, intersect(sing1, sing3))-- quintic with ord 3-point and a node; genus 2
assert (numcols canonicalSeries C4 == 3)
assert (numcols canonicalSeries C5 == 5)
assert (numcols canonicalSeries C6 == 2)
assert (geometricGenus C6 == 2)
///
 
TEST///
--two characteristic pairs
kk = ZZ/32003
S = kk[a,b,c]; T = kk[s,t];
I = ker map(T,S, {s^7, s^6*t, s^3*t^4+s*t^6+t^7});I
R = S/I
assert (geometricGenus R == 0)
use S
p' = ideal(a-c,b+c)
isSubset(I,p')
radical ideal singularLocus R
--p' is a smooth point of the curve.
p = sub(p', R)
linearSeries p^2
L = for i from 0 to 6 list (rank source linearSeries (p^i)) 
assert(L == {1, 2, 3, 4, 5, 6, 7})
assert(degree projectiveImage p^3 == 3)
///

TEST///
setRandomSeed 0
kk = QQ
S = kk[x,y,z]
C1 = ideal (y^3 - x^2*(x-z)) -- cubic with a node; geometric genus 0
C2 = ideal(x^2+y^2+z^2)
C3 = ideal (x^4+y^4+z^4)
sing = (ideal(x,y))^3
C4 = ideal random(5, sing) -- quintic with ord 3-point; genus 3, hyperell.
C5 = ideal random(5, sing+(ideal(x,z))^2) -- quintic with ord 3-point and a node; genus 2
canonicalSeries(S/C1)
canonicalSeries(S/C2)
canonicalSeries(S/C3)
canonicalSeries(C4)
canonicalSeries(C5)

canonicalSeries C1 == 0
canonicalSeries C2 == 0
canonicalSeries C3 == vars ((ring C3)/C3)

geometricGenus C1 == 0
geometricGenus C2 == 0
geometricGenus C3 == 3
geometricGenus C4 == 3
///

TEST///
--hyperelliptic curves. Note that with g>=6, the conductor computation fails.
S = ZZ/101[a,b,c]
q' = ideal(a,b)
p' = ideal(b,c)
g = 8
C = S/random(g+2, intersect (q'^g, p'))
--conductor C -- fails for g>=6
q = sub(q', C)
p = sub(p',C)
--geometricGenus C
assert(geometricGenus (C, Conductor => q^(g-1)) == 8)
--canonicalSeries C
assert(canonicalSeries (C, Conductor =>q^(g-1)) ==  
    sub(matrix" a7, a6b, a5b2, a4b3, a3b4, a2b5, ab6, b7", C))
--linearSeries(p^(g+2))
g = 2 -- 8 takes too long!
C = S/random(g+2, intersect (q'^g, p'))
--conductor C -- fails for g>=6
q = sub(q', C)
p = sub(p',C)
C' = projectiveImage (p^(2*g+1),Conductor =>q^(g-1))
assert (codim C' == max (keys minimalBetti(ideal C')/first))
assert(degree ideal canonicalImage (C,Conductor =>q^(g-1)) == g-1)
///

TEST///
   C = ZZ/19[a,b,c]/(ideal"a4+b4-c4")
   assert(geometricGenus C == 3)
   assert ((p = fromCoordinates({0,1,1}, C)) == ideal"a, c-b")
   (ls, B) = linearSeries(p^6, p^3, ShowBase => true)
   assert(saturate B == ideal(b^2-2*b*c+c^2, a*b-a*c))
///
end--

///
restart
loadPackage ("PlaneCurveLinearSeries", Reload => true)
uninstallPackage "PlaneCurveLinearSeries"      
installPackage "PlaneCurveLinearSeries"      
check "PlaneCurveLinearSeries"      
viewHelp PlaneCurveLinearSeries
///





 





