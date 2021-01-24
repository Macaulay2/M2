newPackage(
	"RandomGenus14Curves",
    	Version => "0.6",
    	Date => "March 4, 2011",
    	Authors => {{Name => "Frank-Olaf Schreyer",
		  Email => "schreyer@math.uni-sb.de",
		  HomePage => "http://www.math.uni-sb.de/ag/schreyer/"},
	            {Name => "Hans-Christian Graf v. Bothmer",
	             Email => "bothmer@uni-math.gwdg.de",
		     HomePage => "http://www.crcg.de/wiki/User:Bothmer"}
                   },
    	Headline => "random smooth curves of genus 14",
	Keywords => {"Examples and Random Objects"},
     	PackageExports => {"RandomObjects"},
	PackageImports => {"Truncations"},
    	DebuggingMode => false
        )

if not version#"VERSION" >= "1.4" then error "this package requires Macaulay2 version 1.4 or newer"

export{
     "randomCurveGenus14Degree18inP6",
     "randomCurveGenus8Degree14inP6",
     "randomCanonicalCurveGenus8with8Points",
     "curveGenus14Degree18inP6",
     "canonicalCurveGenus14"
     }

randomCanonicalCurveGenus8with8Points = method()

randomCanonicalCurveGenus8with8Points PolynomialRing := R ->(
     --Input: R a polynomial ring in 8 variables,
     --Output: a pair of an ideal of a canonical curve C
     --        together with a list of ideals of 8 points
     --Method: Mukai's structure theorem on genus 8 curves.
     --  Note that the curves are have general Clifford index.
     FF:=coefficientRing R;
     p:=symbol p;
     -- coordinate ring of the Plucker space:
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



beginDocumentation()

doc ///
  Key
    RandomGenus14Curves
  Headline
    Construction of random curves of genus 14
  Description
   Text
    In this package the unirationality construction of the moduli space $M_{14}$ of curves of genus 14 due to Verra is implemented.
    The main references are

    \ \ \ \ \ [Mu] S. Mukai, Curves, $K3$ surfaces and Fano $3$-folds of genus $\leq 10$. Algebraic geometry and commutative algebra, Vol. I, 357-377, Kinokuniya, Tokyo, 1988.

    \ \ \ \ \ [Ve] A. Verra, The unirationality of the moduli spaces of curves of genus 14 or lower. Compos. Math. 141 (2005), no. 6, 1425-1444.
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

TEST ///
  -- check that there are not to many non-detected problems in the construction.
  -- This code finds errors in codimension 4 with high probability
  -- since 3^4 \approx 100
  setRandomSeed("alpha")
  Fq= ZZ/3
  T = Fq[t_0..t_6]
  time L=apply(100,i->(print i;(random curveGenus14Degree18inP6)(T,Attempts=>1)));#L
  print tally(apply(L,l->l=!=null))
  -- uses ca. 150 seconds
///

TEST ///
  -- check that the certification sometimes works
  -- (only errors in codim 1 are detected)
  setRandomSeed("alpha")
  Fq= ZZ/11
  T = Fq[t_0..t_6]
  time L=apply(10,i->(print i;(random curveGenus14Degree18inP6)(T,Attempts=>1,Certify=>true)));#L
  print tally(apply(L,l->l=!=null))
  -- uses ca. 130 seconds
  -- usually about half the checks fail and half the checks work
  -- a more thorough check is not possible, since there is
  -- a 160 second time limit for test.
///


end

restart
uninstallPackage("RandomGenus14Curves")
time installPackage("RandomGenus14Curves",RerunExamples=>true,RemakeAllDocumentation=>true);
viewHelp"RandomGenus14Curves"

check("RandomGenus14Curves")
-- takes about 6 minutes

restart
needsPackage("RandomGenus14Curves")

-- a more thorough check of certification (almost codim 3)
Fq= ZZ/5
T = Fq[t_0..t_6]
time L=apply(100,i->(print i;(random curveGenus14Degree18inP6)(T,Attempts=>1,Certify=>true)));#L
-- used 460.28 seconds
print tally(apply(L,l->l=!=null))
-- Tally{false => 97}
--       true => 3
