newPackage(
	"RandomCanonicalCurves",
    	Version => "0.6",
    	Date => "March 4, 2011",
    	Authors => {{Name => "Frank-Olaf Schreyer",
		  Email => "schreyer@math.uni-sb.de",
		  HomePage => "http://www.math.uni-sb.de/ag/schreyer/"},
	            {Name => "Hans-Christian Graf v. Bothmer",
	             Email => "bothmer@uni-math.gwdg.de",
		     HomePage => "http://www.crcg.de/wiki/User:Bothmer"}
                   },
    	Headline => "Construction of random smooth canonical curves up to genus 14",
	Keywords => {"Examples and Random Objects"},
     	PackageImports => {"Truncations","RandomSpaceCurves","RandomPlaneCurves","RandomGenus14Curves"},
     	PackageExports => {"RandomObjects"},
    	DebuggingMode => false
        )

if not version#"VERSION" >= "1.4" then error "this package requires Macaulay2 version 1.4 or newer"

export{
     "canonicalCurve"
     }

undocumented {
     randomCanonicalModelOfPlaneCurve,
     randomCanonicalModelOfSpaceCurve,
     randomCanonicalCurve,
     certifyCanonicalCurve}

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
     -- the canoncial linear system
     omegaC := presentation prune truncate(0,Ext^1(I,S^{ -4}));
     graph := substitute(vars R,RS)*substitute(omegaC,RS);
     J := saturate(ideal graph,substitute(y_0,RS));
     Icanonical := ideal mingens substitute(J,R);
     return Icanonical);


randomCanonicalCurve=method(TypicalValue=>Ideal,Options=>{Certify=>false})

-- construct a random canonical curve of genus g
-- by using plane cuves, space curves and verras construction for g=14
-- R a ring with g variables
randomCanonicalCurve(ZZ,PolynomialRing):= opt -> (g,R)->(
     if g>14 or g<4 then error "no method implemented";
     d := null;
     if g<=10 then (
	  s:=floor(g/3); -- the speciality of a plane model of minimal degree
	  d=g+2-s; -- the degree of the plane model
	  return randomCanonicalModelOfPlaneCurve(d,g,R,Certify=>opt.Certify));
     -- the following space curve models are choosen such that the
     -- brill noether number is positive and the construction via
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

beginDocumentation()
doc ///
  Key
    RandomCanonicalCurves
  Headline
    Construction of canonical curves of genus less or equal to 14
  Description
   Text
    This package bundles the constructions for random points in the moduli spaces of curves $M_g$ for $g \leq 14$ based on
    the proofs of unirationality of $M_g$ by Severi, Sernesi, Chang-Ran and Verra.

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

-- check that the number of generators of the constructed
-- canonical curve is as expected
TEST ///
setRandomSeed("alpha");
apply(5..14,g->(
	  assert (binomial(g-2,2) == rank source mingens (I=(random canonicalCurve)(g,(ZZ/101)[x_0..x_(g-1)])))
     ))
///

end
viewHelp

restart
uninstallPackage("RandomCanonicalCurves")
time installPackage("RandomCanonicalCurves",RerunExamples=>true,RemakeAllDocumentation=>true);
-- caveat: Testing takes some time
viewHelp"RandomCanonicalCurves"

time check ("RandomCanonicalCurves")
-- timing does not work
