--*- coding: utf-8 -*-
---------------------------------------------------------------------------
-- PURPOSE: Calculating arborescent resolutions and higher cotangent cohomology
-- PROGRAMMER : Nathan Ilten
-- UPDATE HISTORY : August 2025
---------------------------------------------------------------------------
newPackage("CotangentCohomology",
    Headline => "arborescent resolutions and cotangent cohomology",
    Version => "1.0",
    Date => "June 18, 2026",
    Authors => {
        {Name => "Nathan Ilten",
	  HomePage => "http://www.sfu.ca/~nilten/",
	  Email => "nilten@sfu.ca"}},
    HomePage => "https://github.com/Macaulay2/M2",
    PackageExports => {"DGAlgebras"},
    Keywords => {"Deformation Theory","Homological Algebra"},
    )

---------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--
-- Copyright 2026 Nathan Owen Ilten
--
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
---------------------------------------------------------------------------

export {"ArborescentResolution",
    "arborescentResolution",
    "liftArborescentResolution",
    "dualCotangent",
    "cotangentCohomology",
    "TT",
    "Arborescent"
    }


protect symbol rho
protect symbol prevDegreeOldBasis
protect symbol topDegreeOldBasis


---------------------------------------------------------
-- Code  for creating Arborescent resolutions
---------------------------------------------------------
ArborescentResolution = new Type of DGAlgebra

--Create first 3 steps of arborescent resolution from free resolution
setupArborescentResolution = method()
setupArborescentResolution Complex := ArborescentResolution => F->(
    K1 := setDiff(freeDGAlgebra(ring F,apply(degrees F_1,i->{1}|i)),flatten entries F.dd_1,InitializeComplex=>false,InitializeDegreeZeroHomology=>false);
    if F.dd_2==0 then (
	K1.cache.resolution=F;
	return new ArborescentResolution from K1);
    K2 := adjoinVariables(K1,flatten entries(getBasis(1,K1)*F.dd_2));
    rho := polyDifferential(2,K1)//(F.dd_2);
    K3 := new ArborescentResolution from adjoinVariables(K2,
	(flatten entries (-((vars K2.natural)_(positions(K2.Degrees,i->i_0==2))*rho)+sub(getBasis(2,K1),K2.natural)))|(
	    flatten entries ((vars K2.natural)_(toList (numgens F_1..numgens F_1+numgens F_2-1))*F.dd_3)));
    K3.cache.resolution = F;
    K3.cache.rho = rho;
    K3.cache.prevDegreeOldBasis = getBasis(2,K1);
    K3.cache.topDegreeOldBasis = getBasis(3,K2);
    K3)


--Extend arborescent resolution one step further
liftArborescentResolution = method()
liftArborescentResolution ArborescentResolution := ArborescentResolution => K->(
    F := K.cache.resolution;
    if F.dd_2==0 then return K;
    topDegree := max apply(K.Degrees,i->i_0); -- 1+ highest cohom degree of generators; we'll add gens in this degree
    topDegreeBasis := getBasis(topDegree,K); 
    p := numgens source topDegreeBasis;
    p' := numgens source K.cache.topDegreeOldBasis;
    p'' := numgens source K.cache.prevDegreeOldBasis;
    r := numgens F_(topDegree);
    r' := numgens F_(topDegree-1);
    --let's figure out gamma and alpha
    olddiff := polyDifferential(topDegree,K);
    n := numgens target olddiff;
    gamma := (olddiff_(toList (0..p'-1))^(toList (0..p''-1))); 
    alpha := (olddiff_(toList (0..p'-1))^(toList (n-r'..n-1))); 
    newrho := (K.cache.rho*gamma+alpha)//(F.dd_(topDegree)); 
    newdiff := flatten entries ((topDegreeBasis*(id_(ZZ^(p'))||(-gamma)||-newrho))|(topDegreeBasis_(toList (p-r..p-1))*(F.dd_(topDegree+1))));
    newK := new ArborescentResolution from adjoinVariables(K,newdiff);
    newK.cache.resolution=F;
    newK.cache.rho=newrho;
    newK.cache.prevDegreeOldBasis=K.cache.topDegreeOldBasis;
    newK.cache.topDegreeOldBasis=getBasis(topDegree+1,K);
    if K.cache.?Ideal then newK.cache.Ideal=K.cache.Ideal; 
    newK)


--create arborescent resolution of length n from free resolution F
arborescentResolution = method(Options=>{Verbose=>0})

arborescentResolution (Complex,ZZ) := ArborescentResolution => opts->(F,n)->(
    K := setupArborescentResolution F;
    i := 3;
    while (i<n) do (
        if opts.Verbose>0 then print ("Lifting to degree "|toString(i+1));
        K = liftArborescentResolution K;
        i = i+1);
    K)

arborescentResolution (Ideal,ZZ) := ArborescentResolution => opts->(I,n)->(
    local Q;
    if I.cache.?ArborescentResolution then Q=I.cache.arborescentResolution else Q=res I;
    K := arborescentResolution(Q,n,opts);
    I.cache.arborescentResolution=K;
    K)

arborescentResolution (ArborescentResolution,ZZ) := ArborescentResolution => opts->(K,n)->(
    i := max apply(K.Degrees,i->i_0);
    while (i<n) do (
        if opts.Verbose>0 then print ("Lifting to degree "|toString(i+1));
        K = liftArborescentResolution K;
        i = i+1);
    K)


------------------------------------------------------------------
-- Code for cotangent cohomology
------------------------------------------------------------------

--replaces any polynomial with the terms of degree at most 1
degreeOnePart := f->(
    C := coefficients f;
    pL := positions(flatten entries C_0,i->member(i,{1_(ring f)}|gens ring f));
    (((C_0)_pL)*(C_1)^pL)_(0,0))


--creates complex from which cotangent cohomology is computed out to degree n
dualCotangent = method(Options=>{Verbose=>0,Arborescent=>true})
dualCotangent (DGAlgebra,ZZ) := Complex => opts->(K,n)->(
    degreeList := K.Degrees;
    R := K.ring;
    if degreeList=={} then return complex map(R^(# gens R),R^0,0);
    if n>max apply(degreeList,i->i_0) then (
        if opts.Verbose>0 then print "Warning: degree bound exceeds largest homological degree of generators.";
        n=max apply(degreeList,i->i_0));
    internalDegrees := -1*apply(degreeList,i->drop(i,1));
    degreePositions := {0}|apply(n,i->#positions(degreeList,j->j_0<=i+1)); 
    moduleList := apply(n,i->R^(internalDegrees_(toList(degreePositions_i..degreePositions_(i+1)-1))));
    if opts.Verbose>0 then print "Reducing differential";
    reducedDiff := apply((flatten entries matrix K.diff)_(toList(0..degreePositions_n-1)),x->degreeOnePart x);
    local indexListTarget;
    local indexListSource;
    if opts.Verbose>0 then print "Computing differential in terms of basis";
    mapList := apply(n-1,i->(
	    indexListTarget = toList(degreePositions_i..degreePositions_(i+1)-1);
	    indexListSource = toList(degreePositions_(i+1)..degreePositions_(i+2)-1);
	    matrix {reducedDiff_indexListSource}//matrix {(gens K.natural)_indexListTarget}));
    jac := jacobian sub(matrix {reducedDiff_(toList(degreePositions_0..degreePositions_1-1))},R);
    firstMap := dual map(dual moduleList_0,,dual jac);
    dual (complex ({firstMap}|(apply(n-1,i->map(moduleList_i,moduleList_(i+1),sub(mapList_i,R))))))
    )

dualCotangent (Ideal,ZZ) := Complex => opts->(I,n)->(
    local K;
    if opts.Arborescent or I==0 then (
	if opts.Verbose>0 then print "Calculating arborescent resolution";
	K = arborescentResolution(I,n,Verbose=>opts.Verbose));
    if not (opts.Arborescent or I==0) then (
	if opts.Verbose>0 then print "Calculating acyclic closure";
	K = acyclicClosure(koszulComplexDGA I,EndDegree=>(n-1)));
    dualCotangent(K,n,opts)
    )

--computes cotangent cohomology modules
cotangentCohomology = method(Options=>{Verbose=>0,Arborescent=>true})
cotangentCohomology (DGAlgebra,Module,ZZ)  := 
cotangentCohomology (Ideal,Module,ZZ) := Module => opts->(I,M,i)->(
    HH^i(dualCotangent(I,i+1,opts)**M))

cotangentCohomology (DGAlgebra,ZZ) := Module => opts->(K,i)->(cotangentCohomology(K,comodule ideal zerothHomology K,i,opts))
cotangentCohomology (Ideal,ZZ) := Module => opts->(I,i)->(cotangentCohomology(I,comodule I,i,opts))

    

TT = new ScriptedFunctor from {
     superscript => (
	  j -> new ScriptedFunctor from {
	       argument => (
		    X -> cotangentCohomology((sequence X)|(sequence j))
		    )
	       }
	  ),
    argument => (
	  X -> cotangentCohomology(sequence X)
	  )
     }


beginDocumentation()
document {
     Key => CotangentCohomology,
     Headline => "arborescent resolutions and cotangent cohomology",
     PARA{},
     "This package is used to construct arborescent resolutions and compute cotangent cohomology groups.
      An arborescent resolution of an algebra R is a special kind of free differential graded algebra
      resolving R. See ", TO "Background on arborescent resolutions", " and ",
      UL {{"Hancharuk, Laurent-Gengoux, and Strobl, Koszul-Tate resolutions and decorated trees, 
      2024, arXiv:2406.03955"}},
     PARA{"For mathematical background on how arborescent resolutions are computed here, see"},
     UL {{"Ilten, Meazzini, and Petracci, Higher cotangent cohomology for Stanley-Reisner Rings, 2026, arXiv:2606.16829"}},
     PARA{"Cotangent cohomology for an algebra R is computed (in characteristic zero) via a complex obtained from a free DG-algebra resolution, either
     an arborescent resolution or one obtained via acyclic closure."},
     PARA{"The author thanks Francesco Meazzini, Frank Moore, and Andrea Petracci for helpful conversations and suggestions."}
 }


 



doc ///
  Key
    arborescentResolution
    (arborescentResolution,Complex,ZZ)
    (arborescentResolution,Ideal,ZZ)
    (arborescentResolution,ArborescentResolution,ZZ)
    [arborescentResolution,Verbose]
  Headline
    Computes a truncated arborescent resolution
  Usage
    A = arborescentResolution(B,n)
    A = arborescentResolution(I,n)
    A = arborescentResolution(F,n)
  Inputs
    B:ArborescentResolution 
    I:Ideal
    F:Complex 
    n:ZZ
    Verbose=>ZZ
  Outputs
    A:ArborescentResolution
  Description
    Text
	Inductively constructs a truncated arborescent resolution up to homological degree @ TT "n" @. For @ TT "I" @ an ideal in a
	polynomial ring @ TT "R"@, @ TT "arborescentResolution(I,n)"@ constructs an arborescent resolution of @ TT "R/I" @. The resulting
	arborescent resolution is cached in @ TT "I.cache.arborescentResolution" @.
	For more control, one may specify the underlying free resolution @ TT "F" @ of @ TT "R/I"@ by using @ TT "arborescentResolution(F,n)"@.
   Text
	Given an existing truncated arborescent resolution @ TT "B" @, @ TT "arborescentResolution(B,n)" @ extends it to homological
	degree @ TT "n" @.

   Example
      R=QQ[x_1..x_5]
      I=ideal {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_1}
      A=arborescentResolution(I,3)
   Text
       The optional input @ TT "Verbose" @ controls the level of verbosity during the computation.
   Example
      R=QQ[x_1..x_4]
      M=matrix {{x_1,x_2,x_3},{x_2,x_3,x_4}}
      I=minors (2,M)
      A=arborescentResolution(I,5,Verbose=>4)

  SeeAlso 
      ArborescentResolution
      liftArborescentResolution
///

doc ///
  Key
    liftArborescentResolution
    (liftArborescentResolution,ArborescentResolution)
  Headline
    lifts a truncated arborescent resolution to one degree higher
  Usage
    A = liftArborescentResolution(B)
  Inputs
    B:ArborescentResolution 
  Outputs
    A:ArborescentResolution
  Description
   Text
      Given an existing truncated arborescent resolution @ TT "B" @ up to homological degree @ TT "n-1" @, @ TT "liftArborescentResolution(B)" @ extends it to homological degree @ TT "n" @. This method is unlikely to be used directly by the user.
   Example
      R=QQ[x_1..x_5]
      I=ideal {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_1}
      B=arborescentResolution(I,3)
      liftArborescentResolution(B)
  SeeAlso 
      ArborescentResolution
      arborescentResolution
///


doc ///
  Key
    ArborescentResolution
  Headline
    The class of all arborescent resolutions
  Description
    Text
      An @ TT "ArborescentResolution" @ @ TT "A"@  is a special class of @ TO DGAlgebra @ encoding a truncation of an arborescent resolution, see @ TO "Background on arborescent resolutions" @.
      The hash table @ TT "A.cache" @ contains extra information that allows one to lift the truncation to higher degree via @ TO liftArborescentResolution @.
      An @ TT "ArborescentResolution" @ is typically created via @ TO arborescentResolution @. The information of the resolution may be
      accessed using the methods applicable to any @ TO DGAlgebra @.
    Example
      R = QQ[x_1..x_5]
      I = ideal {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_1}
      B = arborescentResolution(I,3)
      C = toComplex(B,3)
      HH_0(C)
      prune HH_1(C)
      prune HH_2(C)
  SeeAlso 
      arborescentResolution
      liftArborescentResolution
      "Background on arborescent resolutions"
///


doc ///
  Key
    "Background on arborescent resolutions"
  Headline
    A mathematical description of arborescent resolutions
  Description
    Text
      Let @ TEX "$S$" @ be a polynomial ring over a field and @ TEX "$R$" @ a quotient of @ TEX "$S$" @. An arborescent resolution is a special kind of semifree commutative differential graded algebra @ TEX "$A$" @ over @ TEX "$S$" @ resolving @ TEX "$R$" @. More precisely, let @ TEX "$F$" @ be any free resolution of @ TEX "$R$" @ as an @ TEX "$S$" @-module. An arborescent resolution of @ TEX "$R$" @ with respect to the free resolution @ TEX "$F$" @ is a semifree commative differential graded algebra A such that the following is true.
    Text
      Firstly, the degree zero piece of @ TEX "$A$" @ is @ TEX "$S$" @.
    Text
      Secondly, the degree @ TEX "$i$" @ piece of @ TEX "$A$" @ decomposes as a direct sum @ TEX "$A_i=F_i \\oplus A_i' \\oplus A_i''$"@, where @ TEX "$F_i$" @ is the degree @ TEX "$i$" @ piece of @ TEX "$F$" @, @ TEX "$A_i''$" @ is the part of @ TEX "$A_i$" @ generated by products of elements of lower degrees, and @ TEX "$A_i'$" @ is a copy of @ TEX "$A_{i-1}''$" @, but viewed as having degree @ TEX "$i$" @.
    Text
      Thirdly, the differential @ TEX "$\\partial$" @ of @ TEX "$A$" @ has a particular form. On elements of @ TEX "$F$" @, @ TEX "$\\partial$" @ is simply the differential of @ TEX "$F$" @. Moreover, composing @ TEX "$\\partial$" @ with projections onto direct summands, the map @ TEX "$A_i'\\to A_{i-1}''$" @ is simply the identity, and the map @ TEX "$A_i'\\to A_{i-1}'$" @ is the negative of the map @ TEX "$A_{i-1}''\\to A_{i-2}''$" @.
    Text
      Finally, @ TEX "$A$" @ has zeroeth homology equal to @ TEX "$R$" @ and is acyclic in higher degrees.
    Text
      @PARA{"For more mathematical background on arborescent resolutions, see"}@

      @UL {{"Ilten, Meazzini, and Petracci, Higher cotangent cohomology for Stanley-Reisner Rings, 2026, arXiv:2606.16829"}}@
///


doc ///
  Key
    dualCotangent
    (dualCotangent,Ideal,ZZ)
    (dualCotangent,DGAlgebra,ZZ)
    [dualCotangent,Verbose]
    [dualCotangent,Arborescent]
  Headline
    Gives a sequence of modules for use in computing cotangent cohomology
  Usage
    C = dualCotangent(A,n)
    C = dualCotangent(I,n)
  Inputs
    A:DGAlgebra 
    I:Ideal
    n:ZZ
    Verbose=>ZZ
    Arborescent=>Boolean
  Outputs
    C:Complex
  Description
    Text
        Given an ideal @ TT "I"@ of a polynomial ring @ TT "R"@, @ TT "dualCotangent(I,n)"@ constructs a sequence @ TT "C"@ of free @TT "R"@ modules
	of length @ TT "n" @ such that after tensoring with an @ TT "R/I"@-module @ TT "M"@, the cohomology of the resulting complex up to degree @ TT "n-1" @ is
	the cotangent cohomology of @ TT "R/I" @ with coefficients in @ TT "M" @. If @ TT "Arborescent" @ is true, as is the case by
	default, this sequence is obtained from an arborescent resolution; if @ TT "Arborescent" @ is false, then a minimal model of @ TT "R/I" @ is used instead, see @ TO "minimalModel" @.
    Text
	If @ TT "A" @ is a @ TO DGAlgebra @ resolving @ TT "R/I" @, then @ TT "dualCotangent(A,n)" @ returns a similar sequence,
	making use of the already computed DG-algebra resolution @TT "A"@.
    Example
      R = QQ[x_1..x_5]
      I = ideal {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_1}
      C = dualCotangent(I,3)
    Text
      The optional input @ TT "Verbose" @ controls the level of verbosity during the computation.

 Caveat
       In general, the square of the differential of @ TT"C"@ only vanishes modulo @TT"I"@.
       If @ TT "A" @ is a @ TO DGAlgebra @ resolving @ TT "R/I" @ whose generators have homological degree all less than @ TT "n" @, the sequence
	of free @TT "R"@ modules resulting from @ TT "dualCotangent(A,n)" @  only has length equal to the maximal homological degree of the generators of @ TT "A" @.
///


doc ///
  Key
    cotangentCohomology
    (cotangentCohomology,Ideal,ZZ)
    (cotangentCohomology,DGAlgebra,ZZ)
    (cotangentCohomology,Ideal,Module,ZZ)
    (cotangentCohomology,DGAlgebra,Module,ZZ)
    TT
    [cotangentCohomology,Verbose]
    [cotangentCohomology,Arborescent]
    Arborescent
  Headline
    Computes a cotangent cohomology module
  Usage
    C = cotangentCohomology(I,n)
    C = cotangentCohomology(A,n)
    C = cotangentCohomology(I,M,n)
    C = cotangentCohomology(A,M,n)
  Inputs
    A:DGAlgebra 
    I:Ideal
    M:Module
    n:ZZ
    Verbose=>ZZ
    Arborescent=>Boolean
  Outputs
    C:Module
  Description
    Text
        Given an ideal @ TT "I"@ of a polynomial ring @ TT "R"@ and an @ TT "R/I"@ module @ TT "M" @,  @ TT "cotangentCohomology(I,M,n)"@
	 computes the @TT"n"@th cotangent cohomology of @TT"R/I"@ with coefficients in @TT"M"@. If the module @TT"M"@ is omitted,
	 the coefficients are taken in @TT"R/I"@.
	If @ TT "Arborescent" @ is true, as is the case by
	default, the computation makes use of an arborescent resolution; if @ TT "Arborescent" @ is false, then a minimal model of @ TT "R/I" @ is used instead, see @ TO "minimalModel" @.
    Text
	If @ TT "A" @ is a @ TO DGAlgebra @ resolving @ TT "R/I" @, then @ TT "cotangentCohomology(A,M,n)" @ also computes
	the @TT"n"@th cotangent cohomology of @TT"R/I"@ with coefficients in @TT"M"@,	making use of the already computed DG-algebra resolution @TT "A"@.
    Text
        The @TO ScriptedFunctor@ @TT"TT"@ is an abbreviation for @ TT "cotangentCohomology"@, e.g.
	@TT"TT^n(I,M)"@ returns the same as @ TT "cotangentCohomology(I,M,n)"@.
    Text
       The optional input @ TT "Verbose" @ controls the level of verbosity during the computation.
   Example
      R = QQ[x_1..x_5]
      I = ideal {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_1}
      prune TT^3(I)
      prune TT^3(I,comodule ideal {x_1,x_2*x_3,x_3*x_4,x_4*x_5})
 Caveat
       In general, this method will only give correct results if @ TT "R"@ has characteristic zero, since the equivalence between simplicial and DG algebras fails in positive characteristic.
///


TEST ///
R = QQ[x,y,z]
I = ideal {x*y,x*z,y*z}
A = arborescentResolution(I,6)
C = toComplex(A,6)
assert (all(1..5,i->HH_i(C) == 0) and (HH_0(C) == comodule I))
///


TEST ///
R = QQ[x,y,z]
I = ideal {x*y,x*z,y*z}
assert ((3,0,1,3,6) == apply(1..5,i->numgens source basis TT^i(I)))
assert ((3,0,1,3,6) == apply(1..5,i->numgens source basis TT^i(I,Arborescent=>false)))
assert (2 == numgens source basis TT^2(I,comodule ideal {x,y,z}))
///

TEST ///
R = QQ[x,y,z,DegreeRank=>3]
I = ideal {x*y,x*z,y*z}
assert (degrees basis TT^3(I) == {{{-1, -1, -1}, {-1, -1, -1}, {-1, -1, -1}, {-2, 0, 0}, {0, -2, 0}, {0, 0, -2}}, {{-1, -1, -1}}})
///

TEST ///
R = QQ[x,y]
I = ideal {x*y}
A = arborescentResolution(I,6)
assert (numgens source basis TT^1(I) == 1 and TT^17(I) == 0)
///

TEST ///
R = QQ[x,y,z,Degrees=>entries id_(ZZ^3)]
I = ideal {x*y*z}
assert (2 == numgens source basis({1,1,0},TT^0(I)))
///

TEST ///
R = QQ[x]
TT^0(ideal 0_R) == R^1
TT^1(ideal 0_R) == 0
///

TEST ///
R = QQ[x_1..x_6]
M = genericMatrix(R,2,3)
I = minors(2,M)
assert ((0,0,0,0,0,1) == apply(1..6,i->numgens source basis TT^i(I)))
assert ((0,0,0,0,0,1) == apply(1..6,i->numgens source basis TT^i(I,Arborescent=>false)))
assert ((3,2,3,6,11,18) == apply(1..6,i->numgens source basis TT^i(I,comodule ideal gens R)))
///

TEST ///
R = QQ[x_1..x_4]
M = matrix {{x_1,x_2,x_3},{x_2,x_3,x_4}}
I = minors (2,M)
assert ((2,0,0,1,2,4) == apply(1..6,i->numgens source basis TT^i(I)))
CT = dualCotangent(I,5)
assert (betti CT == new BettiTally from {(-1,{-2},-2) => 3, (-2,{-3},-3) => 2, (-3,{-4},-4) => 3, (-4,{-5},-5) => 6, (-4,{-6},-6) => 1, (-5,{-6},-6) =>
      12, (-5,{-7},-7) => 6, (0,{-1},-1) => 4})
assert ((2,0,0,1) == apply(1..4,i->numgens source basis TT^i(I)))
///

TEST ///
R = QQ[x_1..x_4]
M = matrix {{x_1,x_2,x_3},{x_2,x_3,x_4}}
I = minors (2,M)
A = arborescentResolution(I,2)
A3 = liftArborescentResolution(A)
C = toComplex(A3,4)
assert (all(1..3,i->HH_i(C) == 0) and (HH_0(C) == comodule I))
///




