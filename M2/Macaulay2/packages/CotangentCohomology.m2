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



---------------------------------------------------------
-- Code  for creating Arborescent resolutions
---------------------------------------------------------
ArborescentResolution= new Type of DGAlgebra

--Create first 3 steps of arborescent resolution from free resolution
setupArborescentResolution:=F->(
    K1:=setDiff(freeDGAlgebra(ring F,apply(degrees F_1,i->{1}|i)),flatten entries F.dd_1,InitializeComplex=>false,InitializeDegreeZeroHomology=>false);
    if F.dd_2==0 then (
	K1.cache#"resolution"=F;
	return new ArborescentResolution from K1);
    K2:=adjoinVariables(K1,flatten entries(getBasis(1,K1)*F.dd_2));
    rho:=((toComplex(K1,2)).dd_2)//(F.dd_2);
    K3:=new ArborescentResolution from adjoinVariables(K2,
	(flatten entries (-((vars K2.natural)_(positions(K2.Degrees,i->i_0==2))*rho)+sub(getBasis(2,K1),K2.natural)))|(
	    flatten entries ((vars K2.natural)_(toList (numgens F_1..numgens F_1+numgens F_2-1))*F.dd_3)));
    K3.cache#"resolution"=F;
    K3.cache#"rho"=rho;
    K3.cache#"P''"=getBasis(2,K1);
    K3.cache#"P'"=getBasis(3,K2);
    K3)


--Extend arborescent resolution one step further
liftArborescentResolution=method(TypicalValue=>ArborescentResolution)
liftArborescentResolution ArborescentResolution:=K->(
    F:=K.cache#"resolution";
    if F.dd_2==0 then return K;
    i:=1+max apply(K.Degrees,i->i_0); -- 1+ highest cohom degree of generators; we'll add gens in this degree
    P:=getBasis(i-1,K); -- basis for P_(i-1)
    P':=K.cache#"P'"; -- basis for P_(i-1)^old=P'_i
    P'':=K.cache#"P''"; -- basis for P_(i-2)^old=P'_(i-1)
    p:=numgens source P;
    p':=numgens source P';
    p'':=numgens source P'';
    r':=numgens F_(i-2);
    r:=numgens F_(i-1);
    --let's figure out gamma_(i-1) and alpha_(i-1)
    d:=(toComplex(K,i-1)).dd_(i-1);
    n:=numgens target d;
    gamma:=(d_(toList (0..p'-1))^(toList (0..p''-1))); --gamma_(i-1) as matrix
    alpha:=(d_(toList (0..p'-1))^(toList (n-r'..n-1))); --alpha_(i-1) as matrix
    oldrho:=K.cache#"rho"; --rho_(i-2) as matrix
    newrho:=(oldrho*gamma+alpha)//(F.dd_(i-1)); --rho_(i-1) as matrix
    --change matrices to lists
   gammaL:=sub(P''*gamma,K.natural);
   --alphaL:=(matrix{((gens(K.natural)))_(toList(-r..-1))})*alpha;
   newdiff:=flatten entries ((P*(id_(ZZ^(p'))||(-gamma)||-newrho))|(P_(toList (p-r..p-1))*(F.dd_i)));
   newK:=new ArborescentResolution from adjoinVariables(K,newdiff);
   newK.cache#"resolution"=F;
   newK.cache#"rho"=newrho;
   newK.cache#"P''"=P';
   newK.cache#"P'"=getBasis(i,K);
   if K.cache.?Ideal then newK.cache.Ideal=K.cache.Ideal; 
   newK)


--create arborescent resolution of length n from free resolution F
arborescentResolution=method(TypicalValue=>ArborescentResolution,Options=>{Verbose=>0})

arborescentResolution (ChainComplex,ZZ) :=
arborescentResolution (Complex,ZZ):=opts->(F,n)->(
    K:=setupArborescentResolution F;
    i:=3;
    while (i<n) do (
	if opts#Verbose>0 then print ("Lifting to degree "|toString(i+1));
	K=liftArborescentResolution K;
	i=i+1);
    K)

arborescentResolution (Ideal,ZZ):=opts->(I,n)->(
    local Q;
    if I.cache.?ArborescentResolution then Q=I.cache.ArborescentResolution else Q=res I;
    K:=arborescentResolution(Q,n,opts);
    I.cache.ArborescentResolution=K;
    K)

arborescentResolution (ArborescentResolution,ZZ):=opts->(K,n)->(
    i:=max apply(K.Degrees,i->i_0);
    while (i<n) do (
	if opts#Verbose>0 then print ("Lifting to degree "|toString(i+1));
	K=liftArborescentResolution K;
	i=i+1);
    K)

------------------------------------------------------------------
-- Code for cotangent cohomology
------------------------------------------------------------------

--replaces any polynomial with the terms of degree at most 1
degreeOnePart:=x->(
    C:=coefficients x;
    pL:=positions(flatten entries (coefficients x)_0,i->member(i,{1_(ring x)}|gens ring x));
    (((C_0)_pL)*(C_1)^pL)_(0,0))


--creates complex from which cotangent cohomology is computed out to degree n
dualCotangent=method(TypicalValue=>Complex,Options=>{Verbose=>0,Arborescent=>true})
dualCotangent (DGAlgebra,ZZ):=opts->(K,n)->(
    dL:=K.Degrees;
    if n>max apply(dL,i->i_0) then (
    	if opts#Verbose>0 then print "Warning: degree bound exceeds largest homological degree of generators.";
	n=max apply(dL,i->i_0));
    dL2:=-1*apply(dL,i->drop(i,1));
    vL:=gens K.natural; 
    gL:={0}|apply(n,i->#positions(dL,j->j_0<=i+1)); 
    R:=K.ring;
    modL:=apply(n,i->R^(dL2_(toList(gL_i..gL_(i+1)-1))));
    if opts#Verbose>0 then print "Reducing differential";
    d:=apply((flatten entries matrix K.diff)_(toList(0..gL_n-1)),x->degreeOnePart x);
    local iLa;
    local iLb;
    if opts#Verbose>0 then print "Computing differential in terms of basis";
    mapL:=apply(n-1,i->(
	    iLa=toList(gL_i..gL_(i+1)-1);
	    iLb=toList(gL_(i+1)..gL_(i+2)-1);
	    matrix {d_iLb}//matrix {vL_iLa}));
    jac:=jacobian sub(matrix {d_(toList(gL_0..gL_1-1))},R);
    firstMap:=dual map(dual modL_0,,dual jac);
    dual (chainComplex ({firstMap}|(apply(n-1,i->map(modL_i,modL_(i+1),sub(mapL_i,R))))))
    )

dualCotangent (Ideal,ZZ):=opts->(I,n)->(
    local K;
    if opts#Arborescent then (
	if opts#Verbose>0 then print "Calculating arborescent resolution";
	K=arborescentResolution(I,n,Verbose=>opts#Verbose));
    if not opts#Arborescent then (
	if opts#Verbose>0 then print "Calculating acyclic closure";
	K=acyclicClosure(koszulComplexDGA I,EndDegree=>(n-1)));
    dualCotangent(K,n,opts)
    )

--computes cotangent cohomology modules
cotangentCohomology=method(Options=>{Verbose=>0,Arborescent=>true})
cotangentCohomology (DGAlgebra,Module,ZZ) :=
cotangentCohomology (Ideal,Module,ZZ):=opts->(I,M,i)->(
    HH^i(dualCotangent(I,i+1,opts)**M))

cotangentCohomology (DGAlgebra,ZZ):=opts->(K,i)->(cotangentCohomology(K,comodule ideal zerothHomology K,i,opts))
cotangentCohomology (Ideal,ZZ):=opts->(I,i)->(cotangentCohomology(I,comodule I,i,opts))

    

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
     PARA{
     "This package is used to construct arborescent resolutions and compute cotangent cohomology groups.
      An arborescent resolution of an algebra R is a special kind of free differential graded algebra
      resolving R. See"},
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
    (arborescentResolution,ChainComplex,ZZ)
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
    F:ChainComplex
    n:ZZ
    Verbose=>ZZ
  Outputs
    A:ArborescentResolution
  Description
    Text
	Inductively constructs a truncated arborescent resolution up to homological degree @ TT "n" @. 	For @ TT "I" @ an ideal in a
	polynomial ring @ TT "R"@, @ TT "arborescentResolution(I,n)"@ constructs an arborescent resolution of @ TT "R/I" @. The resulting
	arborescent resolution is cached in @ TT "I.cache.ArborescentResolution" @.
	For more control, one may specify the underlying free resolution @ TT "F" @ of @ TT "R/I"@ by using @ TT "arborescentResolution(F,n)"@.
   Text
	Given an existing truncated arborescent resolution @ TT "B" @, @ TT "arborescentResolution(B,n)" @ extends it to homological
	degree @ TT "n" @.
   Text
       The optional input @ TT "Verbose" @ controls the level of verbosity during the computation.

   Example
      R=QQ[x_1..x_5]
      I=ideal {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_1}
      A=arborescentResolution(I,3)
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
   Example
      R=QQ[x_1..x_5]
      I=ideal {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_1}
      B=arborescentResolution(I,3)
      liftArborescentResolution(B)
///


doc ///
  Key
    ArborescentResolution
  Headline
    The class of all arborescent resolutions
  Description
    Text
      An @ TT "ArborescentResolution" @ @ TT "A"@  is a special class of @ TO DGAlgebra @ encoding a truncation of an arborescent resolution.
      The hash table @ TT "A.cache" @ contains extra information that allows one to lift the truncation to higher degree via @ TO liftArborescentResolution @.
      An @ TT "ArborescentResolution" @ is typically created via @ TO arborescentResolution @. The information of the resolution may be
      accessed using the methods applicable to any @ TO DGAlgebra @.
    Example
      R=QQ[x_1..x_5]
      I=ideal {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_1}
      B=arborescentResolution(I,3)
      C=toComplex(B,3)
      HH_0(C)
      prune HH_1(C)
      prune HH_2(C)
///



doc ///
  Key
    Arborescent
  Headline
    An optional argument for dualCotangent and cotangentCohomology
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
    C:ChainComplex
  Description
    Text
        Given an ideal @ TT "I"@ of a polynomial ring @ TT "R"@, @ TT "dualCotangent(I,n)"@ constructs a sequence @ TT "C"@ of free @TT "R"@ modules
	of length @ TT "n" @ such that after tensoring with an @ TT "R/I"@-module @ TT "M"@, the cohomology of the resulting complex up to degree @ TT "n-1" @ is
	the cotangent cohomology of @ TT "R/I" @ with coefficients in @ TT "M" @. If @ TT "Arborescent" @ is true, as is the case by
	default, this sequence is obtained from an arborescent resolution; if @ TT "Arborescent" @ is false, then an acyclic closure
	is used instead.
    Text
	If @ TT "A" @ is a @ TO DGAlgebra @ resolving @ TT "R/I" @, then @ TT "dualCotangent(A,n)" @ returns a similar sequence,
	making use of the already computed DG-algebra resolution @TT "A"@.
    Text
       The optional input @ TT "Verbose" @ controls the level of verbosity during the computation.
   Example
      R=QQ[x_1..x_5]
      I=ideal {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_1}
      C=dualCotangent(I,3)
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
	default, the computation makes use of an arborescent resolution; if @ TT "Arborescent" @ is false, then an acyclic closure
	is used instead.
    Text
	If @ TT "A" @ is a @ TO DGAlgebra @ resolving @ TT "R/I" @, then @ TT "cotangentCohomology(A,M,n)" @ also computes
	the @TT"n"@th cotangent cohomology of @TT"R/I"@ with coefficients in @TT"M"@,	making use of the already computed DG-algebra resolution @TT "A"@.
    Text
        The @TO ScriptedFunctor@ @TT"TT"@ is an abbreviation for @ TT "cotangentCohomology"@, e.g.
	@TT"TT^n(I,M)"@ returns the same as @ TT "cotangentCohomology(I,M,n)"@.
    Text
       The optional input @ TT "Verbose" @ controls the level of verbosity during the computation.
   Example
      R=QQ[x_1..x_5]
      I=ideal {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_1}
      prune TT^3(I)
      prune TT^3(I,comodule ideal {x_1,x_2*x_3,x_3*x_4,x_4*x_5})
 Caveat
       This method is only guaranteed to give correct results if @ TT "R"@ has characteristic zero.
///


TEST ///
R=QQ[x,y,z]
I=ideal {x*y,x*z,y*z}
A=arborescentResolution(I,6)
C=toComplex(A,6)
assert (all(1..5,i->HH_i(C)==0) and (HH_0(C)==comodule I))
///


TEST ///
R=QQ[x,y,z]
I=ideal {x*y,x*z,y*z}
assert ((3,0,1,3,6)==apply(1..5,i->numgens source basis TT^i(I)))
assert ((3,0,1,3,6)==apply(1..5,i->numgens source basis TT^i(I,Arborescent=>false)))
///

TEST ///
R=QQ[x,y,z]
I=ideal {x*y,x*z,y*z}
assert (2==numgens source basis TT^2(I,comodule ideal {x,y,z}))
///

TEST ///
R=QQ[x,y,z,Degrees=>entries id_(ZZ^3)]
I=ideal {x*y,x*z,y*z}
assert (degrees basis TT^3(I)=={{{-1, -1, -1}, {-1, -1, -1}, {-1, -1, -1}, {-2, 0, 0}, {0, -2, 0}, {0, 0, -2}}, {{-1, -1, -1}}})
///

TEST ///
R=QQ[x,y]
I=ideal {x*y}
A=arborescentResolution(I,6)
assert (numgens source basis TT^1(I)==1 and TT^17(I)==0)
///

TEST ///
R=QQ[x,y,z,Degrees=>entries id_(ZZ^3)]
I=ideal {x*y*z}
assert (2==numgens source basis({1,1,0},TT^0(I)))
///

