     newPackage(
             "AnalyzeSheafOnP1",
             Version => "0.1", 
             Date => "June 3, 2015",
             Authors => {{Name => "David Eisenbud", 
                       Email => "de@msri.org", 
                       HomePage => "http://www.msri.org/~de"}},
             Headline => "decompose a Sheaf on P1",
	     Keywords => {"Commutative Algebra"},
	     PackageImports => {"Varieties"},
             DebuggingMode => false
             )
     export {
	 "analyze",
	 "doubleDualMap",
	 "isNZD",
	 "showSheafOnP1",
	 "killH0"
	 }



isNZD = method()
isNZD(RingElement, Module) := (X,M) ->(ker (X*id_M) == 0)

doubleDualMap = method()
doubleDualMap Module := M ->(
    --returns the map from M to its double dual. Code is
    --adapted from the package "WeilDivisors",
    --where it is called reflexifyModuleWithMap
    S := ring M;
    h := coverMap M;
    ddh := Hom(Hom(h, S^1), S^1);
    map(target ddh, M, matrix ddh)
    )

killH0 = method()
killH0 Module := M-> M/(saturate 0_M)

analyze = method()
analyze CoherentSheaf := SN -> analyze module SN
analyze Module := M ->(
   N := M;
   S := ring N;
   if numgens S != 2 or dim S != 2 then 
        error"Ring should be a polynomial ring in 2 variables";
   kk := coefficientRing S;
   if not isField kk then error"Ring should be polynomials over a field";
   if length complete res N >1 then 
   N = killH0 N;
   Y := symbol Y;
   T := kk[Y];
   e := doubleDualMap N;
   tors := prune ker e;
   freegens := flatten degrees prune target e;
   --find a linear nonzerodivisor on tors if possible
   X := S_0;
   newVars := vars S;
   newVarsBack := vars S;
   r := 0;
   found := false;
   if not isNZD(X, tors) then (
      if isNZD(S_1,tors) then (
      found = true;
      newVars = matrix{{S_1,S_0}};
      newVarsBack = matrix{{S_1,S_0}}) else
            scan(100, i-> (
	      r = random({},kk);
	      X = S_0+r*S_1;
	      if isNZD(X,N) then (
	      found = true;
              newVars = matrix{{X,S_1}};
              newVarsBack = matrix{{S_0-r*S_1, S_1}}; break)
	       ));
      if not found then error"maybe no linear form is a nonzerodivisor; try bigger field"
      );
   changeVars := map(S,S,newVars);
   changeVarsBack := map(S, S, newVarsBack);
   dehomog := map(T,S, {1_T,Y});
   toS := map(S,T,{S_1});
   presN' := changeVars presentation tors;
   Sm1 := smithNormalForm(dehomog presN',ChangeMatrix => {false, false});
   Sm := changeVarsBack homogenize(toS Sm1, S_0);
   anns := apply(numrows Sm, i-> Sm_(i,i));
{freegens,anns,e,Sm}
)

showSheafOnP1 = method()
showSheafOnP1 CoherentSheaf := MS -> showSheafOnP1 module MS
showSheafOnP1 Module := M->(
    L := analyze M;
    <<"The double dual is free on generators of degrees:"<<endl;
    << L_0<<endl;
    <<"The annihilators of the cyclic components are:"<<endl;
     <<L_1<<endl;
    )

beginDocumentation()
     doc ///
     Key
      AnalyzeSheafOnP1
     Headline
      Describe a graded module over k[x,y] without 0-dimensional torsion 
     Description
       Text
        Any sheaf on P1 is the direct sum of line bundles--
	--twists of the structure sheaf--
	and cyclic skyscraper sheaves represented by modules of the form
	k[x,y]/(l^m)
	where l is an irreducible homogeneous polynomial and
	m is a non-negative integer.
	The routine "analyze"
	computes the twists and the annihilators l^m
	that appear in the decomposition, starting from a
	coherent sheaf on P1 or a graded module over a polynomial ring on 2 variables.
       Example
        k = ZZ/5
        S = k[a,b]
        M = S^1/ideal(a^3)++S^{-1}/(ideal b^2)++S^1/(ideal b^2)++ S^{-1,1}
    	L = analyze M;
	twists = L_0
	anns = L_1
	analyze sheaf M
     Caveat
        The script uses a linear nonzerodivisor, which would not exist over a finite
	field in the case where every point of P1 is the support of one of the
	skyscraper components.
     ///


     doc ///
     Key
      analyze
      (analyze,CoherentSheaf)
      (analyze,Module)
     Headline
      Compute the decomposition of a sheaf on P1
     Usage
      L=analyze M
     Inputs
      M:Module
      M:CoherentSheaf
     Outputs
      L:List
       a list {freegens, anns, e, D} where freegens is the list of twists, anns is the list of annihilators, e is the map from M (or its quotient by 0-dim torsion) to its double dual, and D is the Smith normal form presentation of the torsion
     Description
       Text
        The routine decomposes the sheaf associated to M 
	into a direct of twists of the structure sheaf and 
	cycle torsion part modules. It returns a list
	L ={freegens, anns, e, D} where:
	
	freegens is the list of the twists;
	
	anns is the list of annihilators;
	
	e is the map from M' to its double dual, where M' = is the result
	of reducing M mod 0-dimensional torsion, if necessary;
	
	D is a presentation of the torsion part in
	the appropriate version of Smith normal form.
	
	To compute this Smith normal form, we dehomogenize with respect to
	a linear form that is a nonzerodivisor on M', 
	use the routine smithNormalForm, and then
	rehomogenize. To find this nonzerodivisor we 
	try first the first variable, then the second, then
        up to 100 random choices
	
	The routine returns an error if the base ring is not a polynomial ring in 2
	variables over a field 
	or if after 100 tries it finds no linear form that is a nonzerodivisor on
	the module.
       Example
       	setRandomSeed 0
        S = ZZ/101[a,b]
	mm = ideal vars S
	M0 = mm^3*S^{3} ++ S^{-1};
	M1 =S^1/ideal(a^3)++S^{-1}/(ideal b^2)++S^1/(ideal b^2) ;
        M = M0++M1;
        L = analyze M0;
	freegens = L_0
	anns = L_1
	e = L_2
	D = L_3
     SeeAlso
      doubleDualMap
      showSheafOnP1
     ///

doc ///
   Key
    killH0
    (killH0,Module)
   Headline
    removes 0-dimensional torsion
   Usage
    M' = killH0 M
   Inputs
    M:Module
   Outputs
    M':Module
   Description
    Text
     "M' = M/(saturate 0_M)"
///

doc ///
   Key
    showSheafOnP1
    (showSheafOnP1, CoherentSheaf)    
    (showSheafOnP1, Module)
   Headline
    Prints the analysis of a sheaf on P1
   Usage
    showSheafOnP1 M
   Inputs
    M:Module
    M:CoherentSheaf
   Description
    Text
     prints out the twists of the line bundle summands
     and the annihilators of the (cyclic) torsion components.
   SeeAlso
    AnalyzeSheafOnP1
///

doc ///
   Key
    doubleDualMap
    (doubleDualMap, Module)
   Headline
    map from a module to its double dual
   Usage
    e = doubleDualMap M
   Inputs
    M:Module
   Outputs
    e:Matrix
     map from M to double dual
   Description
    Text
     provide the natural map M --> Hom(Hom(M,S),S), where S = ring M.
///

doc ///
   Key
    isNZD
    (isNZD, RingElement, Module)
   Headline
    tests whether a ring element is a non zerodivisor on a module
   Usage
    t = isNZD(X,M)
   Inputs
    X:RingElement
    M:Module
   Outputs
    t:Boolean
   Description
    Text
     returns true if "0 == ker (X*id_M)"
///

TEST///
setRandomSeed 0
k = ZZ/5
S = k[a,b]
M = S^1/ideal(a^3)++S^{-1}/(ideal b^2)++S^1/(ideal b^2)++ S^{-1,1}
L = analyze M
L_1 == { -2*a^3 , b^2 , b^2} --- This is not true
assert(doubleDualMap M ==
    map(Hom(Hom(M,S^1),S^1), M, matrix {{0, 0, 0, 1_S, 0}, {0, 0, 0, 0, 1_S}}))
///

TEST///
k = ZZ/101
S = k[a,b]
M = S^1/ideal(a^3)
assert(isNZD(a,M) == false)
assert(isNZD(a+b,M) == true)
///

TEST///
       	setRandomSeed 0
        S = ZZ/101[a,b]
	mm = ideal vars S
	M0 = mm^3*S^{3} ++ S^{-1};
	M1 =S^1/ideal(a^3)++S^{-1}/(ideal b^2)++S^1/(ideal b^2) ;
        M = M0++M1;
	L = analyze M0;
        assert(L === {{1,-3},{},
	map((S)^{{-1},{3}},
	image map((S)^{{3},{-1}},(S)^{{0},{0},{0},{0},{-1}},
		{{a^3, a^2*b, a*b^2, b^3, 0}, {0, 0, 0, 0, 1}}
		),
	{{0, 0, 0, 0, 1}, {a^3, a^2*b, a*b^2, b^3, 0}}),
	map((S)^0,(S)^0,0)} );
///

-- Regression for the swap branch of analyze: when S_0 is a zerodivisor on
-- the torsion but S_1 is a nonzerodivisor, the function used to error out
-- because the swap branch failed to set found = true; even after that fix,
-- changeVarsBack was not inverting the swap, so the algorithm returned the
-- annihilator in the wrong variable (b^3 instead of a^3).
TEST ///
S := ZZ/101[a,b];
M := S^1/ideal(a^3) ++ S^{-1};
L := analyze M;
assert(L_0 == {1});
assert(L_1 == {a^3});
-- the swap should be its own inverse, so the answer must live in S, in
-- the original variable a, matching the torsion ideal.
assert(ideal L_1 == ideal(a^3));
///

-- Companion check: same shape but torsion in b. Goes through the default
-- (no-swap) branch and was already working, so this guards against a
-- regression caused by the fix.
TEST ///
S := ZZ/101[a,b];
M := S^1/ideal(b^3) ++ S^{-1};
L := analyze M;
assert(L_0 == {1});
assert(L_1 == {b^3});
///

-- analyze on a pure line bundle: no torsion, anns should be empty.
-- (Note that twists are reported as internal generator degrees, so
-- S^{2,-3} -- generators in degrees -2 and 3 -- gives {-2, 3}.)
TEST ///
S := ZZ/101[a,b];
M := S^{2,-3};
L := analyze M;
assert(set L_0 === set {-2, 3});
assert(L_1 == {});
///

-- analyze CoherentSheaf form agrees with analyze Module on the
-- twists and annihilator lists.
TEST ///
S := ZZ/101[a,b];
M := S^1/ideal(b^3) ++ S^{-1};
LM := analyze M;
LS := analyze sheaf M;
assert(LM_0 == LS_0);
assert(LM_1 == LS_1);
///

-- analyze should reject rings that are not polynomial rings in 2
-- variables over a field.
TEST ///
S3 := QQ[a,b,c];
assert(try (analyze(S3^1); false) else true);
SZ := ZZ[a,b];
assert(try (analyze(SZ^1); false) else true);
///

-- L_1 in the (ZZ/5) example actually equals {1, b^2, -2*a^3*b^2} for seed 0,
-- not the value {-2*a^3, b^2, b^2} the file's first TEST originally claimed.
-- (The original bare equation was a no-op without `assert`.) This locks down
-- the real value so a future change to the algorithm doesn't silently regress.
TEST ///
setRandomSeed 0;
k := ZZ/5;
S := k[a,b];
M := S^1/ideal(a^3) ++ S^{-1}/(ideal b^2) ++ S^1/(ideal b^2) ++ S^{-1,1};
L := analyze M;
assert(L_1 == {1_S, b^2, -2*a^3*b^2});
assert(set L_0 === set {-1, 1});
///

-- killH0 direct tests: previously only invoked indirectly by analyze.
TEST ///
S := QQ[x,y];
-- a free module has no torsion, so killH0 is the identity.
assert(killH0(S^1) == S^1);
-- pure 0-dim torsion is killed completely.
assert(killH0(S^1/(ideal(x,y))) == 0);
-- 1-dim torsion (codim 1) should be preserved, since only 0-dim is killed.
assert(killH0(S^1/ideal(x^2)) == S^1/ideal(x^2));
-- mixed: free + 0-dim torsion → just the free part (after prune).
assert(prune killH0(S^1 ++ S^1/(ideal(x,y))) == S^1);
///

-- isNZD basic cases.
TEST ///
S := QQ[u,v];
-- u is a nonzerodivisor on the free module S^1.
assert(isNZD(u, S^1));
-- the zero element is never a nonzerodivisor on a nonzero module.
assert(not isNZD(0_S, S^1));
-- on S/(u), multiplication by u is the zero map (not injective).
assert(not isNZD(u, S^1/ideal(u)));
-- on S/(u), multiplication by v is injective.
assert(isNZD(v, S^1/ideal(u)));
///

-- doubleDualMap on a free module is an isomorphism;
-- on a torsion module the kernel is all of M and the target is zero.
TEST ///
S := QQ[a,b];
e := doubleDualMap(S^{-1});
assert(ker e == 0);
assert(prune coker e == 0);
-- now on a torsion module S/(a)
M := S^1/ideal(a);
e2 := doubleDualMap M;
assert(ker e2 == M);
assert(prune target e2 == 0);
///

-- showSheafOnP1 should run without error and return null.
-- (It prints to stdout; we only check it does not raise.)
TEST ///
S := ZZ/101[a,b];
M := S^1/ideal(b^3) ++ S^{-1};
-- redirect output so the test does not litter the log; pseudo-quiet via discard
ret := showSheafOnP1 M;
assert(ret === null);
ret2 := showSheafOnP1 sheaf M;
assert(ret2 === null);
///


end--
restart
uninstallPackage "AnalyzeSheafOnP1"
installPackage "AnalyzeSheafOnP1"
check "AnalyzeSheafOnP1"
viewHelp AnalyzeSheafOnP1

loadPackage("AnalyzeSheafOnP1", Reload => true)
--Analyze sheaves on P^1 -- ie graded modules without 0-dim torsion over k[x,y]

restart
loadPackage("AnalyzeSheafOnP1", Reload=>true)

S = ZZ/101[a,b]
ff =matrix"a3,b3"
R = S/ideal ff
StoR = map(R,S,vars R)
M = R^1/ideal random(R^1, R^{-2,-3,-4})
N = highSyzygy M 
loadPackage ("CompleteIntersectionResolutions", Reload=>true)
N = highSyzygy M 
analyze N;
analyze M;


use S
M = S^{0,1}++S^1/ideal"a2"++S^1/ideal"a3" ++S^1/ideal (b)
analyze M
installPackage ("CompleteIntersectionResolutions")
loadPackage ("CompleteIntersectionResolutions", Reload=>true)
viewHelp CompleteIntersectionResolutions

installPackage "WeilDivisors"
viewHelp reflexify
--
S = ZZ/101[a,b]
ff =matrix"a3,b3"
R = S/ideal ff
StoR = map(R,S,vars R)

viewHelp highSyzygy

M = R^1/ideal random(R^1, R^{-2,-3,-4})

N = highSyzygy M 
N == highSyzygy (N, Optimism =>1)
mfBound M
betti res M
betti res N
betti res coker dual presentation N
E0 = evenExtModule N
E1 = oddExtModule N
e0 = reflexifyModuleWithMap E0
e1 = reflexifyModuleWithMap E1

prune target e0
prune kernel e0
prune target e1
prune kernel e1


betti (F = res pushForward(StoR, N))
F.dd_1
