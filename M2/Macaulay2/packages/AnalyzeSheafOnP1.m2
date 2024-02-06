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
    --adapted from the package "Divisor",
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
   r := 0;
   found := false;
   if not isNZD(X, tors) then (
      if isNZD(S_1,tors) then
      newVars = matrix{{S_1,S_0}} else
            scan(100, i-> (
	      r = random({},kk);
	      X = S_0+r*S_1;
	      if isNZD(X,N) then ( 
	      found = true;
              newVars = matrix{{X,S_1}}; break)
	       ));
      if not found then error"maybe no linear form is a nonzerodivisor; try bigger field"
      );
   changeVars := map(S,S,newVars);
   changeVarsBack := map(S, S, matrix{{S_0-r*S_1, S_1}});
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
	where l is an kirreducible homogeneous polynomial and
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
       L_0 = map from M to double dual of M, L_1 is the smith normal form pres of the torsion of M
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
L_1 == { -2*a^3 , b^2 , b^2}
doubleDualMap M ==
    map(Hom(Hom(M,S^1),S^1), M, matrix {{0, 0, 0, 1_S, 0}, {0, 0, 0, 0, 1_S}})
///

TEST///
k = ZZ/101
S = k[a,b]
M = S^1/ideal(a^3)
isNZD(a,M) == false
isNZD(a+b,M) == true
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

installPackage "Divisor"
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
