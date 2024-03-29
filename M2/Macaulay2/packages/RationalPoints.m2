--========================================================

-- licensed under GPL v2 or any later version

newPackage(
     "RationalPoints",
     Version => "0.95",
     Date => "Aug 21, 2009",
     Authors => {
	  {Name => "Nathaniel Stapleton", Email => "nat.j.stapleton@gmail.com"}
	  },
     Headline => "find the common zeroes of a set of polynomials with coefficients in a finite field",
     Keywords => {"Commutative Algebra"},
     DebuggingMode => false
     )

--==========================================================
--This program takes in an ideal generated by a list of polynomials with coefficients in a finite field.
--It finds all of the common zeroes of the polynomials (over the coefficient field) and returns them.
--It does this by intelligently testing points in affine n-space.
--Stage 1: Preparation: The generators of the inputted ideal are sorted according to minimum index of support
--         As some polynomials do not contain all of the variables some points do not need to be tested.
--         These points are avoided as much as possible. Instructions for avoiding these points and how to
--         add in the necessary points for the successive polynomial are computed and stored in the 
--         Preparation stage.
--Stage 2: Evaluation: After preparation the polynomials are iterated through. In each case the data stored
--         from the preparation process is used to generate potential zeroes and the polynomial is evaluated
--         at these points. There are two possible ways this is done, in most cases one variable is left
--         free in the polynomial so that evaluation provides a one variable polynomial which is then 
--         factored. However if the polynomials minimal variable has already been tested then the polynomial
--         is just evaluated at the points that have been found to be zeroes of the previous polynomials.
--         In this way at the end of the process only points that are zeroes of all of the polynomials are
--         left behind.
-------------------------------------------------------------
--Variable names for the interested reader:
--     I - the ideal whose generators the program finds the common zeroes of
--     A - the ring I is contained in
--     k - the coefficient field of A
--     Igens - the generators of I that the program works with
--     els - a list of the elements of the field k
--     theinfo - a list containing several pieces of information related to how potential zeroes should be tested
--     zips - a list containing zeroes, by the end it will contain all of them
--     evaluationspace - a list of potential zeroes (although not necessarily whole points of affine n-space)

export{"rationalPoints", "UseGB", "UseMinGens", "SortGens", "LowMem", "Amount"}

--This function shuffles two lists of lists naively.
--e.g. combineLists({{a},{b}},{{0,1},{2}}) == {{a,0,1},{a,2},{b,0,1},{b,2}}
combineLists = (k,l) -> (
     return flatten apply(k, i->apply(l, j-> i|j));
     );

--This function takes in a finite field and returns a list of the elements of the field. It can be thought of
--as the underlying set.
fieldElements = (k) -> (
     J := ideal k;
     p := char k;
     els := {};
     galoisfield := class k === GaloisField;     	       
     if galoisfield then (
	  x := k.PrimitiveElement; --sometimes k_0 is not the primitive element ie. GF 9
	  e := 1;
	  b := 0;
	  els = els|{0};
	  while b != 1 do (
	       b = x^e;
	       e = e+1;
	       els = els | {b};
	       );
	  );
     if not galoisfield and char ring J != 0 then ( 
     	  d := (degree((flatten entries gens J)_0))_0;
     	  a := (gens k)_0;
          coeffs := toList ((set toList (0..p-1)) ^** (d));
     	  for i to # coeffs - 1 do (
	       x := 0;
	       for j to d-1 do (
	       	    x = x+coeffs_i_j*a^j;
	       	    );
	       els = els | {x};
	       );
	  );
     if not galoisfield and char ring J == 0 then els = toList (0..p-1);     
     return els;
     );

--orderIt orders a list polynomials from highest minimum index of the support to lowest
orderIt = (l,n) -> (
     p := {};
     for i from 0 to n-1 do (
     	  for j to #l-1 do (
     	       if index max support l_j == n-i-1 then (
		    p = p|{l_j};
		    );
	       );
	  );
     return p;
     );	  

--shuffleSet is called by superPrep, it provides the instructions for how superCombine should work for a
--given polynomial
shuffleSet = (place, a, z) -> (
     x := 0;
     y := 0;
     result := {};
     for i from a to z do (
	  if member(i,place) then (
	       result = result | {(0,x)};
	       x = x+1;
	       ) 
	  else ( 
	       result = result | {(1,y)};
	       );
	  y = y+1;
	  );
     return result;
     );

--sortGens is the function called when the SortGens option is set to true. It applies an automorphism of A to 
--each element of Igens. The automorphism just permutes the variables of A. The purpose is to cause the fewest
--number of points to be tested. Besides applying the automorphism to Igens it returns the recipe for the inverse
--automorphism which is applied to each of the zeroes at the very end of the computation.
sortGens = (Igens, A) -> (
     s := {};
     s = sort apply(gens A, i -> {sum apply(Igens, j -> if isSubset({i}, support j) then 1 else 0),i});
     perm := apply(s, i -> i#1);
     f := map(A,A,perm);
     Igensperm := apply(Igens, i -> f i);
     l := apply(perm, i -> index i);    
     return (Igensperm, l);
     )

--superPrep returns two pieces of information, the first is the instructions for which variable to test
--and the second is the variables actually showing up in polynomials (certainly we do not want to test
--points for variables that don't even show up)
superPrep = (Igens, z) -> (
     sup := apply(Igens, i -> apply(support i, j -> index j)); --maybe call sup supportofpolys
     usedvars := set sup_0;
     sup2 := {set sup_0};
     place := shuffleSet(toList (set sup_0 - {first sup_0}), min sup_0 + 1, z); --apply(toList (set sup_0 - {first sup_0}), j -> j - min sup_0 - 1);
     theinfo := {{place,0, min sup_0, # sup_0 - 1}};
     --sup2_i contains the support of the ith poly that is relevant when creating an list of points to be evaluated at.
     for i from 1 to #sup-1 do (
	  if min sup_i < min sup_(i-1) then sup2 = sup2|{set sup_i - {first sup_i}} else sup2 = sup2|{set sup_i};
      	  );	    
     for i from 1 to #sup - 1 do ( --perhaps start this at 1? how about doing sup2 in it's own loop first.
    	  if isSubset(sup2_i,usedvars) then ( --should be sup_i, used to be sup2_i
	       theinfo = theinfo|{{{},1,min sup_i,0}}; 
	       )
	  else if toList (sup2_i*(usedvars)) == {} then ( --should this be intersecting with usedvars? said set sup_i in place of usedvars
	       place = shuffleSet(sup2_i, min sup_i + 1, z);
	       theinfo = theinfo|{{place,2,min sup_i,#sup2_i}}; 
	       ) 
	  else (
	       if min toList sup_i == min toList usedvars then ( 
		    place = shuffleSet(toList sup2_i - usedvars, min sup_i, z); 
		    )
	       else ( 
		    place = shuffleSet(toList sup2_i - usedvars, min sup_i + 1, z); 
		    );
	       theinfo = theinfo|{{place,0, min sup_i,# (toList sup2_i - usedvars)}};
	       );
	  usedvars = usedvars + set sup_i;
    	  );
     return (theinfo, usedvars);
     );

--superCombine is the function that needs to be fastest. It takes in two lists of lists and shuffles them
--together based on a recipe (contained in the variable theinfosubi).
superCombine = (theinfosubi, zips, els) -> (
     scheme := theinfosubi_0;
     case := theinfosubi_1;
     n := theinfosubi_3;
     if case == 1 then return zips;
     newstuff := apply(toList (set els)^**n, deepSplice);
     if n == 1 and versionTest() then newstuff = apply(newstuff, i -> {i}); --take out in newest version of M2
     return flatten apply(newstuff, k -> apply(zips, l -> apply(scheme, (i,j) -> {k,l}#i#j))); 
     );

newvar := symbol newvar

--zeroesByFactoring takes in a polynomial and an evaluationspace that does not include one of the variables in the
--polynomial so that the evaluation homomorphism takes values in single variable polynomials which are then factored.
zeroesByFactoring = (thepoly, evaluationspace, A, k, els, numdeadvars) -> (
     deadvars := apply(numdeadvars, i->0);
     B := k[newvar];
     zeroes := {};
     memfact := memoize factor;
     for i to # evaluationspace - 1 do ( 
	  ev := map(B,A,deadvars|{newvar}|(evaluationspace#i));
	  evk := map (k,B,{0});
	  q := {};
	  if isConstant ev thepoly then (
	       if ev thepoly == 0 then (
		    q = apply(els,j -> {j});
		    )
	       else q = {};
	       ); 
	  if not isConstant ev thepoly then (
	       p := toList memfact ev thepoly;
	       p = apply(p,j->(toList j)_0);
	       for j to #p-1 do (
		    if (degree p#j)_0 == 1 then q = q|{p#j};	       --if p_i == 0 ... else ...	    
		    );	   
	       q = apply(q,j -> { -evk j }); --now I've got the zeroes of thepoly
	       );
	  if evaluationspace_0 === {} then zeroes = q;
	  if evaluationspace_0 =!= {} then zeroes = zeroes | combineLists(q,{evaluationspace#i});
	  );
     return zeroes;
     );
 
--zeroesByEvaluation evaluates a polynomial on the evaluationspace, a list of possible zeroes.
zeroesByEvaluation = (thepoly, evaluationspace, A, k, numdeadvars) -> (
     deadvars := apply(numdeadvars, i->0);
     return flatten apply(evaluationspace, i -> (if (map(k,A,deadvars|i)) thepoly == 0 then {i} else {}));
     );
 
findPoints = (Igens, theinfo, els, A, k, m) -> (
     z := m - index max support Igens_0 - 1;
     zips := {apply(z, i -> 0)};
     dif := {};
     evaluationspace := superCombine(theinfo_0,zips,els);
     zips = zeroesByFactoring(Igens_0, evaluationspace, A, k, els, theinfo_0_2);
     temporaryzips := {};
     for i from 1 to #Igens - 1 do (     
     	  thepoly := Igens_i;
	  case := (theinfo_i)_1;
	  dif = index max support Igens_(i-1) - index max support thepoly - 2;
	  extra := apply(toList (0..dif), i -> 0);
	  zips = apply(zips, j -> extra|j);
	  evaluationspace = superCombine(theinfo_i, zips, els);
	  if max support thepoly == max support Igens_(i-1) then ( --shouldn't this use case == 0 or 1 or something
	       temporaryzips = zeroesByEvaluation(thepoly, evaluationspace, A, k, (theinfo_i)_2) 
	       ) else temporaryzips = zeroesByFactoring(thepoly, evaluationspace, A, k, els, (theinfo_i)_2);
--	  if case == 2 then combineLists( -- I need to check and see if this is gonna be worth it)
	  zips = temporaryzips;
	  );
     return zips;
     );

lowMemIterate = (thelist, numels) -> (
     thelist#0 = thelist#0+1;
     if thelist#0 == numels then (
	  thelist#0 = 0;
	  flag := 1;
	  i := 1;
	  while flag == 1 do (
	       thelist#i = thelist#i+1;
	       if thelist#i == numels then thelist#i = 0 else flag = 0;
	       i = i+1;
	       )
	  );
     return thelist;
     );

lowMemPoints = (Igens, els, A, k) -> (
     loop := # els - 1;
     stop := #Igens-1;
     endofloop := (loop+1)^(# gens A);
     i := 0;
     evaluationat := new MutableList from apply(#gens A,i -> 0);
     evaluationat#0 = -1;
     zeroes := {};
     nonzeroflag := 0;
     while i < endofloop do (
	  lowMemIterate(evaluationat, loop+1);
	  for p to stop do (
	       if (map(k,A,els_(toList evaluationat))) Igens_p != 0 then (
		    nonzeroflag = 1;
		    break;
		    )
	       );
     	  if nonzeroflag == 0 then zeroes = zeroes|{els_(toList evaluationat)};
	  i = i+1; 
	  nonzeroflag = 0;
	  );		    
     return zeroes;
     );

--this tests for a bug in versions of M2 at least less than or equal to 1.2
versionTest = () -> (
     return (toList (set {1})^**1)_0 === 1;
     );

rationalPoints = method(Options => {Verbose => false, UseGB => false, UseMinGens => false, SortGens => false, LowMem => false, Amount => false})     
rationalPoints(Ideal) := opts -> Iprime -> (
     -- Initializing the variables. We change the ring so that it has the Lex ordering.
     Aprime := ring Iprime; 
     if not isPolynomialRing Aprime then break; --basic sanity check
     k := coefficientRing Aprime;
     v := gens Aprime;
     m := #v;
     J := ideal k;
     A := k (monoid [gens Aprime,MonomialOrder => Lex]);
     f := map(A,Aprime,gens A);
     I := f Iprime;
     perm := {};
     Igens := {};
     if dim k != 0 or char k == 0 or numgens J > 1  then break; --basic sanity check 2
     els := {};
     els = fieldElements(k);
     if opts.UseGB then Igens = flatten entries gens gb I 
     else if opts.UseMinGens then Igens = flatten entries mingens I
     else Igens = flatten entries gens I;
     if Igens == {0} then Igens = {};
     Igens = orderIt(Igens, m);
     if opts.SortGens then (Igens, perm) = sortGens(Igens,A);
     zips := {{}};
     if opts.Verbose then print Igens;
     if Igens === {} then (
	  if opts.Amount then return (#els)^m;
	  zips = toList (set els)^**m;
	  if m == 1 and versionTest() then zips = apply(zips,i -> {i}); --take away this line for new versions
	  zips = apply(zips,i -> toList i);
	  return zips;
	  );
     if opts.LowMem or class k === GaloisField then {
	  if opts.Amount then return # lowMemPoints(Igens, els, A, k);
	  if not opts.Amount then return lowMemPoints(Igens, els, A, k);
	  };
     --note: as soon as polynomials over GF's can be factored the "or" here should be taken away
     unusedvars := {};
     (theinfo, usedvars) := superPrep(Igens,m-1);
     zips = findPoints(Igens, theinfo, els, A, k, m);
     unusedvars = set (0..m-1) - usedvars;
     if opts.Amount then return #zips*(#els)^(#unusedvars);
     theinfosubfinal := {};
     theinfosubfinal = {shuffleSet(unusedvars,0,m-1),0,0,#unusedvars};
     zips = superCombine(theinfosubfinal, zips, els);
     if opts.SortGens then zips = apply(zips, i -> i_perm);
     return zips;
     );	  


--=================================================

beginDocumentation()

----------------------------------------------------

document {
     Key => RationalPoints,
     Headline => "Find the rational points of an affine variety defined over a finite field",
     EM "RationalPoints", " is a package for computing the rational points of an affine variety."
     }

document {
     Key => {rationalPoints,
	  (rationalPoints, Ideal),
	  UseGB,
	  UseMinGens,
	  SortGens,
	  LowMem,
	  Amount,
	  [rationalPoints,UseGB],
	  [rationalPoints,UseMinGens],
	  [rationalPoints,SortGens],
	  [rationalPoints,LowMem],
	  [rationalPoints,Amount],
	  [rationalPoints,Verbose]
	  },
     Headline => "Compute all of the rational points of an affine variety",
     Usage => "l = rationalPoints I",
     Inputs => {
	  "I" => null => {"which is ", ofClass Ideal, " contained in a polynomial ring over a finite field."},
	  UseGB => Boolean => " turns on and off a Groebner basis computation of the ideal. Default is false.",
	  UseMinGens => Boolean => " turns on and off a mingens computation of the ideal that may change the chosen generators.",
	  SortGens => Boolean => " sorts generators in order to make searching for zeroes more efficient.",
	  LowMem => Boolean => " uses an alternative algorithm that is slower but much less memory intensive.",
	  Amount => Boolean => " output changes to the number of zeroes.",	  
	  Verbose => Boolean => "output includes the generators of the ideal that the computation uses. These may be modified by UseGB or UseMinGens",
	  },
     Outputs => {
	  "l" => List => { "a list of lists. Each internal list is an n-tuple of elements of the finite field such that the n-tuple represents a point  
	       in Affine n-space lying on the variety defined by the input ideal ", TT "I", ".",},
     	  },
     EXAMPLE lines ///
     R = ZZ/5[x_1..x_4];
     I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);
     p = rationalPoints I
     ///,
     PARA {
	  "This symbol is provided by the package ", TO RationalPoints, "."
	  }
}
     TEST ///
     R = ZZ/13[e];
     I = ideal(e-2);
     assert(# rationalPoints I == 1)
     R = ZZ/5[x_1..x_4];
     I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);
     assert(# rationalPoints I == 8)
     k = GF 9;
     R = k[m,n,j];
     I = ideal(m+a, n*j);
     assert(rationalPoints(I,Amount => true) == 17)
     R = ZZ/13[b,c,d];
     I = ideal (b-b);
     assert(rationalPoints(I,Amount => true) == 2197)     
     assert(#rationalPoints Grassmannian(1,3,CoefficientRing=>ZZ/2) == 36);
     ///
endPackage "RationalPoints"

--=========================================================================--
