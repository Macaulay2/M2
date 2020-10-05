newPackage ( "DGAlgResThree",
    Version => "0.6",
    Date => "1 October 2020",
    Authors => {
	{ Name => "Lars Winther Christensen",
	  Email => "lars.w.christensen@ttu.edu",
	  HomePage => "http://www.math.ttu.edu/~lchriste/index.html" },
      { Name => "Luigi Ferraro",
	  Email => "lferraro@ttu.edu",
	  HomePage => "http://www.math.ttu.edu/~lferraro" },
	{ Name => "Francesca Gandini",
	  Email => "fra.gandi.phd@gmail.com",
	  HomePage => "TBD" },
	{ Name => "Frank Moore",
	  Email => "moorewf@wfu.edu",
	  HomePage => "http://users.wfu.edu/moorewf/" },
	{ Name => "Oana Veliche", 
	  Email => "o.veliche@northeastern.edu",
	  HomePage => "https://web.northeastern.edu/oveliche/index.html" }
	},
    Headline => "Multiplication in free resolutions of length three",
    Reload => true,
    DebuggingMode => true
    )

export { "eeProd", "efProd", 
    "mapX", "mapY", "makeRes" , "findRegSeq" , "eeProdH",
    "eeMultTable", "efMultTable", "codimThreeAlgStructure", "codimThreeTorAlgebra", "Labels", "Compact" }

--==========================================================================
-- EXPORTED FUNCTIONS
--==========================================================================

codimThreeAlgStructure = method()

codimThreeAlgStructure(ChainComplex, List) := (F, sym) -> (
   if F.cache#?"Algebra Structure" then return F.cache#"Algebra Structure";
   if length F != 3 then
     error "Expected a chain complex of length three which is free of rank one in degree zero.";
   if #sym != 3 or any(sym, s -> (class baseName s =!= Symbol))  then
     error "Expected a list of three symbols.";
   mult := multtables(F);
   Q := ring F;
   m := numcols F.dd_1;
   l := numcols F.dd_2;
   n := numcols F.dd_3;        
   degreesP := if isHomogeneous F then 
                  flatten apply(3, j -> apply(degrees source F.dd_(j+1), d -> {j+1} | d))
	       else
	          flatten apply(3, j -> apply(degrees source F.dd_(j+1), d -> {0} | d));
   skewList := toList((0..(m-1)) | ((m+l)..(m+l+n-1)));
   e := baseName (sym#0);
   f := baseName (sym#1);
   g := baseName (sym#2);
   -- use this line if you want to ensure that 'basis' works properly on the returned ring.
   --P := first flattenRing (Q[e_1..e_m,f_1..f_l,g_1..g_n,SkewCommutative=>skewList, Degrees => degreesP, Join => false]);
   P := Q[e_1..e_m,f_1..f_l,g_1..g_n,SkewCommutative=>skewList, Degrees => degreesP, Join => false];
   phi := map(P,Q,apply(numgens Q, i -> P_(m+l+n+i)));
   eVector := matrix {apply(m, i -> P_(i))};
   fVector := matrix {apply(l, i -> P_(m+i))};
   gVector := matrix {apply(n, i -> P_(m+l+i))};
   eeGens := apply(pairs mult#0, p -> first flatten entries (P_(p#0#0-1)*P_(p#0#1-1) - fVector*(phi(p#1))));
   efGens := apply(pairs mult#1, p -> first flatten entries (P_(p#0#0-1)*P_(m+p#0#1-1) - gVector*(phi(p#1))));
   I := (ideal eeGens) +
        (ideal efGens) +
	(ideal apply(m..(m+l-1), i -> P_i))^2 +
	(ideal apply(0..(m-1), i -> P_i))*(ideal apply((m+l)..(m+l+n-1), i -> P_i)) + 
	(ideal apply(m..(m+l-1), i -> P_i))*(ideal apply((m+l)..(m+l+n-1), i -> P_i)) +
	(ideal apply((m+l)..(m+l+n-1), i -> P_i))^2;
   A := P/I;
   A.cache#"l" = l;
   A.cache#"m" = m;
   A.cache#"n" = n;
   F.cache#"Algebra Structure" = A;
   A
)

codimThreeTorAlgebra = method()

codimThreeTorAlgebra(ChainComplex,List) := (F,sym) -> (
   if F.cache#?"Tor Algebra Structure" then return F.cache#"Tor Algebra Structure";
   A := codimThreeAlgStructure(F,sym);
   P := ambient A;
   Q := ring F;
   kk := coefficientRing Q;
   PP := kk monoid P;
   I := ideal mingens sub(ideal A, PP);
   B := PP/I;
   B.cache#"l" = A.cache#"l";
   B.cache#"m" = A.cache#"m";
   B.cache#"n" = A.cache#"n";
   F.cache#"Tor Algebra Structure" = B;
   B
)

makeRes = (d1,d2,d3) -> ( 
    --d1 = matrix entries d1; 
    --d2 = matrix entries d2;
    --d3 = matrix entries d3;
    -- to build the maps correctly, what you should do is:
    -- map(R^{degrees} (target), R^{degrees} (source), 
    F := new ChainComplex; 
    F.ring = ring d1;
    F#0 = target d1; 
    F#1 = source d1; F.dd#1 = d1; 
    F#2 = source d2; F.dd#2 = d2;
    F#3 = source d3; F.dd#3 = d3; 
    F#4 = (F.ring)^{}; F.dd#4 = map(F#3,F#4,0);
    F 
    )

eeMultTable = method(Options => {Labels => true, Compact => false})

eeMultTable(Ring) := opts -> A -> (
   if not (A.cache#?"l" and A.cache#?"m" and A.cache#?"n") then
      error "Expected an algebra created with a CodimThree routine.";
   l := A.cache#"l";
   m := A.cache#"m";
   n := A.cache#"n";
   eVector := matrix {apply(m, i -> A_i)};
   if (opts.Compact) then (
       oneTimesOneA := table(m,m, (i,j) -> if i <= j then (A_i)*(A_j) else 0))
   else (
       oneTimesOneA = matrix table(m,m,(i,j) -> (A_i)*(A_j));
       );
   result := entries ((matrix {{0}} | eVector) || ((transpose eVector) | oneTimesOneA));
   if (opts.Labels) then result else oneTimesOneA
   )

efMultTable = method(Options => { Labels => true} )

efMultTable(Ring) := opts -> A -> (
   if not (A.cache#?"l" and A.cache#?"m" and A.cache#?"n") then
      error "Expected an algebra created with a CodimThree routine.";
   l := A.cache#"l";
   m := A.cache#"m";
   n := A.cache#"n";
   eVector := matrix {apply(m, i -> A_i)};
   fVector := matrix {apply(l, i -> A_(m+i))};
   oneTimesTwoA := matrix table(m,l,(i,j) -> (A_i)*(A_(m+j)));
   -- put on the row and column labels for fun
   result := matrix entries ((matrix {{0}} | fVector) || ((transpose eVector) | oneTimesTwoA));
   if (opts.Labels) then entries result else entries oneTimesTwoA
)

----------------------------------------------------------------------
-- Internal functions
----------------------------------------------------------------------

multtables = F -> (
    Q := ring F;
    d1:= matrix entries F.dd_1;
    d2:= matrix entries F.dd_2;    
    d3:= matrix entries F.dd_3;    
    m := numcols d1;
    l := numcols d2;
    n := numcols d3;
    
    EE := new MutableHashTable;
    for i from 1 to m do (
	for j from i+1 to m do (
	 a := d1_(0,i-1)*(id_(Q^m))^{j-1} - d1_(0,j-1)*(id_(Q^m))^{i-1};
    	 b := ( matrix entries transpose a ) // d2;
	 EE#(i,j) = ( matrix entries b );
	 EE#(j,i) = -EE#(i,j);
	 );
     EE#(i,i) = matrix entries map(Q^l,Q^1,(i,j) -> 0);
     );

    EF := new MutableHashTable;
    for i from 1 to m do (
	for j from 1 to l do (
    	    c := sum(1..m, k -> d2_(k-1,j-1) * (EE#(i,k)));
    	    d := d1_(0,i-1)*((id_(Q^l))_(j-1));
	    e := (matrix entries (matrix d - c)) // d3;
    	    EF#(i,j) = (matrix entries e);
	    );
	);
    {EE,EF}
    )
 

multMap = method()
multMap(Ring, ZZ, ZZ) := (A,m,n) -> (
    Abasism := basis(m,A);
    Abasisn := basis(n,A);
    AbasismPlusn := basis(m+n,A);
    
    AmTimesAn := matrix {flatten entries ((transpose Abasism) * Abasisn)};
    sub(last coefficients(AmTimesAn, Monomials=>AbasismPlusn), coefficientRing A)
)

multMap(RingElement,ZZ) := (f,m) -> (
    -- returns the matrix of left multiplication by f
    A := ring f;
    n := first degree f;
    Abasism := basis(m,A);
    AbasismPlusn := basis(m+n,A);
    fTimesAbasism := f*Abasism;
    sub(last coefficients(fTimesAbasism, Monomials=>AbasismPlusn), coefficientRing A)
)

rankMultMap = method()
rankMultMap(Ring,ZZ,ZZ) := (A,m,n) -> rank multMap(A,m,n);

homothetyMap = method()
homothetyMap(Ring,ZZ,ZZ) := (A,m,n) -> (
    Abasism := basis(m,A);
    homothetyList := apply(flatten entries Abasism, f -> transpose matrix {flatten entries multMap(f,n)});
    matrix {homothetyList}
)

multPairingDualMap = method()
multPairingDualMap(Ring,ZZ,ZZ,RingElement) := (A,m,n,w) -> (
   -- this function returns the map A_m --> A_n^*
   -- provided w is a basis of the image of the multiplication
   -- map A_m ** A_n \to A_(m+n)
    Abasism := basis(m,A);
    multMapList := apply(flatten entries Abasism, f -> multMap(f,n));
    matrix {multMapList}
)

rankHomothetyMap = method()
rankHomothetyMap(Ring,ZZ,ZZ) := (A,m,n) -> rank homothetyMap(A,m,n);

tauMaps = method()
tauMaps(Ring,ZZ,ZZ,ZZ) := (A,l,m,n) -> (
  kk := coefficientRing A;
  multMaplm := multMap(A,l,m);
  multMapmn := multMap(A,m,n);
  Al := kk^(numcols basis(l,A));
  An := kk^(numcols basis(n,A));
  lTensmn := (id_Al) ** multMapmn;
  lmTensn := multMaplm ** (id_An);
  psi := matrix {{lTensmn},{lmTensn}}; 
  {rank lTensmn + rank lmTensn - rank psi,lTensmn, lmTensn, psi}
)

torAlgebraClassCodim3 = method()
torAlgebraClassCodim3 QuotientRing := A -> (
  -- check to ensure that A is torAlgebra for codim 3 example?  
  p := rank multMap(A,1,1);
  q := rank multMap(A,1,2);
  r := rank homothetyMap(A,2,1);
  tau := first tauMaps(A,1,1,1);
  if (p >= 4 or p == 2) then
      return ("H(" | p | "," | q | ")")
  else if (p == 3) then
  (
      if (q > 1) then return ("H(" | p | "," | q | ")")
      else if (q == 1 and r != 1) then return "C(3)"
      else if (q == 0 and tau == 0) then return ("H(" | p | "," | q | ")")
      else return "T";
  )
  else if (p == 1) then
  (
      if (q != r) then return "B"
      else return ("H(" | p | "," | q | ")");
  )
  else if (p == 0) then
  (
      if (q != r) then return ("G(" | r | ")")
      else return ("H(" | p | "," | q | ")");
  );
)

torAlgebraClassCodim3 ChainComplex := F -> (
   B := codimThreeTorAlgebra(F,{getSymbol "e",getSymbol "f", getSymbol "g"});
   torAlgebraClassCodim3 B
)

makeMonic = f -> (leadCoefficient(f))^(-1)*f

performBasisChange = method()
performBasisChange(ChainComplex, List, List, List) := (F, eList, fList, gList) -> (
   Q := ring F;
   B := ring first eList;
   
   newF1 := Q^(-apply(eList, x -> drop(degree x,1)));
   newF2 := Q^(-apply(fList, x -> drop(degree x,1)));
   newF3 := Q^(-apply(gList, x -> drop(degree x,1)));

   P1 := matrix entries sub(last coefficients(matrix{eList}, Monomials=>basis(1,B)), Q);
   P2 := matrix entries sub(last coefficients(matrix{fList}, Monomials=>basis(2,B)), Q);
   P3 := matrix entries sub(last coefficients(matrix{gList}, Monomials=>basis(3,B)), Q);

   newdd1 := (F.dd#1)*map(F#1,newF1,P1);
   newdd2 := map(newF1,F#1,P1^(-1))*(F.dd#2)*map(F#2,newF2,P2);
   newdd3 := map(newF2,F#2,P2^(-1))*(F.dd#3)*map(F#3,newF3,P3);
   
   makeRes(newdd1,newdd2,newdd3)
)

poincareSeriesCodim3 = method()
poincareSeriesCodim3 QuotientRing := R -> (
   I := ideal R;
   F := res I;
   A := codimThreeTorAlgebra(F,{getSymbol "e",getSymbol "f", getSymbol "g"});
   p := rank multMap(A,1,1);
   q := rank multMap(A,1,2);
   r := rank homothetyMap(A,2,1);
   tau := first tauMaps(A,1,1,1);
   m := numcols basis(1,A);
   n := numcols basis(3,A);
   e := numcols mingens ideal vars R;
   G := ZZ[getSymbol "T",Weights=>{-1},Global=>false];
   num := (1 + G_0)^(e-1);
   den := 1 - G_0 - (m-1)*G_0^2 - (n-p)*G_0^3 + q*G_0^4 - tau*G_0^5;
   (expression num) / (expression den)
)

changeBasisT = method()
changeBasisT(ChainComplex,List) := (F,inputEs) -> (
  --- this function performs the change of basis of F required
  --- so that the multiplication table of the tor algebra computed from F
  --- is of the desired form.
  Q := ring F;
  B := ring first inputEs;
  eList := inputEs;
  annEs := ann ideal eList;
  -- tack on the annihilator of inputEs in degree 1 to the eList.
  eList = eList | flatten entries ((gens annEs)*sub(matrix basis(1,annEs),B));
  fList := flatten entries matrix {{eList#1*eList#2,eList#2*eList#0,eList#0*eList#1}};
  fList = fList | flatten entries (basis(2,B)*(mingens coker multMap(B,1,1)));
  gList := flatten entries basis(3,B);
  performBasisChange(F,eList,fList,gList)
)

-- this function tries to find an appropriate choice for e_1,e_2,e_3
-- in the classification theorem of AKM
changeBasisT(ChainComplex) := F -> (
   B := codimThreeTorAlgebra(F,{getSymbol "e",getSymbol "f", getSymbol "g"});
   kk := coefficientRing B;
   eList := select(gens B, d -> first degree d == 1);

   --- attempt to find the generators we want of the truncated exterior subalgebra
   m := #eList;
   a := getSymbol "a";
   ee := getSymbol "ee";
   D := kk[ee_1,ee_2,ee_3,SkewCommutative=>true];
   
   -- for each subset of size three of the degrees set, choose a random element
   -- until you find a set of size 3 that is isomorphic to a truncated exterior algebra
   -- for this, it suffices to check that the hom in degree two has full rank.
   genDegs := unique subsets(eList / degree, 3);
   goodGens := {};
   for genDeg in genDegs list
   (
       potentialGens := apply(genDeg, g -> makeMonic(random(g,B)));
       phi := map(B,D,potentialGens);
       phi2 := sub(last coefficients(phi basis(2,D), Monomials => basis(2,B)),kk);
       if (rank phi2 == 3 and isWellDefined phi) then 
       (
      	  goodGens = potentialGens;
      	  break;
       );
   );
   if goodGens == {} then error "Unable to find an appropriate change of basis for class T.";
   changeBasisT(F, goodGens)
)

-- this method changes basis so that ee is the distinguished element in the
-- H(p,q) classification
changeBasisHpq = method()
changeBasisHpq(ChainComplex,RingElement) := (F,ee) -> (
   Q := ring F;
   B := ring ee;
   
   multMap1 := multMap(ee,1);
   imagMultMap1 := mingens image multMap1;
   eList := flatten entries (basis(1,B)*(imagMultMap1 // multMap1));

   -- now we add in some indeterminants to track the lifts, and choose
   -- the first few es carefully so that they have trivial products among themselves
   a := getSymbol "a";
   p := #eList;
   varRing := (coefficientRing B)[a_1..a_p];
   overC := varRing monoid B;
   newI := sub(ideal B, overC);
   C := overC/newI;
   eeC := sub(ee,C);

   -- eList is a list of preimages of a basis of image of multiplication by ee
   if eList != {} then (
      -- We purturb the preimages by a multiple of ee, in the hopes of finding
      -- a set of elements that also have trivial products among themselves.
      genEList := apply(#eList, i -> sub(eList#i,C) + sub(varRing_i,C)*eeC);
      products := subsets(genEList,2) / product;
      -- these are the coefficients of the pairwise products.  The coefficients
      -- give a linear system of equations over (coefficientRing B)
      prodCoeffs := last coefficients(matrix {products}, Monomials => basis(2,C));
      soln := mingens ideal flatten entries prodCoeffs;
      -- reduce modulo the ideal of soln to make the substitution
      eList = apply(genEList, f -> sub(f % sub(soln, C), B));
   );

   -- the rest of the es are the annihilator ee (modulo ee)
   eList = eList | {ee};
   dubAnnMod := (ann ee) / (ideal ee);
   newEs := flatten entries ((gens dubAnnMod)*(matrix basis(1,dubAnnMod)));
   newEs = newEs / (f -> f % ideal {ee});
   eList = eList | newEs;
   
   -- fs are a basis of the image of the multiplication map of ee in degree 1,
   -- followed by a lift of the image of the multiplication map of ee in degree 2
   fList := flatten entries (basis(2,B)*imagMultMap1);
   multMap2 := multMap(ee,2);
   imagMultMap2 := mingens image multMap2;
   fList = fList | flatten entries (basis(2,B)*(imagMultMap2 // multMap2));
   -- complete fList to a basis in degree two now
   newFs := basis(2,B)*(matrix basis(2, (ideal basis(2,B))/(sub(ideal fList,B))));
   fList = fList | flatten entries newFs;

   -- gs are a basis of the image of the multiplication map of ee in degree 2
   -- then completed to a basis arbitrarily
   gList := flatten entries (basis(3,B)*imagMultMap2);
   gList = gList | flatten entries (basis(3,B)*(mingens coker multMap2));

   performBasisChange(F,eList,fList,gList)
)

changeBasisHpq(ChainComplex) := F -> (
   --- this function selects an element ee of degree one such that left mult by ee
   --- has full rank.  This is a generic condition, but we would like to choose
   --- it homogeneously (if grading exists)

   -- first try the es to see if any of them will work
   B := codimThreeTorAlgebra(F,{getSymbol "e",getSymbol "f", getSymbol "g"});
   p := rank multMap(B,1,1);
   eGens := apply(B.cache#"m", i -> B_i);
   goodEGens := select(eGens, ee -> rank multMap(ee,1) == p);
   if goodEGens != {} then return changeBasisHpq(F,first goodEGens);

   -- next try a random (homogeneous) element of each internal degree in homological degree 1.
   eGenDegs := eGens / degree // unique;   
   randElts := apply(eGenDegs, d -> random(d,B));
   goodRands := select(randElts, ee -> rank multMap(ee,1) == p);
   if goodRands != {} then return changeBasisHpq(F,first goodRands);
   
   << "Unable to determine a distinguished element for multiplication table." << endl;
)

-- This version finds gTop and then calls the below function to finish the rest
-- of the computation
changeBasisG = method()
changeBasisG(ChainComplex) := F -> (
   B := codimThreeTorAlgebra(F,{getSymbol "e",getSymbol "f", getSymbol "g"});
   gTop := first flatten entries (basis(3,B)*(matrix mingens image multMap(B,1,2)));
   changeBasisG(F,gTop)
)

-- This version finds a basis of es and fs so that 'gTop' is the top
-- class of the Tor algebra
changeBasisG(ChainComplex, RingElement) := (F, gTop) -> (
   B := ring gTop;
   -- complete gTop to a basis
   gList := {gTop} | flatten entries mingens ((ideal basis(3,B))/(ideal gTop));
   -- compute the matrix of the map corresponding to the dual basis element of gTop
   proj := (last coefficients(matrix{gList}, Monomials=>basis(3,B)))^{0};
   -- for each basis element, restrict codomain of mult map (from degree 1 to 3)
   -- to the 'top class', and take the dual map.
   multMapList := apply(flatten entries basis(2,B), f -> transpose (proj*multMap(f,1)));
   -- make a matrix out of all these maps.  This gives the map from
   -- M : A_2 --> Hom(gTop^*,A_1^*) \cong A_1^*
   M := matrix {multMapList};
   -- the 'other fs' are those fs which don't participate in mult
   otherFs := flatten entries (basis(2,B)*sub(gens ker M,B));
   -- the 'other es' are those es which don't participate in mult
   -- these are found using the coker of M above
   otherEs := flatten entries (basis(1,B)*sub(matrix mingens coker M,B));
   -- the good es are the duals of those elements in the image of M
   goodEs := flatten entries (basis(1,B)*sub(matrix mingens image M, B));
   -- the good fs are the lifts of these under M
   goodFs := flatten entries (basis(2,B)*sub(matrix mingens image M // M,B));
   eList := goodEs | otherEs;
   fList := goodFs | otherFs;
   -- change coordinates
   performBasisChange(F,eList,fList,gList)
)

-- This version finds gTop and then calls the below function to finish the rest
-- of the computation
changeBasisB = method()
changeBasisB(ChainComplex) := F -> (
   B := codimThreeTorAlgebra(F,{getSymbol "e",getSymbol "f", getSymbol "g"});
   gTop := first flatten entries (basis(3,B)*(matrix mingens image multMap(B,1,2)));
   changeBasisB(F,gTop)
)

-- This version finds a basis of es and fs so that 'gTop' is the top
-- class of the Tor algebra
changeBasisB(ChainComplex, RingElement) := (F, gTop) -> (
   B := ring gTop;
   kk := coefficientRing B;
   -- complete gTop to a basis
   gList := {gTop} | flatten entries mingens ((ideal basis(3,B))/(ideal gTop));
   -- compute the matrix of the map corresponding to the dual basis element of gTop
   proj := (last coefficients(matrix{gList}, Monomials=>basis(3,B)))^{0};
   -- for each basis element, restrict codomain of mult map (from degree 1 to 3)
   -- to the 'top class', and take the dual map.
   multMapList := apply(flatten entries basis(2,B), f -> transpose (proj*multMap(f,1)));
   -- make a matrix out of all these maps.  This gives the map from
   -- A_2 --> Hom(gTop^*,A_1^*) \cong A_1^*
   M := matrix {multMapList};
   -- the 'other fs' are those fs which don't participate in mult
   otherFs := flatten entries (basis(2,B)*sub(gens ker M,B));
   -- we choose f_1 and f_2 to be preimages of the image of the map A_2 --> A_1^*
   goodFs := flatten entries (basis(2,B)*sub(matrix mingens image M // M,B));
   -- the good e_1 and e_2 are the partners of the f_1 and f_2 just found
   goodEcoords := apply(goodFs, x -> (sub((last coefficients(gTop,Monomials=>basis(3,B))),kk) // multMap(x,1)));
   goodEs := flatten apply(goodEcoords, c -> flatten entries (basis(1,B)*c));
   -- complete es and fs to a basis
   otherEs := flatten entries (basis(1,B)*sub(matrix mingens coker M,B));
   f3 := goodEs#0*goodEs#1;
   otherFs = flatten entries mingens ((ideal otherFs)/(ideal f3));
   eList := goodEs | otherEs;
   fList := goodFs | {goodEs#0*goodEs#1} | otherFs;
   -- change coordinates
   performBasisChange(F,eList,fList,gList)
)

-- this function determines the class of the resolution
-- and calls the appropriate change of basis command
changeBasisCodim3 = method()
changeBasisCodim3(ChainComplex) := F -> (
   torClass := torAlgebraClassCodim3 F;
   if torClass#0 == "H" then
      changeBasisHpq F
   else if torClass#0 == "T" then
      changeBasisT F
   else if torClass#0 == "C" then
      F
   else if torClass#0 == "B" then
      changeBasisB F
   else if torClass#0 == "G" then
      changeBasisG F
)

TEST ///
Q = QQ[x,y,z];
F = res ideal (x*y, y*z, x^3, y^3-x*z^2,x^2*z,z^3);
assert ( (eeProd(F,1,2))_(0,0) == y )
assert ( (efProd(F,1,4))_(0,0) == -x )
///

end
--==========================================================================
-- end of package code
--==========================================================================

uninstallPackage "DGAlgResThree"
restart
debug loadPackage "DGAlgResThree"
check "DGAlgResThree"

-- dev space

needsPackage "TorAlgebra"'
needsPackage "PruneComplex"

Q = QQ[x,y,z];

F = res ideal (x*y, y*z, x^3, y^3-x*z^2,x^2*z,z^3);
A = codimThreeAlgStructure (F, {e,f,g})
B = codimThreeTorAlgebra (F, {e,f,g})

torAlgebraClassCodim3 F
torAlgebraClassCodim3 B
torAlgebraClassCodim3 A

eeMultTable A
eeMultTable (B, Compact => true)
describe A
---- new code 8/4/2020
restart
debug loadPackage "MultFreeResThree"
Q = ZZ/53[x,y,z];
F = res ideal (x*y, y*z, x^3, y^3-x*z^2,x^2*z,z^3);
I = ideal (random(3,Q), random(2,Q), random(2,Q), random(3,Q))
F = res I
codim I
torAlgClass(Q/I)
F = res I

A = codimThreeAlgStructure(F,{e,f,g})
B = codimThreeTorAlgebra(F,{e,f,g})
torAlgebraClassCodim3 B
netList eeMultTable B

netList entries efMultTable B
efMultTable B

rankMultMap(B,1,1)
rankMultMap(B,1,2)
multMap(f_6,1)

homothetyMap(B,2,1)

--- tau example
restart
debug loadPackage "MultFreeResThree"
Q = ZZ/3[x,y,z];
--I = ideal (x^2, y^3, z^4, x*y*z)
I = ideal (x^2, y^3, z^4, x*y)
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
torAlgebraClassCodim3(B)
poincareSeriesCodim3(Q/I)

tau = tauMaps(B,1,1,1)
X = tau#1
Y = tau#2
Z = tau#3
I = id_(source tau#1);
P = matrix {{I},{I}};
assert((X++Y)*P == Z)

mingens (image (X ++ Y) / image Z)
(X ++ Y)P = Z

0 1 0 0  0 1
0 0 0 1  0 1

q_11 = rank(A_1 \otimes A_1 \to A_2)
2*((rank A_2)(rank A_1)-q_11*(rank A_1))

V = A_1 ** A_1 ** A_1
W_1 = A_1 ** A_2
W_2 = A_2 ** A_1

V --> V \oplus V -phi-> W_1 \oplus W_2

\psi : V --> im \phi

tau = coker \psi

|X 0| |I|   |X|
|0 Y| |I| = |Y|

-- these functions take the information required to make the change of coordinates
-- and will perform the change and return the new complex.
changeBasisT(F)                 -- can determine based on class (open set)
changeBasisHpq(F)               -- can determine based on class (open set)
changeBasisGr(F)                -- can determine based on class.
changeBasisB(F)                 -- can determine based on class.
changeBasisC3(F)                -- no need to do anything

--- Example of a class T (truncated exterior algebra) ring
restart
debug loadPackage "MultFreeResThree"
kk = ZZ/32003
Q = kk[x,y,z];
I = ideal (x^2, y^3, z^4, x*y*z)
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisCodim3 F
isHomogeneous G
C = codimThreeTorAlgebra(G, {e,f,g})
netList eeMultTable(C)
netList efMultTable(C)
(net F.dd) | (net G.dd)

--- Another example of a class T ring
restart
debug loadPackage "MultFreeResThree"
kk = ZZ/32003
Q = kk[x,y,z];
I = ideal (x^3, y^3, z^4, x*y*z)
F = res I
torAlgebraClassCodim3 F
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisCodim3 F
isHomogeneous G
--G = changeBasisT(F, {e_1,e_2,e_4})
C = codimThreeTorAlgebra(G, {e,f,g})
netList eeMultTable(C)
netList efMultTable(C)

--- A `random' example of a class T ring
restart
debug loadPackage "MultFreeResThree"
kk = ZZ/32003
Q = kk[x,y,z];
I = ideal (random(3,Q), random(3,Q), random(3,Q), random(6,Q))
F = res I
torAlgebraClassCodim3 F
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisCodim3 F
isHomogeneous G
--G = changeBasisT(F, {e_1,e_2,e_4})
C = codimThreeTorAlgebra(G, {e,f,g})
netList eeMultTable(C)
netList efMultTable(C)

--- Another example of a class T ring
restart
debug loadPackage "MultFreeResThree"
kk = ZZ/32003
Q = kk[x,y,z];
I = ideal (x^3, y^3, z^3, x*y*z)
F = res I
torAlgebraClassCodim3 F
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisCodim3 F
isHomogeneous G
--G = changeBasisT(F, {e_1,e_2,e_4})
C = codimThreeTorAlgebra(G, {e,f,g})
netList eeMultTable(C)
netList efMultTable(C)

--- H(3,2) example  (H - hypersurface)
restart
debug loadPackage "MultFreeResThree"
Q = ZZ/53[x,y,z];
I = ideal (x^2, y^3, z^4, x*y)
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable(B)
--- can specify the distinguished element
G = changeBasisHpq(F,e_4)
--- or try to find one automatically
G = changeBasisHpq(F)
isHomogeneous G
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable(C)
netList efMultTable(C)

p = rank (B_1 \otimes B_1 --> B_2)
q = rank (B_1 \otimes B_2 --> B_3)

--- playing around with trying to find a distinguished element on a random example
--- for class H(3,2)
restart
debug loadPackage "MultFreeResThree"
kk = ZZ/32003
Q = kk[x,y,z]
I = ideal (random(3,Q), random(2,Q), random(2,Q), random(3,Q))
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisCodim3(F) 
G = changeBasisHpq(F,e_1)
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable C
netList efMultTable C

-- this is a change of coords away from example sent on 8/8
restart
debug loadPackage "MultFreeResThree"
kk = ZZ/32003
Q = kk[x,y,z]
I = ideal (x^5 + y^5, x^5 - y^5, x*y^2*z^2 + x^5, x^2*y^2*z, x^3*y^2-y^5, x^4*y, z^3)
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisHpq(F,e_1)
G = changeBasisHpq(F)
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable C
netList efMultTable C

--- example from Lars -- class H(6,5)
restart
debug loadPackage "MultFreeResThree"
Q = ZZ/53[x,y,z]
I = ideal (x^5, y^5, x*y^4, x^2*y^3, x^3*y^2, x^4*y, z^3)
F = res I
A = codimThreeAlgStructure(F,{e,f,g})
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
torAlgebraClassCodim3 F
torAlgebraClassCodim3 A
torAlgebraClassCodim3 B
G = changeBasisCodim3(F)
G = changeBasisHpq(F,e_1)
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable(C)
netList efMultTable(C)

--- change of basis working space -- class H(0,0)
--- this one is Golod, so hard to tell if it is really 'working'
--- but good to have this case anyway.
restart
debug loadPackage "MultFreeResThree"
Q = QQ[x,y,z]
I = ideal(x^2,x*y^2)*ideal(y*z,x*z,z^2)  
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisCodim3(F)
G = changeBasisHpq(F,e_1)
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable(C)
netList efMultTable(C)

--- change of basis working space G(2) (G - Gorenstein)
restart
debug loadPackage "MultFreeResThree"
kk = ZZ/32003
Q = kk[x,y,z]
I = ideal(x*y^2,x*y*z,y*z^2,x^4-y^3*z,x*z^3-y^4)
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisCodim3(F)
G = changeBasisG(F)
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable C
netList efMultTable C

--- 'generic' G example
restart
debug loadPackage "MultFreeResThree"
kk = ZZ/32003
Q = kk[x,y,z]
I = ideal fromDual matrix{{random(4,Q),random(9,Q)}};
F = res I
torAlgebraClassCodim3 F
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisCodim3(F)
G = changeBasisG(F)
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable C
netList efMultTable C

--- change of basis for class B (B - Brown)
restart
debug loadPackage "MultFreeResThree"
kk = ZZ/32003
Q = kk[x,y,z]
I = ideal(x^2,x*y,z^2,y*z)
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisCodim3 F
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable C
netList efMultTable C

--- 'generic' B example
restart
debug loadPackage "MultFreeResThree"
kk = ZZ/32003
Q = kk[x,y,z]
I = ideal fromDual matrix{{random(2,Q),random(3,Q)}};
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisB F
G = changeBasisCodim3 F
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable C
netList efMultTable C

---- another working area
gmt = genmulttables F

use A
netList table (m,m,(i,j) -> if j > i then e_(i+1)*e_(j+1) else 0)
netList table (m,m,(i,j) -> if i < j then e_(i+1)*e_(j+1) else (if i == j then 0 else "-"))
netList table (m,l,(i,j) -> e_(i+1)*f_(j+1))

netList table (m+1,m+1,(i,j) -> if i == 0 and j == 0 then " " else (if i == 0 then e_j else (if j == 0 then e_i else ( if i < j then e_i*e_j else (if i == j then 0 else "-")))))

netList ( {toList(e_1..e_m)} | table (m,m,(i,j) -> if i < j then e_(i+1)*e_(j+1) else (if i == j then 0 else "-")) )
netList table (m,l,(i,j) -> e_(i+1)*f_(j+1))


--==========================================================================
-- DOCUMENTATION
--==========================================================================

beginDocumentation()

doc ///
  Key
    TorAlgebra
  Headline
    Classification of local rings based on multiplication in homology
  Description

    Text 
      Let $I$ be an ideal of a regular local ring $Q$ with residue
      field $k$. The minimal free resolution of $R=Q/I$ carries a
      structure of a differential graded algebra. If the length of the
      resolution, which is called the codepth of $R$, is at most $3$,
      then the induced algebra structure on Tor$_Q*$ ($R,k$) is unique
      and provides for a classification of such local rings.
      
      The package also recognizes Golod rings, Gorenstein rings, and
      complete intersection rings of any codepth. To recognize Golod
      rings the package implements a test found in J. Burke, {\it Higher
      homotopies and Golod rings}
      @HREF"https://arxiv.org/abs/1508.03782"@. ///


doc ///
  Key
    torAlgData
  Headline
    invariants of a local ring and its class (w.r.t. multiplication in homology)
  Usage
    torAlgData R
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra  by an ideal contained in the irrelevant maximal ideal
  Outputs
      : HashTable
        a hash table with invariants of the local ring obtained by
  	localizing {\tt R} at the irrelevant maximal ideal
  Description
  
    Text 
      Computes invariants of the local ring obtained by localizing
      {\tt R} at the irrelevant maximal ideal and, provided that it
      has codepth at most 3, classifies it as belonging to one of the
      (parametrized) classes {\bf B}, {\bf C}(c), {\bf G}(r), {\bf
      H}(p,q), {\bf S}, or {\bf T}. Rings of higher codepth are
      classified as {\bf C}(c) (complete intersection), {\bf Gorenstein},
      {\bf Golod}, or {\tt no class}. Gorenstein rings of codepth 4 are further
      classified as belonging to one of the (parametrized) classes
      {\bf C}(4), {\bf GS}, {\bf GT}, or {\bf GH}(p). 
      
      Returns a hash table with the following data of the local ring:
  
      "c": codepth
      
      "e": embedding dimension
      
      "h": Cohen-Macaulay defect
      
      "m": minimal number of generators of defining ideal
      
      "n": type
      
      "Class": class ('B', 'C', 'G', 'GH', 'GS', 'GT', 'H', 'S', 'T',
      'Golod', 'Gorenstein' `zero ring', or 'no class')
      
      "p": classification parameter
      
      "q": classification parameter
      
      "r": classification parameter
      
      "isCI": boolean
      
      "isGorenstein": boolean
      
      "isGolod": boolean
      
      "PoincareSeries": Poincar\'e series in closed from (rational function)
      
      "BassSeries": Bass series in closed from (rational function)
      
    Example
      Q = QQ[x,y,z];
      data = torAlgData (Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3))
      data#"PoincareSeries"

    Example
      Q = QQ[w,x,y,z];
      torAlgData (Q/ideal (w^2-x*y*z,x^3,y^3,x^2*z,y^2*z,z^3-x*y*w,x*z*w,y*z*w,z^2*w-x^2*y^2))

    Example
      Q = QQ[v,w,x,y,z];
      torAlgData (Q/(ideal(v^2-w^3)*ideal(v,w,x,y,z)))

    Example
      Q = QQ[u,v,w,x,y,z];
      torAlgData (Q/ideal (u^2,v^2,w^2-y^4,x^2,x*y^15))

    Text  
      To extract data from the hash table returned by the function one may use  
      @TO torAlgDataList@ and @TO torAlgDataPrint@.
       
  Caveat
      If the embedding dimension of {\tt R} is large, then the response time
      may be longer, in particular if {\tt R} is a quotient of a polynomial
      algebra over a small field. The reason is that the function attempts to
      reduce {\tt R} modulo a generic regular sequence of generators of the irrelevant
      maximal ideal. The total number of attempts made can be controlled with
      @TO setAttemptsAtGenericReduction@.
      
      If {\tt R} is a quotient of a polynomial algebra by a
      homogeneous ideal, then it is graded and the relevant invariants
      of the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal can be determined directly from {\tt R}.
      If {\tt R} is a quotient of a polynomial algebra by a
      non-homogeneous ideal, then the function uses the package @TO
      LocalRings@ to compute some of the invariants.
///


--===================================================================================================
-- TESTS
--===================================================================================================

-- #0 zero ring, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( promote(1,Q) )
assert( torAlgClass(Q/I) === "zero ring" )
///
