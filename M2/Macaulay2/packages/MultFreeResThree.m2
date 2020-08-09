newPackage ( "MultFreeResThree",
    Version => "0.5",
    Date => "3 June 2020",
    Authors => {
	{ Name => "Lars Winther Christensen",
	  Email => "lars.w.christensen@ttu.edu",
	  HomePage => "http://www.math.ttu.edu/~lchriste/index.html" },
      { Name => "Luigi Ferraro",
	  Email => "ferrarl@wfu.edu",
	  HomePage => "http://users.wfu.edu/ferrarl/" },
	{ Name => "Francesca Gandini",
	  Email => "fra.gandi.phd@gmail.com",
	  HomePage => "TBD" },
	{ Name => "Oana Veliche", 
	  Email => "o.veliche@northeastern.edu",
	  HomePage => "https://cos.northeastern.edu/faculty/oana-veliche/" }
	},
    Headline => "Multiplication in free resolution of length three",
    -- Certification => { -- this package was certified under its old name, "CodepthThree"
    -- 	 "journal name" => "The Journal of Software for Algebra and Geometry",
    -- 	 "journal URI" => "http://j-sag.org/",
    -- 	 "article title" => "Local rings of embedding codepth 3: A classification algorithm",
    -- 	 "acceptance date" => "2014-07-11",
    --      "published article DOI" => "http://dx.doi.org/10.2140/jsag.2014.6.1",
    -- 	 "published article URI" => "http://msp.org/jsag/2014/6-1/jsag-v6-n1-p01-s.pdf",
    -- 	 "published code URI" => "http://msp.org/jsag/2014/6-1/jsag-v6-n1-x01-code.zip",
    -- 	 "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/CodepthThree.m2",
    -- 	 "release at publication" => "4b2e83cd591e7dca954bc0dd9badbb23f61595c0",
    -- 	 "version at publication" => "1.0",
    -- 	 "volume number" => "6",
    -- 	 "volume URI" => "http://msp.org/jsag/2014/6-1/"
    -- 	 }
    Reload => true,
    DebuggingMode => true
    )

export { "genmulttables", "multTables", "multTablesLink", "eeProd", "efProd", 
    "mapX", "mapY", "makeRes" , "findRegSeq" , "eeProdH",
    "eeMultTable", "efMultTable", "codimThreeAlgStructure", "codimThreeTorAlgebra", "Labels", "Compact" }

-- exportMutable{ "u" }
--==========================================================================
-- EXPORTED FUNCTIONS
--==========================================================================

----------------------------------------------------------------------------
-- Implementation of the classification algorithm
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- torAlgData

-- R a quotient of a polynomial algebra

-- Returns a hash table with the following data of the local ring
-- obtained by localizing R at the irrelevant maximal ideal:

-- c: codepth
-- e: embedding dimension
-- h: Cohen-Macaulay defect
-- m: minimal number of generators the defining ideal
-- n: type
-- Class: class (B ,C ,G ,GS, GT, GH, H ,S, T) in the classification due to 
--   Weyman and to Avramov Kustin and Miller; see [L.L. Avramov, 
--   A cohomological study of local rings of embedding codepth 3, 
--   J. Pure Appl. Algebra, 216, 2489--2506 (2012)] and [L.L. Avramov, Homological
--   asymptotics  of modules over local rings, Commutative algebra (Berkeley, CA, 1987), 
--   Math. Sci. Res. Inst.  Publ., vol. 15, Springer, New York, 1989, pp.~33--62] 
--   for an overview
-- p: classification parameter
-- q: classification parameter
-- r: classification parameter
-- isCI: boolean, TRUE if R is complete intersection, otherwise FALSE
-- isGolod: boolean, TRUE if R is Golod, otherwise FALSE
-- isGorenstein: boolean, TRUE if R is Gorenstein, otherwise FALSE
-- PoincareSeries: rational function, Poincare series in closed form
-- BassSeries: rational function, Bass series in closed form

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

genmulttables = F -> (
    P := ring F;
    m := numcols F.dd_1;
    l := numcols F.dd_2;
    n := numcols F.dd_3;
    uIndices := (subsets(toList(1..m),2) ** toList(1..n)) / flatten;
    u := getSymbol("u");
    uIndexHash := hashTable apply(#uIndices, i -> uIndices#i => i);
    Q := P[for l in uIndices list u_l];    
    F = F**Q;
    d1:= matrix entries F.dd_1;
    d2:= matrix entries F.dd_2;    
    d3:= matrix entries F.dd_3;    
    
    EE := new MutableHashTable;
    for i from 1 to m do (
	for j from i+1 to m do (
	 a := d1_(0,i-1)*(id_(Q^m))^{j-1} - d1_(0,j-1)*(id_(Q^m))^{i-1};
    	 b := ( matrix entries transpose a ) // d2;
	 us := transpose matrix {apply(n, k -> Q_(uIndexHash#{i,j,k+1}))};
	 EE#(i,j) = ( d3*us + matrix entries b );
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

multtableslink = (F,i,j,k) -> (
    Q := ring F;
    d1:= matrix entries F.dd_1;
    d2:= matrix entries F.dd_2;    
    d3:= matrix entries F.dd_3;    
    m := numcols d1;
    l := numcols d2;
    n := numcols d3;

    mtable := multtables F;
    EE := mtable#0;
    EF := mtable#1;
        
    EEE := new MutableHashTable;
    for i from 1 to m do (
	for j from i+1 to m do (
	    for k from j+1 to m do(
    	    c := sum(1..l, s -> (EE#(i,j))_(s-1,0)*(EF#(k,s)));
    	    EEE#(i,j,k) = (matrix entries c);
	    EEE#(j,i,k) = -EEE#(i,j,k);
	    EEE#(i,k,j) = -EEE#(i,j,k);
	    EEE#(k,j,i) = -EEE#(i,j,k);
	    EEE#(j,k,i) = EEE#(i,j,k);
	    EEE#(k,i,j) = EEE#(i,j,k);
	    );
	);
    );
    for i from 1 to m do(
	for j from 1 to m do(
	    EEE#(i,i,j) = matrix entries map(Q^n,Q^1,(i,j) -> 0);
	    EEE#(i,j,i) = matrix entries map(Q^n,Q^1,(i,j) -> 0);
	    EEE#(j,i,i) = matrix entries map(Q^n,Q^1,(i,j) -> 0);
	   );
     );
 
    X := new MutableHashTable;
    d3t := matrix entries(transpose(d3));
    for s from 1 to n do (
    	X#(s,s) = matrix entries (0*(id_(Q^l))_{0});
	for t from s+1 to n do (
	    X#(s,t) = matrix entries (matrix{ {-(EEE#(i,j,k))_(t-1,0)},{(EEE#(i,j,k))_(s-1,0)}} // d3t);
	    X#(t,s) = -X#(s,t);
	    );
	);
    
    Y := new MutableHashTable;
    A := {} ;
    d2t := matrix entries(transpose(d2)) ;
    for s from 1 to l do (
    	for t from 1 to n do (
    	    for u from 1 to l do (
    		S := -(EE#(i,j))_(s-1,0)*(EF#(k,u))_(t-1,0) + (EE#(i,k))_(s-1,0)*(EF#(j,u))_(t-1,0) - (EE#(j,k))_(s-1,0)*(EF#(i,u))_(t-1,0);
    		S = S + sum(1..n, v -> (- X#(t,v)*(d3t_(v-1,s-1)))_(u-1,0)) ;
    		if u==s then S = S + (EEE#(i,j,k))_(t-1,0) ;
    		if S != 0 then A = append(A,S*(id_(Q^l)_{u-1}));
    		);
    	    Y#(s,t) =  matrix entries (sum(A) // d2t) ;
    	    ) ;
    	);
    
 {X,Y,EEE}
 )

-- Public functions

multTables = ( cacheValue "multTables" ) multtables

multTablesLink =  multtableslink

eeProd = (F,i,j) -> (
    mult := multTables F;
    (mult#0)#(i,j)
    )

efProd = (F,i,j) -> (
    mult := multTables F;
    (mult#1)#(i,j)
    )

mapX = (F,i,j,k) -> (
    m := multTablesLink (F,i,j,k);
    m#0
    )

mapY = (F,i,j,k) -> (
    m := multTablesLink (F,i,j,k);
    m#1
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

findRegSeq = ( M, L ) -> (
    L = L - {1,1,1};
    c := numcols M;
    C := flatten entries M ;
    K := toList(0..c-1) ;
    for l in L do K = delete( l, K);
    P := {};
    for l in L do (
	X := ideal(for i in delete(l,L) list C#i); 
	for k  in K do (
	    Y := X + ideal(C#l+C#k);
	    if codim Y == 3 then ( P = append( P, {Y,{l+1,k+1}} ));
	    );
	);
    if P != {} then P 
    else (
	for l in L do (
    	    X := ideal(C#l) ;
	    L1 := delete(l,L);
	    for k in K do (
		for m in delete(k,K) do (
		Y := X + ideal(C#(L1#0) + C#k, C#(L1#1) + C#m);
		if codim Y == 3 then ( P = append( P, {Y,{l+1,(L1#0+1,k+1),(L1#1+1,m+1)}} ) );
		);
	    );
	);
    );
P
)

codimThreeAlgStructure = method()

codimThreeAlgStructure(ChainComplex, List) := (F, sym) -> (
   if length F != 3 then
     error "Expected a chain complex of length three which is free of rank one in degree zero.";
   if #sym != 3 or any(sym, s -> (class baseName s =!= Symbol))  then
     error "Expected a list of three symbols.";
   mult := multTables(F);
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
   A
)

codimThreeTorAlgebra = method()

codimThreeTorAlgebra(ChainComplex,List) := (F,sym) -> (
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
   B
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

efMultTable = method(Options => options eeMultTable)

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
  -- check to ensure that A is torAlgebra for codim 3 example
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
   eList = eList | flatten entries ((gens annEs)*sub(matrix basis(1,annEs),B));
   fList := flatten entries matrix {{eList#1*eList#2,eList#2*eList#0,eList#0*eList#1}};
   fList = fList | flatten entries (basis(2,B)*(mingens coker multMap(B,1,1)));
   gList := flatten entries basis(3,B);
   performBasisChange(F,eList,fList,gList)
)

changeBasisHpq = method()
changeBasisHpq(ChainComplex,RingElement) := (F,ee) -> (
   Q := ring F;
   B := ring ee;
   
   multMap1 := multMap(ee,1);
   imagMultMap1 := mingens image multMap1;
   eList := flatten entries (basis(1,B)*(imagMultMap1 // multMap1));

   -- now we add in some indeterminants to track the lifts, and choose
   -- the first few es carefully so that they have trivial products.
   a := getSymbol "a";
   p := #eList;
   varRing := (coefficientRing B)[a_1..a_p];
   overC := varRing monoid B;
   newI := sub(ideal B, overC);
   C := overC/newI;
   eeC := sub(ee,C);

   if eList != {} then (
      genEList := apply(#eList, i -> sub(eList#i,C) + sub(varRing_i,C)*eeC);
      products := subsets(genEList,2) / product;
      prodCoeffs := last coefficients(matrix {products}, Monomials => basis(2,C));
      soln := mingens ideal flatten entries prodCoeffs;
      eList = apply(genEList, f -> sub(f % sub(soln, C), B));
   );

   eList = eList | {ee};
   dubAnnMod := (ann ann ee) / (ideal ee);
   newEs := flatten entries ((gens dubAnnMod)*(matrix basis(1,dubAnnMod)));
   newEs = newEs / (f -> f % ideal {ee});
   eList = eList | newEs;
   
   fList := flatten entries (basis(2,B)*imagMultMap1);
   multMap2 := multMap(ee,2);
   imagMultMap2 := mingens image multMap2;
   fList = fList | flatten entries (basis(2,B)*(imagMultMap2 // multMap2));
   -- complete fList to a basis in degree two now
   -- this is probably a very bad way to do it...
   newFs := basis(2,B)*(matrix basis(2, (ideal basis(2,B))/(sub(ideal fList,B))));
   fList = fList | flatten entries newFs;

   gList := flatten entries (basis(3,B)*imagMultMap2);
   gList = gList | flatten entries (basis(3,B)*(mingens coker multMap2));

   performBasisChange(F,eList,fList,gList)
)

changeBasisG = method()
changeBasisG(ChainComplex) := F -> (
   B := codimThreeTorAlgebra(F,{getSymbol "e",getSymbol "f", getSymbol "g"});
   gTop := first flatten entries (basis(3,B)*(matrix mingens image multMap(B,1,2)));
   gList := {gTop} | flatten entries mingens ((ideal basis(3,B))/(ideal gTop));
   proj := (last coefficients(matrix{gList}, Monomials=>basis(3,B)))^{0};
   multMapList := apply(flatten entries basis(2,B), f -> transpose (proj*multMap(f,1)));
   M := matrix {multMapList};
   -- ker M are the 'other' fs
   -- coker M are the 'other es
   -- the rest is a pairing that we have to diagonalize.
   otherFs := flatten entries (basis(2,B)*sub(gens ker M,B));
   otherEs := flatten entries (basis(1,B)*sub(matrix mingens coker M,B));
   goodEs := flatten entries (basis(1,B)*sub(matrix mingens image M, B));
   goodFs := flatten entries (basis(2,B)*sub(matrix mingens image M // M,B));
   eList := goodEs | otherEs;
   fList := goodFs | otherFs;
   performBasisChange(F,eList,fList,gList)
)

changeBasisB = method()
changeBasisB(ChainComplex) := F -> (
   B := codimThreeTorAlgebra(F,{getSymbol "e",getSymbol "f", getSymbol "g"});
   kk := coefficientRing B;
   gTop := first flatten entries (basis(3,B)*(matrix mingens image multMap(B,1,2)));
   gList := {gTop} | flatten entries mingens ((ideal basis(3,B))/(ideal gTop));
   proj := (last coefficients(matrix{gList}, Monomials=>basis(3,B)))^{0};
   multMapList := apply(flatten entries basis(2,B), f -> transpose (proj*multMap(f,1)));
   M := matrix {multMapList};
   otherFs := flatten entries (basis(2,B)*sub(gens ker M,B));
   goodFs := flatten entries (basis(2,B)*sub(matrix mingens image M // M,B));
   goodEcoords := apply(goodFs, x -> (sub((last coefficients(gTop,Monomials=>basis(3,B))),kk) // multMap(x,1)));
   goodEs := flatten apply(goodEcoords, c -> flatten entries (basis(1,B)*c));
   otherEs := flatten entries (basis(1,B)*sub(matrix mingens coker M,B));
   f3 := goodEs#0*goodEs#1;
   otherFs = flatten entries mingens ((ideal otherFs)/(ideal f3));
   eList := goodEs | otherEs;
   fList := goodFs | {goodEs#0*goodEs#1} | otherFs;
   performBasisChange(F,eList,fList,gList)
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

uninstallPackage "MultFreeResThree"
restart
debug loadPackage "MultFreeResThree"
check "MultFreeResThree"

-- dev space

needsPackage "TorAlgebra"'
needsPackage "PruneComplex"

Q = QQ[x,y,z];

F = res ideal (x*y, y*z, x^3, y^3-x*z^2,x^2*z,z^3);
mult = multTables(F)
peek (mult#0)
peek (mult#1)
eeProd(F,2,3)
efProd(F,2,3)

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
changeBasisT(F,B,{e_1,e_2,e_4}) -- (currently) based on es list
changeBasisHpq(F,e_(p+1))       -- can determine based on distinguised element (open set of them)
changeBasisG(F)                 -- can determine based on class.
changeBasisC3(F,B)              -- no need to do anything
changeBasisB(F,B,{e_1,e_2},g_1) -- e's and g inform choice of fs

--- Example of a class T ring
restart
debug loadPackage "MultFreeResThree"
Q = ZZ/3[x,y,z];
I = ideal (x^2, y^3, z^4, x*y*z)
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisT(F,{e_1,e_2,e_4})
isHomogeneous G
C = codimThreeTorAlgebra(G,{e,f,g})
(net F.dd) | (net G.dd)
netList eeMultTable(C)
netList efMultTable(C)

--- H(3,2) example
restart
debug loadPackage "MultFreeResThree"
Q = ZZ/53[x,y,z];
I = ideal (x^2, y^3, z^4, x*y)
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable(B)
G = changeBasisHpq(F,e_4)
isHomogeneous G
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable(C)
netList efMultTable(C)

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
G = changeBasisHpq(F,e_2)
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
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable C
netList efMultTable C

--- example from Lars -- class H(6,5)
restart
debug loadPackage "MultFreeResThree"
Q = ZZ/53[x,y,z]
I = ideal (x^5, y^5, x*y^4, x^2*y^3, x^3*y^2, x^4*y, z^3)
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable(B)
netList efMultTable B
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
G = changeBasisHpq(F,e_1)
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable(C)
netList efMultTable(C)

--- change of basis working space G(2)
restart
debug loadPackage "MultFreeResThree"
kk = ZZ/32003
Q = kk[x,y,z]
I = ideal(x*y^2,x*y*z,y*z^2,x^4-y^3*z,x*z^3-y^4)
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisG(F)
C = codimThreeTorAlgebra(G,{e,f,g})
netList eeMultTable C
netList efMultTable C

--- change of basis for class B
restart
debug loadPackage "MultFreeResThree"
kk = ZZ/32003
Q = kk[x,y,z]
I = ideal(x^2,x*y,z^2,y*z)
F = res I
B = codimThreeTorAlgebra(F,{e,f,g})
netList eeMultTable B
netList efMultTable B
G = changeBasisB F
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

m = genmulttables F
peek m#0
Q_5
m = multTablesLink (F,1,2,3)
eeProd(F,2,4)
for i from 1 to 7 list efProd(F,3,i)
codim ideal(d1_(0,0),d1_(0,1) + d1_(0,2),d1_(0,3) + d1_(0,5))
mapX(F,1,2,3)
mapY(F,1,2,3)

peek m(sum m)
m_0
#m

id_(Q^7)_{0}

peek m
m#(1,1)
peek F.cache
m = multtable(d1,d2,d3)

(m#0)#(1,2)
ee = EEtable(d1,d2,d3)
peek ee

ee#(1,2,3)
ee#(2,1,3)
a = map(Q^1,Q^3,(i,j) -> 0)

numcols ee#(1,1)

n=numcols d3

 matrix entries map(Q^n,Q^1,(i,j) -> 0)

----------------------------------------------------------------------------
-- Functions for presenting classification data
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- torAlgDataList
--
-- R a quotient of a polynomial algebra
-- L a list of keys for the hash table returned by torAlgClass
--
-- Returns a list of the values of the specified keys

--==========================================================================
-- INTERNAL ROUTINES
--==========================================================================

----------------------------------------------------------------------------
-- Routines used by 
----------------------------------------------------------------------------

----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- Auxiliary routines
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- sup
--
-- C a chain complex
--
-- Returns the supremum of C: the highest degree of a non-zero module

-- sup = C -> (
--     j := max C;
--     while true do (
--         if j < min C then (
-- 	    break -infinity 
-- 	    )
-- 	else (
-- 	    if C_j != 0 then (
-- 		break j 
-- 		)
-- 	    else (
-- 		j = j-1
-- 		)
-- 	    )
-- 	)
--     )


-- ----------------------------------------------------------------------------
-- -- grade
-- --
-- -- I a homogeneous ideal in a polynomial algebra
-- --
-- -- Returns the grade of I

-- grade = I -> (
--     - sup prune HH(dual res I)
--     )

-- ----------------------------------------------------------------------------
-- -- zeroIdeal
-- --
-- -- R a ring
-- --
-- -- Returns the zero ideal of R

-- zeroIdeal = R -> ideal (map(R^1,R^0,0)) 


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
      
      According to the multiplicative structure on Tor$_Q*$ ($R,k$), a
      non-zero local ring $R$ of codepth at most 3 belongs to exactly one of
      the (parametrized) classes designated {\bf B}, {\bf C}(c), {\bf G}(r), {\bf
      H}(p,q), {\bf S}, or {\bf T}. An overview of the theory can be
      found in L.L. Avramov, {\it A cohomological study of local rings
      of embedding codepth 3}, @HREF"http://arxiv.org/abs/1105.3991"@.

      There is a similar classification of Gorenstein local rings of codepth
      4, due to A.R. Kustin and M. Miller. There are four classes,
      which in the original paper, {\it Classification of the
      Tor-Algebras of Codimension Four Gorenstein Local rings}
      @HREF"https://doi.org/10.1007/BF01215134"@, are called A, B, C,
      and D, while in the survey {\it Homological asymptotics of
      modules over local rings}
      @HREF"https://doi.org/10.1007/978-1-4612-3660-3_3"@ by
      L.L. Avramov, they are called CI, GGO, GTE, and GH(p),
      respectively. Here we denote these classes {\bf C}(c), {\bf GS},
      {\bf GT}, and {\bf GH}(p), respectively.
  
      The package implements an algorithm for classification of local
      rings in the sense discussed above. For rings of codepth at most
      3 it is described in L.W. Christensen and O. Veliche, {\it Local
      rings of embedding codepth 3: a classification algorithm},
      @HREF"http://arxiv.org/abs/1402.4052"@. The classification of
      Gorenstein rings of codepth 4 is analogous. 
      
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

doc ///
  Key
    torAlgClass
  Headline
    the class (w.r.t. multiplication in homology) of a local ring
  Usage
    torAlgClass R
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : String
        the (parametrized) class of the local ring obtained by
        localizing {\tt R} at the irrelevant maximal ideal, provided
        that this ring is non-zero and of codepth at most 3 or Gorenstein 
	or Golod; otherwise "no class"
  Description
  
    Text 
      Classifies the local ring obtained by localizing {\tt R} at
      the irrelevant maximal ideal as belonging to one of the
      (parametrized) classes {\bf B}, {\bf C}(c), {\bf G}(r), {\bf H}(p,q),
      {\bf S}, or {\bf T}, provided that it is codepth at most 3.
      
    Example
      Q = QQ[x,y,z];
      torAlgClass Q
      torAlgClass (Q/ideal (x*y))
      torAlgClass (Q/ideal (x^2,y^2))
      torAlgClass (Q/ideal (x^2,y^2,x*y))
      torAlgClass (Q/ideal (x^2,x*y,y*z,z^2))
      torAlgClass (Q/ideal (x^2,y^2,z^2))      
      torAlgClass (Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3))
      torAlgClass (Q/ideal (x*z+y*z,x*y+y*z,x^2-y*z,y*z^2+z^3,y^3-z^3))
      torAlgClass (Q/ideal (x^2,y^2,z^2,x*y))
      torAlgClass (Q/ideal (x^2,y^2,z^2,x*y*z))
      
    Text  
      If the local ring is Gorenstein or Golod of codepth 4, then it is classified
      as belonging to one of the (parametrized) classes {\bf C}(4), {\bf GH}(p), 
      {\bf GS}, {\bf GT}, or {\bf codepth 4 Golod}.
      
    Example
      Q = QQ[w,x,y,z];
      torAlgClass (Q/ideal (w^2,x^2,y^2,z^2))
      torAlgClass (Q/ideal (y*z,x*z,x*y+z^2,x^2,w*x+y^2+z^2,w^2+w*y+y^2+z^2))
      torAlgClass (Q/ideal (z^2,x*z,w*z+y*z,y^2,x*y,w*y,x^2,w*x+y*z,w^2+y*z))
      torAlgClass (Q/ideal (x^2,y^2,z^2,x*w,y*w,z*w,w^3-x*y*z))
      torAlgClass (Q/(ideal (w,x,y,z))^2)

    Text	  
      If the local ring has codepth at least 5, then it is classified as belonging
      to one of the classes {\bf C}(c), if it is complete intersection, {\bf codepth c Gorenstein}, 
      if it is Gorenstein and not complete intersection, {\bf codepth c Golod}, if it is Golod,
      and {\tt no class} otherwise.
            
    Example
      Q = QQ[u,v,w,x,y,z];
      torAlgClass (Q/ideal (u^2,v^2,w^2,x^2+y^2, x^2+z^2))
      torAlgClass (Q/ideal (w^2,v*w,z*w,y*w,v^2,z*v+x*w,y*v,x*v,z^2+x*w,y*z,x*z,y^2+x*w,x*y,x^2))
      torAlgClass (Q/ideal (x^2*y^2,x^2*z,y^2*z,u^2*z,v^2*z,w^2*z))
      torAlgClass (Q/ideal (u^2,v^2,w^2,x^2,z^2,x*y^15))
      
    Text  
      If the defining ideal of {\tt R} is not contained in the irrelevant maximal ideal, 
      then the resulting local ring is zero, and the function returns {\tt zero ring}.
      
    Example
      Q = QQ[x,y,z];
      torAlgClass (Q/ideal (x^2-1))
///

doc ///
  Key
    isCI
  Headline
    whether the ring is complete intersection
  Usage
    isCI R
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : Boolean
        whether the local ring obtained by localizing {\tt R} at the irrelevant maximal ideal is complete intersection

  Description
  
    Text 
      Checks if the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal is complete intersection.
      
    Example
      Q = QQ[x,y,z];
      isCI (Q/ideal (x^2,x*y,y*z,z^2))
      isCI (Q/ideal (x^2,y^2))
///

doc ///
  Key
    isGorenstein
  Headline
    whether the ring Gorenstein
  Usage
    isGorenstein R
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : Boolean
        whether the local ring obtained by
        localizing {\tt R} at the irrelevant maximal ideal is Gorenstein
	
  Description
  
    Text 
      Checks if the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal is Gorenstein.
      
    Example
      Q = QQ[x,y,z];
      isGorenstein (Q/ideal (x^2,x*y,y*z,z^2))
      isGorenstein (Q/ideal (x^2,y^2))
      isGorenstein (Q/ideal (x*z+y*z,x*y+y*z,x^2-y*z,y*z^2+z^3,y^3-z^3))
///

doc ///
  Key
    isGolod
  Headline
    whether the ring is Golod
  Usage
    isGolod R
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : Boolean
        whether the local ring obtained by
        localizing {\tt R} at the irrelevant maximal ideal is Golod
	
  Description
  
    Text 
      Checks if the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal is Golod
      
    Example
      Q = QQ[x,y,z];
      isGolod (Q/ideal (x^2,x*y,y*z,z^2))
      isGolod (Q/ideal (x^2))
      isGolod (Q/(ideal (x,y,z))^2)      
///

doc ///
  Key
    torAlgDataList
  Headline
    list invariants of a local ring
  Usage
    torAlgDataList(R,L)
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra  by an ideal contained in the irrelevant maximal ideal
    L : List
        a list of keys from the hash table returned by @TO torAlgData@
	
  Outputs
      : List
        the list of values corresponding to the keys specified in {\tt L}
	
  Description
    Text 
      Extracts data from the hash table returned by @TO torAlgData@.

    Example
      Q = QQ[x,y,z];
      R = Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      torAlgDataList( R, {m, n, Class, p, q, r, PoincareSeries, BassSeries} )            
///      
      
doc ///
  Key
    torAlgDataPrint
  Headline
    print invariants of a local ring
  Usage
    torAlgDataPrint (R,L)
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra  by an ideal contained in the irrelevant maximal ideal
    L : List 
        a list of keys from the hash table returned by @TO torAlgData@    
  Outputs
      : String
        the string of keys specified in {\tt L} together with their values
  Description
    Text 
       Extracts data from the hash table returned by @TO torAlgData@.

    Example
      Q = QQ[x,y,z];
      R = Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      torAlgDataPrint( R, {c, e, h, m, n, Class, p, q, r} )      
     
///

doc ///
  Key
    setAttemptsAtGenericReduction
  Headline
    control the number of attempts to compute Bass numbers via a generic reduction
  Usage
    setAttemptsAtGenericReduction(R,n)
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
    n : ZZ
        a positive integer
  Outputs
      : ZZ
        the number of attempts that will be made to perform a generic reduction to compute the Bass 
	numbers of the local ring obtained by localizing {\tt R} at the irrelevant maximal ideal
  Description
  
    Text 
      Changes the number of attempts made to reduce {\tt R} modulo a generic regular sequence of
      generators of the irrelevant maximal ideal in order to compute the Bass numbers of the
      local ring obtained by localizing {\tt R} at the irrelevant maximal ideal. The function has
      the effect of setting {\tt R.attemptsAtGenericReduction = n}, and the number of attempts made is 
      at most {\tt n^2}. If {\tt R.attemptsAtGenericReduction} is not set, then at most 625 attempts
      are made.
      
    Example
      Q = ZZ/2[u,v,w,x,y,z];
      R = Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      R.?attemptsAtGenericReduction
      setAttemptsAtGenericReduction(R,100)
      R.attemptsAtGenericReduction
      
    Text 
      If the value of {\tt R.attemptsAtGenericReduction} is too small, then the computation of Bass
      numbers may fail resulting in an error message. Notice, though, that if the local ring obtained     
      by localizing {\tt R} at the irrelevant maximal ideal has embedding dimension at most 3, then the 
      Bass numbers are computed without any attempt to reduce the ring, and {\tt R.attemptsAtGenericReduction}
      has no significance.

    Example
      Q = ZZ/2[x,y,z];
      R = Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      setAttemptsAtGenericReduction(R,0)
      torAlgClass R
///

doc ///
  Key
    attemptsAtGenericReduction
  Headline
    see setAttemptsAtGenericReduction
  Description
  
    Text 
      See @TO setAttemptsAtGenericReduction@

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

-- #1 zero ring, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( x^2-1 )
assert( torAlgClass(Q/I) === "zero ring" )
///

-- #2 C(0), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(u+v+w+x+y+z)
assert( torAlgClass(Q/I) === "C(0)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 0, 0, 0, 1, "C", 0, 0, 0} )
///

-- #3 C(0), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x-y^2-z^7+u+v+w)
assert( torAlgClass(Q/I) === "C(0)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {5, 0, 0, 0, 1, "C", 0, 0, 0} )
///

-- #4 C(1), graded
TEST ///
Q = QQ[x]
I = ideal(x^2)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {1, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #5 C(1), local
TEST ///
Q = QQ[x]
I = ideal(x^2-x^3)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {1, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #6 C(1), graded
TEST ///
Q = ZZ/53[x,y]
I = ideal(x*y)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {2, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #7 C(1), local
TEST ///
Q = ZZ/53[x,y]
I = ideal(x*y-x^3)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {2, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #8 C(1), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y,x+y+z,u+v+w)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #9 C(1), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2-y^3+z^3)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #10 C(2), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y,u*v,x+y+z+u+v+w)
assert( torAlgClass(Q/I) === "C(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 2, 0, 2, 1, "C", 1, 0, 0} )
///

-- #11 C(2), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y,u*v,z^2-w)
assert( torAlgClass(Q/I) === "C(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 2, 0, 2, 1, "C", 1, 0, 0} ) 
///

-- #12 S, graded
TEST ///
Q = QQ[x,y]
I = ideal(x^2,x*y)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {2, 2, 1, 2, 1, "S", 0, 0, 0} )
///

-- #13 S, local
TEST ///
Q = QQ[x,y]
I = ideal((x+y^2)^2,(x+y^2)*y)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {2, 2, 1, 2, 1, "S", 0, 0, 0} )
///

-- #14 S, graded
TEST ///
Q = ZZ/53[u,v,w,x,y,z]
I = ideal(x^2,x*y,y*z,x+y+z+u+v+w)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 2, 0, 3, 2, "S", 0, 0, 0} )
///

-- #15 S, local
TEST ///
Q = ZZ/53[u,v,w,x,y,z]
I = ideal(x^2*y-y^2,x^3-x*y)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 2, 1, 2, 1, "S", 0, 0, 0} ) 
///

-- #16 B, graded
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,x*y,z^2,y*z,x+y+z+u+v+w)
assert( torAlgClass(Q/I) === "B" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 3, 1, 4, 1, "B", 1, 1, 2} )
///

-- #17 B, local
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[u,v,w,x,y,z]
I = ideal(x^4-z*w^2,y^3-x^2*w,z^3-x*y,w^3-x*y^2*z^2,z^2*x^3-y*w^2,u,v)
assert( torAlgClass(Q/I) === "B" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 3, 0, 5, 2, "B", 1, 1, 2} ) 
///

-- #18 C(3), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u*v,w*x,y*z)
assert( torAlgClass(Q/I) === "C(3)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 0, 3, 1, "C", 3, 1, 3} ) 
///

-- #19 C(3), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(u*v,w*(x+w^2),y*z)
assert( torAlgClass(Q/I) === "C(3)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 0, 3, 1, "C", 3, 1, 3} )
///

-- #20 G(7) Gorenstein, graded
TEST ///
Q = QQ[x,y,z]
I = ideal(x^3,x^2*z,x*(z^2+x*y),z^3-2*x*y*z,y*(z^2+x*y),y^2*z,y^3)
assert( torAlgClass(Q/I) === "G(7), Gorenstein" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {3, 3, 0, 7, 1, "G", 0, 1, 7} ) 
///

-- #21 G(7) Gorenstein, local
TEST ///
Q = QQ[x,y,z]
I = ideal(x^6,x^4*z,x^2*(z^2+x^2*y),z^3-2*x^2*y*z,y*(z^2+x^2*y),y^2*z,y^3)
assert( torAlgClass(Q/I) === "G(7), Gorenstein" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {3, 3, 0, 7, 1, "G", 0, 1, 7} )
///

-- #22 G(2), graded
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y^2,x*y*z,y*z^2,x^4-y^3*z,x*z^3-y^4)
assert( torAlgClass(Q/I) === "G(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 1, 5, 2, "G", 0, 1, 2} ) 
///

-- #23 G(4), local
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[u,v,w,x,y,z]
I = ideal (x^6,x^2*(z^2+x^2*y),z^3-2*x^2*y*z,y*(z^2+x^2*y),y^2*z,y^3)+x^4*z*ideal(x,y,z)
assert( torAlgClass(Q/I) === "G(4)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {6, 3, 0, 7, 2, "G", 0, 1, 4} )
///

-- #24 H(0,0), graded
TEST ///
Q = QQ[x,y,z]
I = ideal(x^2,x*y^2)*ideal(y*z,x*z,z^2)  
assert( torAlgClass(Q/I) === "H(0,0)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {3, 3, 2, 5, 2, "H", 0, 0, 0} ) 
///

-- #25 H(0,0), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,x*(y+v^2)^2)*ideal((y+v^2)*z,x*z,z^2)
assert( torAlgClass(Q/I) === "H(0,0)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {6, 3, 2, 5, 2, "H", 0, 0, 0} )
///

-- #26 H(3,2), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*y,x+u+z,y+v+w) 
assert( torAlgClass(Q/I) === "H(3,2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 3, 0, 4, 2, "H", 3, 2, 2} ) 
///

-- #27 H(3,2), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,(y+w^2)^2,z^2,x*(y+w^2))
assert( torAlgClass(Q/I) === "H(3,2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {6, 3, 0, 4, 2, "H", 3, 2, 2} )
///

-- #28 T, graded
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[u,v,w,x,y,z]
I = ideal(x^3,y^3,z^3,x^2*y*z,x+y+x+u+v+w)
assert( torAlgClass(Q/I) === "T" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {5, 3, 0, 4, 3, "T", 3, 0, 0} )
///

-- #29 T, local
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[u,v,w,x,y,z]
I = ideal((x+u^2)^2,y^2,z^3,(x+u^2)*y*z^2) 
assert( torAlgClass(Q/I) === "T" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 0, 4, 3, "T", 3, 0, 0} ) 
///

-- #30 C(4), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2,v^2,w^2,x^2,y+z )
assert( torAlgClass(Q/I) === "C(4)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 4, 0, 4, 1, "C", 6, 4, 4} ) 
///

-- #31 C(4), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2-v^3,z^6,w^2,x^2,y )
assert( torAlgClass(Q/I) === "C(4)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 4, 0, 4, 1, "C", 6, 4, 4} ) 
///

-- #32 GT, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,u*w^2-x*y*z)
assert( torAlgClass(Q/I) === "GT" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 4, 0, 7, 1, "GT", 3, 3, 7} ) 
///

-- #33 GT, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2-x*y*z)
assert( torAlgClass(Q/I) === "GT" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 4, 0, 7, 1, "GT", 3, 3, 7} ) 
///

-- #34, GH(5) graded
TEST ///
setRandomSeed "TorAlgebra"
Q = QQ[w,x,y,z]
I = ideal fromDual(matrix random(3,Q))
assert( torAlgClass(Q/I) === "GH(5)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 4, 0, 6, 1, "GH", 5, 6, 6} ) 
///

-- #35 GH(2), local,
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(w^2-x*y*z,x^3,y^3,x^2*z,y^2*z,z^3-x*y*w,x*z*w,y*z*w,z^2*w-x^2*y^2)
assert( torAlgClass(Q/I) === "GH(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 4, 0, 9, 1, "GH", 2, 3, 9} ) 
///

-- #36 GS, graded
TEST ///
setRandomSeed "TorAlgebra"
Q = QQ[w,x,y,z]
I = ideal fromDual(matrix random(2,Q))
assert( torAlgClass(Q/I) === "GS" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 4, 0, 9, 1, "GS", 0, 0, 9} ) 
///

-- #37 codepth 4 Golod, graded
TEST ///
Q = QQ[w,x,y,z]
I = ideal(w^2,x^2,y^2)*ideal(y^2,z^2)
assert( torAlgClass(Q/I) === "codepth 4 Golod" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 4, 2, 6, 1, "Golod", 0, 0, "-"} ) 
///

-- #38 codepth 4 Golod, local
TEST ///
Q = QQ[w,x,y,z]
I = ideal(w^2-y^3,x^2,y^2)*ideal(y^2,z^2)
assert( torAlgClass(Q/I) === "codepth 4 Golod" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 4, 2, 6, 1, "Golod", 0, 0, "-"} ) 
///

-- #39 codepth 4 no class, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2)
assert( torAlgClass(Q/I) === "codepth 4 no class" )
///

-- #40 codepth 4 no class, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,u+x*w,y*w,z*w,w^2)
assert( torAlgClass(Q/I) === "codepth 4 no class" )
///

-- #41 C(5), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2,v^2,w^2,x^2,y^2 )
assert( torAlgClass(Q/I) === "C(5)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 5, 0, 5, 1, "C", 10, 10, 5} ) 
///

-- #42 C(5), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2-v^3,z^6,w^2,x^2,y^3 )
assert( torAlgClass(Q/I) === "C(5)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 5, 0, 5, 1, "C", 10, 10, 5} ) 
///

-- #43 codepth 5 Gorenstein, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^3-x*y*z,v^2)
assert( torAlgClass(Q/I) === "codepth 5 Gorenstein" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 5, 0, 8, 1, "Gorenstein", "UNDETERMINED", "UNDETERMINED", 8} ) 
///

-- #44 codepth 5 Gorenstein, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2-x*y*z,v^2)
assert( torAlgClass(Q/I) === "codepth 5 Gorenstein" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 5, 0, 8, 1, "Gorenstein", "UNDETERMINED", "UNDETERMINED", 8} ) 
///

-- #45 codepth 5 Golod, graded
TEST ///
Q = QQ[v,w,x,y,z]
I = ideal(w^2,x^2,y^2)*ideal(y^2,z^2,v^3-x*y*z)
assert( torAlgClass(Q/I) === "codepth 5 Golod" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 5, 2, 9, 1, "Golod", 0, 0, "-"} ) 
///

-- #46 codepth 5 Golod, local
TEST ///
Q = QQ[v,w,x,y,z]
I = ideal(v,w,x,y,z)*ideal(z^2-x*y*v)
assert( torAlgClass(Q/I) === "codepth 5 Golod" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 5, 4, 5, 1, "Golod", 0, 0, "-"} ) 
///

-- #47 codepth 5 no class, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2,v^2)
assert( torAlgClass(Q/I) === "codepth 5 no class" )
///

-- #48 codepth 5 no class, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2-x*y*z*v,v^2)
assert( torAlgClass(Q/I) === "codepth 5 no class" )
///

-- #49 codepth 6 no class, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u^2,v^2,w^2,x^2,x*y^15,w*z^4)
assert( torAlgClass(Q/I) === "codepth 6 no class" )
///

-- #50 C(6), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u^2,v^2,w^2,x^2,y^2,x*y+z^2)
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 6, 0, 6, 1, "C", 15, 20, 6} )
///

-- #51 Codepth 6 Gorenstein, graded
TEST ///
Q = ZZ/53[u,v,w,x,y,z]
I = ideal fromDual(matrix random(3,Q))
L = torAlgDataList(Q/I,{e, c, h, m, n, Class})
assert( L === {6, 6, 0, 15, 1, "Gorenstein"} )
///

-- #52 Codepth 6 Golod, graded
TEST ///
Q = ZZ/53[u,v,w,x,y,z]
I = ideal(x^2*y^2,x^2*z,y^2*z,u^2*z,v^2*z,w^2*z,z^2)
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 6, 4, 7, 1, "Golod", 0, 0, "-"} )
///


