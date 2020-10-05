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

codimThreeAlgStructure( ChainComplex ) := (F) -> (
    codimThreeAlgStructure(F, {getSymbol "e", getSymbol "f", getSymbol "g" })
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

codimThreeTorAlgebra( ChainComplex ) := (F) -> (
    codimThreeTorAlgebra(F, {getSymbol "e", getSymbol "f", getSymbol "g" })
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

torAlgebraClassCodim3 = method()

torAlgebraClassCodim3 ChainComplex := F -> (
   B := codimThreeTorAlgebra(F,{getSymbol "e",getSymbol "f", getSymbol "g"});
   toralgebraclass B
)

torAlgebraClassCodim3 Ideal := I -> (
   torAlgebraClassCodim3 res I
)


--======================================================================
-- Internal functions
--======================================================================

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

homothetyMap = method()

homothetyMap(Ring,ZZ,ZZ) := (A,m,n) -> (
    Abasism := basis(m,A);
    homothetyList := apply(flatten entries Abasism, f -> transpose matrix {flatten entries multMap(f,n)});
    matrix {homothetyList}
)

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

toralgebraclass = A -> (
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

TEST ///
Q = QQ[x,y,z];
F = res ideal (x*y, y*z, x^3, y^3-x*z^2,x^2*z,z^3);
assert ( (eeProd(F,1,2))_(0,0) == y )
assert ( (efProd(F,1,4))_(0,0) == -x )
///


-- #16 B, graded
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[x,y,z]
I = ideal(x^2,x*y,z^2,y*z,z^2)
assert( torAlgebraClassCodim3(I) === "B" )
///

-- #18 C(3), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u*v,w*x,y*z)
assert( torAlgebraClassCodim3(I) === "C(3)" )
///

-- #20 G(7) Gorenstein, graded
TEST ///
Q = QQ[x,y,z]
I = ideal(x^3,x^2*z,x*(z^2+x*y),z^3-2*x*y*z,y*(z^2+x*y),y^2*z,y^3)
assert( torAlgebraClassCodim3(I) === "G(7)" )
///

-- #22 G(2), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y^2,x*y*z,y*z^2,x^4-y^3*z,x*z^3-y^4)
assert( torAlgebraClassCodim3(I) === "G(2)" )
///

-- #24 H(0,0), graded
TEST ///
Q = QQ[x,y,z]
I = ideal(x^2,x*y^2)*ideal(y*z,x*z,z^2)  
assert( torAlgebraClassCodim3(I) === "H(0,0)" )
///

-- #24 H(0,0), graded
TEST ///
Q = QQ[x,y,z]
I = ideal(x^2,x*y^2)*ideal(y*z,x*z,z^2)  
assert( torAlgebraClassCodim3(I) === "H(0,0)" )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^3,y^3,z^3,x*y*z)  
assert( torAlgebraClassCodim3(I) === "T" )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^5,y^5,x*y^4,x^2*y^3,x^3*y^2,x^4*y,z^3)  
assert( torAlgebraClassCodim3(I) === "H(6,5)" )
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

I = ideal (x*y, y*z, x^3, y^3-x*z^2,x^2*z,z^3);
F = res I
A = codimThreeAlgStructure (F)

A = codimThreeAlgStructure (F, {e,f,g})
B = codimThreeTorAlgebra (F, {e,f,g})
torAlgebraClassCodim3 F
torAlgebraClassCodim3 I


torAlgebraClassCodim3 F
torAlgebraClassCodim3 B
torAlgebraClassCodim3 A


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
