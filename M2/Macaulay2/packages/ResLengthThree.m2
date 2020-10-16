newPackage ( "ResLengthThree",
    Version => "0.6",
    Date => "15 October 2020",
    Authors => {
	{ Name => "Lars Winther Christensen",
	  Email => "lars.w.christensen@ttu.edu",
	  HomePage => "http://www.math.ttu.edu/~lchriste/index.html" },
      { Name => "Luigi Ferraro",
	  Email => "lferraro@ttu.edu",
	  HomePage => "http://www.math.ttu.edu/~lferraro" },
	{ Name => "Francesca Gandini",
	  Email => "fra.gandi.phd@gmail.com",
	  HomePage => "http://www.kzoo.edu/faculty/index.php?name=fgandini" },
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

export { "resLengthThreeAlg", "resLengthThreeTorAlg", "multTableOneOne", "multTableOneTwo", 
    "resLengthThreeTorAlgClass", "makeRes", "Labels", "Compact" }

--==========================================================================
-- EXPORTED FUNCTIONS
--==========================================================================

resLengthThreeAlg = method()
resLengthThreeAlg(ChainComplex, List) := (F, sym) -> (
   if F.cache#?"Algebra Structure" then return F.cache#"Algebra Structure";
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
   I = ideal mingens I;
   A := P/I;
   A.cache#"l" = l;
   A.cache#"m" = m;
   A.cache#"n" = n;
   F.cache#"Algebra Structure" = A;
   A
)

resLengthThreeAlg( ChainComplex ) := (F) -> (
    resLengthThreeAlg(F, {getSymbol "e", getSymbol "f", getSymbol "g" })
    )

resLengthThreeAlg' = method()
resLengthThreeAlg'(ChainComplex, List) := (F, sym) -> (
   if F.cache#?"Algebra Structure" then return F.cache#"Algebra Structure";
   if length F != 3 then
     error "Expected a chain complex of length three which is free of rank one in degree zero.";
   if #sym != 3 or any(sym, s -> (class baseName s =!= Symbol))  then
     error "Expected a list of three symbols.";
   mult := multTables'(F);
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
   eVector := matrix {apply(m, i -> P_(i))};
   fVector := matrix {apply(l, i -> P_(m+i))};
   gVector := matrix {apply(n, i -> P_(m+l+i))};

   eeGens := flatten entries (matrix {flatten entries (-((transpose eVector) * eVector))} - fVector*(mult#0));
   efGens := flatten entries (matrix {flatten entries (-((transpose eVector) * fVector))} - gVector*(mult#1));

   I := (ideal eeGens) +
        (ideal efGens) +
	(ideal apply(m..(m+l-1), i -> P_i))^2 +
	(ideal apply(0..(m-1), i -> P_i))*(ideal apply((m+l)..(m+l+n-1), i -> P_i)) + 
	(ideal apply(m..(m+l-1), i -> P_i))*(ideal apply((m+l)..(m+l+n-1), i -> P_i)) +
	(ideal apply((m+l)..(m+l+n-1), i -> P_i))^2;
   I = ideal mingens I;
   A := P/I;
   A.cache#"l" = l;
   A.cache#"m" = m;
   A.cache#"n" = n;
   F.cache#"Algebra Structure" = A;
   A
)

resLengthThreeAlg'( ChainComplex ) := (F) -> (
    resLengthThreeAlg'(F, {getSymbol "e", getSymbol "f", getSymbol "g" })
    )

resLengthThreeTorAlg = method()

resLengthThreeTorAlg(ChainComplex,List) := (F,sym) -> (
   if F.cache#?"Tor Algebra Structure" then return F.cache#"Tor Algebra Structure";
   A := resLengthThreeAlg(F,sym);
   P := ambient A;
   Q := ring F;
   kk := coefficientRing first flattenRing Q;
   PP := kk monoid P;
   I := ideal mingens sub(ideal A, PP);
   B := PP/I;
   B.cache#"l" = A.cache#"l";
   B.cache#"m" = A.cache#"m";
   B.cache#"n" = A.cache#"n";
   F.cache#"Tor Algebra Structure" = B;
   B
)

resLengthThreeTorAlg( ChainComplex ) := (F) -> (
    resLengthThreeTorAlg(F, {getSymbol "e", getSymbol "f", getSymbol "g" })
    )

resLengthThreeTorAlg' = method()

resLengthThreeTorAlg'(ChainComplex,List) := (F,sym) -> (
   if F.cache#?"Tor Algebra Structure" then return F.cache#"Tor Algebra Structure";
   A := resLengthThreeAlg(F,sym);
   P := ambient A;
   Q := ring F;
   kk := coefficientRing first flattenRing Q;
   PP := kk monoid P;
   I := ideal mingens sub(ideal A, PP);
   B := PP/I;
   B.cache#"l" = A.cache#"l";
   B.cache#"m" = A.cache#"m";
   B.cache#"n" = A.cache#"n";
   F.cache#"Tor Algebra Structure" = B;
   B
)

resLengthThreeTorAlg'( ChainComplex ) := (F) -> (
    resLengthThreeTorAlg'(F, {getSymbol "e", getSymbol "f", getSymbol "g" })
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

multTableOneOne = method(Options => {Labels => true, Compact => false})

multTableOneOne(Ring) := opts -> A -> (
   if not (A.cache#?"l" and A.cache#?"m" and A.cache#?"n") then
      error "Expected an algebra created with a resLengthThree routine.";
   l := A.cache#"l";
   m := A.cache#"m";
   n := A.cache#"n";
   eVector := matrix {apply(m, i -> A_i)};
   if (opts.Compact) then (
       oneTimesOneA := matrix table(m,m, (i,j) -> if i <= j then (A_i)*(A_j) else 0))
   else (
       oneTimesOneA = matrix table(m,m,(i,j) -> (A_i)*(A_j));
       );
   result := entries ((matrix {{0}} | eVector) || ((transpose eVector) | oneTimesOneA));
   if (opts.Labels) then result else oneTimesOneA
   )

multTableOneTwo = method(Options => { Labels => true} )

multTableOneTwo(Ring) := opts -> A -> (
   if not (A.cache#?"l" and A.cache#?"m" and A.cache#?"n") then
      error "Expected an algebra created with a resLengthThree routine.";
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

resLengthThreeTorAlgClass = method()

resLengthThreeTorAlgClass ChainComplex := F -> (
  A := resLengthThreeTorAlg(F);
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

resLengthThreeTorAlgClass Ideal := I -> (
   resLengthThreeTorAlgClass res I
)

resLengthThreeTorAlgClass' = method()

resLengthThreeTorAlgClass' ChainComplex := F -> (
  A := resLengthThreeTorAlg'(F);
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

resLengthThreeTorAlgClass' Ideal := I -> (
   resLengthThreeTorAlgClass' res I
)


--======================================================================
-- INTERNAL FUNCTIONS
--======================================================================

multTables = F -> (
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
 
multTables' = F -> (
    Q := ring F;
    d1:= matrix entries F.dd_1;
    d2:= matrix entries F.dd_2;    
    d3:= matrix entries F.dd_3;    
    m := numcols d1;
    l := numcols d2;
    n := numcols d3;
    
    multEE := (d1**(id_(source d1)) - (id_(source d1))**d1) // d2;
    multEF := time (d1**(id_(source d2)) - multEE * (id_(source d1)**d2)) // d3;

    {multEE,multEF}
    --EE := hashTable flatten apply(m, i -> apply(m, j -> ((i+1,j+1), multEE_{m*i+j})));
    --EF := hashTable flatten apply(m, i -> apply(l, j -> ((i+1,j+1), multFF_{m*i+j})));
    --{EE,EF}
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

TEST ///
Q = QQ[x,y,z];
F = res ideal (x*y, y*z, x^3, y^3-x*z^2,x^2*z,z^3);
G = resLengthThreeAlg (F)
assert ( e_1*e_2 == y*f_1 )
assert ( e_1*f_4 == -x*g_1 )
///

TEST ///
Q = QQ[x,y,z];
F = res ideal (x*y, y*z, x^3, y^3-x*z^2,x^2*z,z^3);
G = resLengthThreeAlg (F)
assert ( e_1*e_2 == y*f_1 )
assert ( e_1*f_4 == -x*g_1 )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^2,x*y,z^2,y*z,z^2)
assert( resLengthThreeTorAlgClass(I) === "B" )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u*v,w*x,y*z)
assert( resLengthThreeTorAlgClass(I) === "C(3)" )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^3,x^2*z,x*(z^2+x*y),z^3-2*x*y*z,y*(z^2+x*y),y^2*z,y^3)
assert( resLengthThreeTorAlgClass(I) === "G(7)" )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y^2,x*y*z,y*z^2,x^4-y^3*z,x*z^3-y^4)
assert( resLengthThreeTorAlgClass(I) === "G(2)" )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^2,x*y^2)*ideal(y*z,x*z,z^2)  
assert( resLengthThreeTorAlgClass(I) === "H(0,0)" )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^2,x*y^2)*ideal(y*z,x*z,z^2)  
assert( resLengthThreeTorAlgClass(I) === "H(0,0)" )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^3,y^3,z^3,x*y*z)  
assert( resLengthThreeTorAlgClass(I) === "T" )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^5,y^5,x*y^4,x^2*y^3,x^3*y^2,x^4*y,z^3)  
assert( resLengthThreeTorAlgClass(I) === "H(6,5)" )
///

--==========================================================================
-- DOCUMENTATION
--==========================================================================

beginDocumentation()

document{
  Key => ResLengthThree,
  
  Headline => "Computation of multiplicative structures on free resolutions of length three",

  PARA { "Let ", EM  "I ", " be a homogeneous ideal contained in the irrelevant
      maximal ideal of a graded ring ", EM "Q ", " (obtained as a quotient of a
      polynomial ring). If the length of the minimal free resolution ",
      EM "F ", " of ", TEX /// $R=Q/I$ ///, " is 3, then it carries a structure of a
      differential graded algebra. The induced algebra structure on ",
      TEX /// $A = Tor_Q^*(R,k)$ ///, " is unique and provides for a classification of
      such quotient rings.  The package determines a multiplicative
      structure on the free resolution ", EM "F ", " as well as the unique induced
      structure on ", EM "A."}
      }

document{
  Key => {
    resLengthThreeAlg, (resLengthThreeAlg, ChainComplex ), (resLengthThreeAlg, ChainComplex, List )
    },

  Headline => "the minimal free resolution presented as a graded-commutative ring",

  Usage => "resLengthThreeAlg F, resLengthThreeAlg(F,L)", 

  Inputs =>{
      "F" => ChainComplex => "a length three free resolution of a cyclic module",
      "L" => List => "of three symbols"
      },
  
  Outputs => { 
      QuotientRing => "the resolution presented as a quotient of a graded-commutative free algebra
  over the ambient ring"},

  PARA { "For a free resolution ", TT "F", " over a ring ", TT "Q", ", the function returns
  the resolution ", TT "F", " as a quotient of a graded-commutative free algebra
  over ", TT "Q", ". The basis vectors in degrees 1, 2, and 3 are named with the
  symbols from the list ", TT "L", ". The defaul symbols are ", TT "e", ", ", TT "f", ", and ", TT "g", "." },
  
  EXAMPLE {
	"Q = QQ[x,y,z];",
	"A = resLengthThreeAlg res ideal (x^2,y^2,z^2)",
	"describe A",
	"e_1*e_2",
	"e_1*f_2",
	"e_1*f_3",	
	"f_1*f_2",	
	},

  PARA { "The ambient ring ", TT "Q", " does not need to be a polynomial algebra." },

  EXAMPLE {
	"P = QQ[u,v,x,y,z];",
	"Q = P/ideal(u^2,u*v);",
	"F = resLengthThreeAlg ( res ideal (x^2,x*y,y^2,z^2), {a,b,c} )",
	"describe F",
	"a_1*a_4",
	"a_2*a_4",
	"a_3*a_4"
	},

  PARA { },
  
  EXAMPLE {
	"P = QQ[u,v];",
	"Q = (P/ideal(u^2,u*v))[x,y,z];",
	"A = resLengthThreeAlg ( res ideal (x^2,x*y,y^2,z^2))",
	"describe A",
	},

  PARA { },
  
  EXAMPLE {
      "P = ZZ[x,y,z];",
      "Q = P/ideal(4_P);",
      "A = resLengthThreeAlg ( res ideal (x^2,y^2,z^2) )",
      "describe A"
	},
        
Caveat => { "The ambient ring ", TT "Q ", "must be homogeneous." }
}  

document{
  Key => {
    resLengthThreeTorAlg, (resLengthThreeTorAlg, ChainComplex ), (resLengthThreeTorAlg, ChainComplex, List )
    },

  Headline => "the Tor algebra presented as a graded-commutative ring",

  Usage => "resLengthThreeTorAlg F, resLengthThreeTorAlg(F,L)", 

  Inputs =>{
      "F" => ChainComplex => "a length three free resolution of a cyclic module",
      "L" => List => "of three symbols"
      },
  
  Outputs => { 
      QuotientRing => "the Tor algebra presented as a quotient of a graded-commutative free algebra
  over the residue field of the ambient ring"},

  PARA { "For a free resolution ", TT "F", " over a ring ", TT "Q", ", the function returns
  the algebra ", TEX /// $Tor_Q^*(R,k)$ ///, " as a quotient of a graded-commutative free algebra
  over the residue field of ", TT "Q", ". The basis vectors in degrees 1, 2, and 3 are named with the
  symbols from the list ", TT "L", ". The defaul symbols are ", TT "e", ", ", TT "f", ", and ", TT "g", "." },
  
  EXAMPLE {
	"Q = QQ[x,y,z];",
	"A = resLengthThreeTorAlg res ideal (x^2,y^2,z^2)",
	"describe A",
	"e_1*e_2",
	"e_1*f_2",
	"e_1*f_3",	
	"f_1*f_2",	
	},

  PARA { "The ambient ring ", TT "Q", " does not need to be a polynomial algebra." },

-- These still don't return right...

  EXAMPLE {
	"P = QQ[u,v,x,y,z];",
	"Q = P/ideal(u^2,u*v);",
	"A = resLengthThreeAlg ( res ideal (x^2,x*y,y^2,z^2), {a,b,c} )",
	"describe A",
	"a_1*a_4",
	"a_2*a_4",
	"a_3*a_4"
	},

  PARA { },

  EXAMPLE {
	"P = QQ[u,v];",
	"Q = (P/ideal(u^2,u*v))[x,y,z];",
	"A = resLengthThreeTorAlg ( res ideal (x^2,x*y,y^2,z^2) )",
	"describe A",
	"a_1*a_4",
	"a_2*a_4",
	"a_3*a_4"
	},
    
    Caveat => { "For the function to return an algebra over the residue
	field of the ambient ring ", TT "Q ", "that ring must a homogeneous
	quotient of a polynomial algebra over a field."},

}

document{
  Key => {
    multTableOneOne, (multTableOneOne, Ring)
    },

  Headline => "the multiplication table for products of elements in degree one",

  Usage => "multTableOneOne A", 

  Inputs =>{
      "A" => Ring => { "created with ", TO resLengthThreeAlg, " or ", TO resLengthThreeTorAlg } 
      },
  
  Outputs => { 
      List => { "of the rows in the multiplication table; use ", TO netList, " to display it as a table" }
      },

  PARA { "For a length tree free resolution described as a
  graded-commutative ring ", TT "A", " the function returns a list of
  the rows of the table of products of elements in degree one. It does
  the same for the graded-commutative homology algebra obtained from
  ", TT "A", "." },
  
  EXAMPLE {
	"Q = QQ[x,y,z];",
	"A = resLengthThreeAlg res ideal (x^2,y^2,z^2)",
	"multTableOneOne A",
	"netList multTableOneOne A"
	},
}

document{
  Key => {
    [multTableOneOne, Labels],
    },

  Headline => "an optional argument for multTableOneOne determining whether to label rows and columns",

  Usage => "multTableOneOne A", 

  Inputs =>{
      "A" => Ring => { "created with ", TO resLengthThreeAlg, " or ", TO resLengthThreeTorAlg } 
      },
  
  Outputs => { 
      Matrix => { "of the rows in the multiplication table." }
      },

  PARA { "The default value of ", TO Labels, " is ", TO true, ". Changing the value to ", TO false, " removes the row and column labels." },
  
  EXAMPLE {
	"Q = QQ[x,y,z];",
	"A = resLengthThreeAlg res ideal (x^2,y^2,z^2)",
	"multTableOneOne (A, Labels => false)",
	},
}

document{
  Key => {
    [multTableOneOne, Compact], Compact
    },

  Headline => "an optional argument for multTableOneOne that sets the products of e_ie_j to be zero for i>j" ,

  Usage => "multTableOneOne A", 

  Inputs =>{
      "A" => Ring => { "created with ", TO resLengthThreeAlg, " or ", TO resLengthThreeTorAlg } 
      },
  
  Outputs => { 
      List => { "of the rows in the multiplication table; use ", TO netList, " to display it as a table" }
      },

  PARA { "The default value of ", TO Compact, " is ", TO false, ". Changing the value to ", TO true, " sets the products ",  TEX /// $e_ie_j=0$ ///, " for ",  TEX /// $i>j$ ///  },
  
  EXAMPLE {
	"Q = QQ[x,y,z];",
	"A = resLengthThreeAlg res ideal (x^2,y^2,z^2)",
	"multTableOneOne (A, Compact=> true)",
	"netList multTableOneOne(A, Compact=>true)",
	},
}

document{
  Key => {
    multTableOneTwo, (multTableOneTwo, Ring)
    },

  Headline => "the multiplication table for products of elements in degree one with elements in degree two",

  Usage => "multTableOneTwo A", 

  Inputs =>{
      "A" => Ring => { "created with ", TO resLengthThreeAlg, " or ", TO resLengthThreeTorAlg } 
      },
  
  Outputs => { 
      List => { "of the rows in the multiplication table; use ", TO netList, " to display it as a table" }
      },

  PARA { "For a length tree free resolution described as a
  graded-commutative ring ", TT "A", " the function returns a list of
  the rows of the table of products of elements in degree one with elements in degree two. It does
  the same for the graded-commutative homology algebra obtained from
  ", TT "A", "." },
  
  EXAMPLE {
	"Q = QQ[x,y,z];",
	"A = resLengthThreeAlg res ideal (x^2,y^2,z^2)",
	"multTableOneTwo A",
	"netList multTableOneTwo A"
	},
}

document{
  Key => {
   Labels
    },

  Headline => "an optional argument for multTableOneOne and MultTableOneTwo determining whether to label rows and columns",

  Usage => "multTableOneOne A", "multTableOneTwo A",

  Inputs =>{
      "A" => Ring => { "created with ", TO resLengthThreeAlg, " or ", TO resLengthThreeTorAlg } 
      },
  
  Outputs => { 
      Matrix => { "of the rows in the multiplication table. If used with multTableOneOne." },
      List => { "of the rows in the multiplication table. If used with multTableOneTwo. Use ", TO netList, " to display it as a table" }
      },

  PARA { "The default value of ", TO Labels, " is ", TO true, ". Changing the value to ", TO false, " removes the row and column labels." },
  
  EXAMPLE {
	"Q = QQ[x,y,z];",
	"A = resLengthThreeAlg res ideal (x^2,y^2,z^2)",
	"multTableOneOne (A, Labels => false)",
	"netList multTableOneTwo (A, Labels=>false)",
	},
}

document{
  Key => {
    [multTableOneTwo, Labels],
    },

  Headline => "an optional argument for multTableOneTwo determining whether to label rows and columns",

  Usage => "multTableOneTwo A", 

  Inputs =>{
      "A" => Ring => { "created with ", TO resLengthThreeAlg, " or ", TO resLengthThreeTorAlg } 
      },
  
  Outputs => { 
      List => { "of the rows in the multiplication table. Use ", TO netList, " to display it as a table" }
      },

  PARA { "The default value of ", TO Labels, " is ", TO true, ". Changing the value to ", TO false, " removes the row and column labels." },
  
  EXAMPLE {
	"Q = QQ[x,y,z];",
	"A = resLengthThreeAlg res ideal (x^2,y^2,z^2)",
	"netList multTableOneTwo (A, Labels => false)",
	},
}


document{
  Key => {
    resLengthThreeTorAlgClass, (resLengthThreeTorAlgClass, ChainComplex),(resLengthThreeTorAlgClass, Ideal),
    },

  Headline => "the class (w.r.t. multiplication in homology) of an ideal",

  Usage => "resLengthThreeTorAlgClass F", 

  Inputs =>{
      "F" => ChainComplex => { "a length three free resolution of a cyclic module" } ,
      "I" => Ideal => {"an ideal of codepth 3"},
      },
  
  Outputs => { 
      String => { "the (parametrized) class of the ideal I"}
      },

  PARA { "Classifies the ideal  ", TEX /// $I$ ///, "  as belonging to one of the (parametrized) classes ", BOLD /// B ///,", ",BOLD /// C///,"(c), ",BOLD /// G///,"(r), ",BOLD ///H///,"(p,q) ,
     ", BOLD ///T///,", provided that it is codepth 3." },
  
  EXAMPLE {
	"Q = QQ[x,y,z];",
        "resLengthThreeTorAlgClass ideal(x*y,x^2,y*z,z^2)",
	"resLengthThreeTorAlgClass ideal(x^2,y^2,z^2)",
	"resLengthThreeTorAlgClass ideal(x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3)",
	"resLengthThreeTorAlgClass ideal(x*z+y*z,x*y+y*z,x^2-y*z,y*z^2+z^3,y^3-z^3)",
	"resLengthThreeTorAlgClass ideal(x^2,y^2,z^2,x*z)",
	"resLengthThreeTorAlgClass ideal(x^2,y^2,z^2,x*y*z)",
	},
    
  Caveat => { "The codepth of the ideal ", TEX///I///," must be exactly 3, and the length of the complex ",TEX///F///,
      " must be exactly 3."},
}


document{
  Key => {
    makeRes,
    },

  Headline => "creates a resolution starting from three matrices",

  Usage => "makeRes(d1,d2,d3)", 

  Inputs =>{
      "d1" => Matrix => {"matrix of the differential in degree 1"} ,
      "d2" => Matrix => {"matrix of the differential in degree 2"},
      "d3" => Matrix => {"matrix of the differential in degree 3"},
      },
  
  Outputs => { 
      ChainComplex => { "the resolution with differentials d1, d2, d3."}
      },
  
    PARA { "Creates a resolution of length 3 that has the given three matrices as differentials." },

EXAMPLE {
	"Q = QQ[x,y,z];",
        "d1=matrix{{-x^2,z^2-x*y,-y^2,-x*z,-y*z}}",
	"d2=matrix{{0,0,z,0,-y},{0,0,0,-y,x},{-z,0,0,x,0},{0,y,-x,0,z},{y,-x,0,-z,0}}",
	"d3=transpose d1",
	"makeRes(d1,d2,d3)",
	},
}


end
--==========================================================================
-- end of package code
--==========================================================================

uninstallPackage "ResLengthThree"
restart
installPackage "ResLengthThree"
debug loadPackage "ResLengthThree"
check "ResLengthThree"

-- dev space

P = ZZ[x,y,z];
Q = P/ideal(4_P)
A = resLengthThreeAlg ( res ideal (x^2,y^2,z^2) )
describe A


needsPackage "TorAlgebra"'
needsPackage "PruneComplex"

P = ZZ[w,x,y,z]
Q = P/ideal(4_P)
I = ideal (x^2,y^2,z^2)
A = resLengthThreeTorAlg res I
describe A

Q = QQ[u,v,x,y,z];
R = Q/ideal(u^2,u*v)
I = ideal (x^2,y^2,z^2)
F = res I
G = resLengthThreeAlg F
A = resLengthThreeTorAlg F
resLengthThreeTorAlgClass I
netList multTableOneOne G
netList multTableOneTwo G
netList multTableOneOne A
netList multTableOneTwo A


Q = QQ[u,v,x,y,z];
R = Q/ideal(u^2-u*v^2)
I = ideal (x^2,y^2,z^2)
F = res I
resLengthThreeTorAlgClass I

P = QQ[u,v];
Q = P/ideal(u^2-u*v)
R = Q[x,y,z]
I = ideal (x^2,y^2,z^2)
G = resLengthThreeAlg res I
netList multTableOneOne G
netList multTableOneTwo G
A = resLengthThreeTorAlg res I
netList multTableOneOne A
netList multTableOneTwo A

P = QQ[u,v];
Q = P/ideal(u^2-u*v^2)
R = Q[x,y,z]
I = ideal (x^2,y^2,z^2)
G = resLengthThreeAlg res I
netList multTableOneOne G
netList multTableOneTwo G
A = resLengthThreeTorAlg res I
netList multTableOneOne A
netList multTableOneTwo A

rank multMap(A,1,1)
basis(1,A)
resLengthThreeTorAlgClass I

restart
debug needsPackage "ResLengthThree"
Q = ZZ/101[x,y,z];
time for s1 from 2 to 5 do (
  for s2 from s1 to 2*s1 do (
      I := ideal fromDual matrix {{ random(s1,Q), random(s2,Q) }};
      F := res I;
      d1 := F.dd_1;
      d2 := F.dd_2;
      d3 := F.dd_3;
      mult11 := (d1**(id_(source d1)) - (id_(source d1))**d1) // d2;
      mult12 :=  - d1**(id_(source d2))) // d3 + -(mult11 * (id_(source d1)**d2)
      --time multTables res I;
      --time multTables' res I;
  );
);

restart
debug needsPackage "ResLengthThree"
Q = ZZ/32003[x,y,z];
I = ideal fromDual matrix {{random(5,Q), random(10,Q)}};
--I = ideal (x^3,y^3,z^3)
F = res I;
time B = resLengthThreeAlg F
time C = resLengthThreeAlg' F
time resLengthThreeTorAlgClass F
time resLengthThreeTorAlgClass' F

A = ambient B
d1 = F.dd_1;
d2 = F.dd_2;
d3 = F.dd_3;
m = rank source d1
l = rank source d2
n = rank source d3
mult11 = time (d1**(id_(source d1)) - (id_(source d1))**d1) // d2;
mult12 = time (d1**(id_(source d2)) - mult11 * (id_(source d1)**d2)) // d3;
EE = hashTable flatten apply(m, i -> apply(m, j -> ((i+1,j+1), mult11_{m*i+j})));
EF = hashTable flatten apply(m, i -> apply(l, j -> ((i+1,j+1), mult12_{m*i+j})));
mults = time multTables F;
netList sort pairs first mults | netList sort pairs EE
netList sort pairs last mults | netList sort pairs EF
eVector = matrix {{e_1,e_2,e_3}}
fVector = matrix {{f_1,f_2,f_3}}
gVector = matrix {{g_1}}
flatten entries (matrix {flatten entries (-((transpose eVector) * eVector))} - fVector*mult11)
e1
e2
e3 * ( e1 e2 e3 )
--  F1 ** F2 --> F1 ** F1 ++ F2 --mult and add--> F2

--===================================================================================================
-- TESTS
--===================================================================================================

-- #0 zero ring, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( promote(1,Q) )
assert( torAlgClass(Q/I) === "zero ring" )
///
