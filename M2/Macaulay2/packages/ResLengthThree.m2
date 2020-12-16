-* 
Copyright 2020 Lars Winther Christensen, Luigi Ferraro, Francesca
Gandini, Frank Moore, and Oana Veliche.

You may redistribute this file under the terms of the GNU General Public
License as published by the Free Software Foundation, either version 2 of
the License, or any later version.
*-

newPackage ( "ResLengthThree",
    Version => "1.0",
    Date => "3 December 2020",
    Authors => {
	{ Name => "Lars Winther Christensen",
	  Email => "lars.w.christensen@ttu.edu",
	  HomePage => "http://www.math.ttu.edu/~lchriste/index.html" },
      { Name => "Luigi Ferraro",
	  Email => "lferraro@ttu.edu",
	  HomePage => "http://www.math.ttu.edu/~lferraro" },
	{ Name => "Francesca Gandini",
	  Email => "fra.gandi.phd@gmail.com",
	  HomePage => "https://github.com/fragandi" },
	{ Name => "Frank Moore",
	  Email => "moorewf@wfu.edu",
	  HomePage => "http://users.wfu.edu/moorewf/" },
	{ Name => "Oana Veliche", 
	  Email => "o.veliche@northeastern.edu",
	  HomePage => "https://web.northeastern.edu/oveliche/index.html" }
	},
    Headline => "Multiplication in free resolutions of length three",
    Reload => false,
    DebuggingMode => false,
    Keywords => { "Homological Algebra" }
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
   eVector := matrix {apply(m, i -> P_(i))};
   fVector := matrix {apply(l, i -> P_(m+i))};
   gVector := matrix {apply(n, i -> P_(m+l+i))};

   eeGens := flatten entries (matrix {flatten entries (-((transpose eVector) * eVector))} - fVector*(mult#0));
   efGens := flatten entries (matrix {flatten entries (((transpose eVector) * fVector))} - gVector*(mult#1));

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

makeRes = (d1,d2,d3) -> ( 
    -- first check that the differentials are ok
    if ( (image matrix entries gens ker d1) != image matrix entries gens image d2 or 
	(image matrix entries gens ker d2) != image matrix entries gens image d3 or
	ker d3 !=0) then error "Expected differentials of resolution of length three";
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
   oneTimesOneA := table(m,m, (i,j) -> if i <= j then (A_i)*(A_j) else if opts.Compact then "." else (A_i)*(A_j));
   topLine := {{" "} | flatten entries eVector};
   sideLine := entries transpose eVector;
   result := (topLine | apply(sideLine,oneTimesOneA, (i,j) -> i | j));
   
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
    
    multEE := (matrix entries (d1**(id_(source d1)) - (id_(source d1))**d1)) // d2;
    multEF := (matrix entries (d1**(id_(source d2)) - multEE * (id_(source d1)**d2))) // d3;

    {multEE,multEF}
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


--===================================================================================================
-- TESTS
--===================================================================================================

TEST ///
Q = QQ[x,y,z];
F = res ideal (x*y, y*z, x^3, y^3-x*z^2,x^2*z,z^3);
G = resLengthThreeAlg F;
assert ( e_1*e_2 == y*f_1 )
assert ( e_1*f_4 == -x*g_1 )
///

TEST ///
Q = QQ[x,y,z];
F = res ideal (x*y, y*z, x^3, y^3-x*z^2,x^2*z,z^3);
G = resLengthThreeAlg F
assert ( e_1*e_2 == y*f_1 )
assert ( e_1*f_4 == -x*g_1 )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^2,x*y,z^2,y*z,z^2)
assert( resLengthThreeTorAlgClass I === "B" )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u*v,w*x,y*z)
assert( resLengthThreeTorAlgClass I === "C(3)" )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^3,x^2*z,x*(z^2+x*y),z^3-2*x*y*z,y*(z^2+x*y),y^2*z,y^3)
assert( resLengthThreeTorAlgClass I === "G(7)" )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y^2,x*y*z,y*z^2,x^4-y^3*z,x*z^3-y^4)
assert( resLengthThreeTorAlgClass I === "G(2)" )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^2,x*y^2)*ideal(y*z,x*z,z^2)  
assert( resLengthThreeTorAlgClass res I === "H(0,0)" )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^2,x*y^2)*ideal(y*z,x*z,z^2)  
assert( resLengthThreeTorAlgClass I === "H(0,0)" )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^3,y^3,z^3,x*y*z)  
assert( resLengthThreeTorAlgClass I === "T" )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^5,y^5,x*y^4,x^2*y^3,x^3*y^2,x^4*y,z^3)  
assert( resLengthThreeTorAlgClass I === "H(6,5)" )
///

TEST ///
Q = QQ[x,y,z]
d1 = matrix{{-x^2,z^2-x*y,-y^2,-x*z,-y*z}}
d2 = matrix{{0,0,z,0,-y},{0,0,0,-y,x},{-z,0,0,x,0},{0,y,-x,0,z},{y,-x,0,-z,0}}
d3 = transpose d1
F = makeRes(d1,d2,d3)
A = resLengthThreeAlg F 
assert(e_2*e_4===y*f_3+z*f_5)
assert(e_1*e_2===-z*f_3-x*f_5)
assert(e_3*e_5===-y*f_1)
T = resLengthThreeTorAlg F
assert(e_2*e_4===sub(0,T))
assert(e_1*e_2===sub(0,T))
assert(e_3*e_5===sub(0,T))
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x*y, y*z, x^3, y^3-x*z^2,x^2*z, z^3)
G = resLengthThreeAlg res I
assert( e_1*e_2 == y*f_1 )
assert( e_1*e_3 == x*f_2 )
assert( e_1*e_4 == -x*z*f_1 + y*f_3 - z*f_5 )
assert( e_1*e_5 == x^2*f_1 + x*f_5 )
assert( e_1*e_6 == z^2*f_1 + x*f_7 )
assert( e_1*f_1 == 0 )
assert( e_1*f_2 == 0 )
assert( e_1*f_3 == 0 )
assert( e_1*f_4 == -x*g_1 )
assert( e_1*f_5 == 0 )
assert( e_1*f_6 == g_2 )
assert( e_1*f_7 == 0 )
///

--==========================================================================
-- DOCUMENTATION
--==========================================================================

beginDocumentation()

document{
  Key => {ResLengthThree},
  
  Headline => "Computation of multiplicative structures on free resolutions of length three",

  PARA { "Let ", EM "I ", " be a homogeneous ideal contained in the 
      irrelevant maximal ideal of a graded ring ", EM "Q ", "
      (obtained as a quotient of a polynomial ring). If the length of
      the minimal free resolution ", EM "F ", " of ", TEX /// $R=Q/I$
      ///, " is 3, then the resolution admits the structure of a differential
      graded algebra. The induced algebra structure on ", TEX /// $A =
      Tor^Q(R,k)$ ///, " is unique and provides for a classification
      of such quotient rings.  The package determines a multiplicative
      structure on the free resolution ", EM "F ", " as well as the
      unique induced structure on ", EM "A ", "and the class of the
      quotient ", EM "R ", "according to the classification scheme
      of ", HREF{"https://doi.org/10.1016/0021-8693(88)90056-7","Avramov, Kustin, and Miller"},"." }
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
  symbols from the list ", TT "L", ". The default symbols are ", TT "e", ", ", TT "f", ", and ", TT "g", "." },
  
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
	},

  PARA { },
  
  EXAMPLE {
	"P = QQ[u,v];",
	"Q = (P/ideal(u^2,u*v))[x,y,z];",
	"A = resLengthThreeAlg res ideal (x^2,x*y,y^2,z^2)",
	"describe A",
	},

  PARA { },
  
  EXAMPLE {
      "P = ZZ[x,y,z];",
      "Q = P/ideal(4_P);",
      "A = resLengthThreeAlg res ideal (x^2,y^2,z^2)",
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
  the algebra ", TEX /// $Tor^Q(R,k)$ ///, " as a quotient of a graded-commutative free algebra
  over the residue field of ", TT "Q", ". The basis vectors in degrees 1, 2, and 3 are named with the
  symbols from the list ", TT "L", ". The default symbols are ", TT "e", ", ", TT "f", ", and ", TT "g", "." },
  
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

  EXAMPLE {
	"P = QQ[u,v,x,y,z];",
	"Q = P/ideal(u^2,u*v);",
	"A = resLengthThreeTorAlg ( res ideal (x^2,x*y,y^2,z^2), {a,b,c} )",
	"describe A",
	},

  PARA { },

  EXAMPLE {
	"P = QQ[u,v];",
	"Q = (P/ideal(u^2,u*v))[x,y,z];",
	"A = resLengthThreeTorAlg ( res ideal (x^2,x*y,y^2,z^2), {a,b,c} )",
	"describe A",
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

  PARA { "For a free resolution of length three described as a
  graded-commutative ring ", TT "A", ", the function returns a list of
  the rows of the multiplication table of elements in degree one. It also computes
  the multiplication table for products of elements in degree one in the
   graded-commutative homology algebra obtained from
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
      List => { "of the rows in the multiplication table; use ", TO netList, " to display it as a table" }
      },

  PARA { "The default value of ", TO Labels, " is ", TO true, ". Changing the value to ", TO false, " removes the row and column labels." },
  
  EXAMPLE {
	"Q = QQ[x,y,z];",
	"A = resLengthThreeAlg res ideal (x^2,y^2,z^2)",
	"netList multTableOneOne (A, Labels => false)",
	},
}

document{
  Key => {
    [multTableOneOne, Compact], Compact
    },

  Headline => "an optional argument for multTableOneOne that prints dots below the diagonal" ,

  Usage => "multTableOneOne A", 

  Inputs =>{
      "A" => Ring => { "created with ", TO resLengthThreeAlg, " or ", TO resLengthThreeTorAlg } 
      },
  
  Outputs => { 
      List => { "of the rows in the multiplication table; use ", TO netList, " to display it as a table" }
      },

  PARA { "The default value of ", TO Compact, " is ", TO false, ". Changing the value to ", TO true, " saves space by printing dots for the products ",  TEX /// $e_ie_j$ ///, " for ",  TEX /// $i>j$ ///  },
  
  EXAMPLE {
	"Q = QQ[x,y,z];",
	"A = resLengthThreeAlg res ideal (x^2,y^2,z^2)",
	"multTableOneOne (A, Compact => true)",
	"netList multTableOneOne(A, Compact => true)",
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

  PARA { "For a free resolution of length three described as a
  graded-commutative ring ", TT "A", ", the function returns a list of
  the rows of the multiplication table of elements in degree one with elements in degree two. 
  It also computes
  the multiplication table for products of elements in degree one with elements in degree two 
  in the graded-commutative homology algebra obtained from
  ", TT "A", "."  },
  
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
      List => { "of the rows in the multiplication table, use ", TO netList, " to display it as a table" }
      },

  PARA { "The default value of ", TO Labels, " is ", TO true, ". Changing the value to ", TO false, " removes the row and column labels." },
  
  EXAMPLE {
	"Q = QQ[x,y,z];",
	"A = resLengthThreeAlg res ideal (x^2,y^2,z^2)",
	"netList multTableOneOne (A, Labels => false)",
	"netList multTableOneTwo (A, Labels => false)",
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
        "resLengthThreeTorAlgClass ideal (x*y,x^2,y*z,z^2)",
	"resLengthThreeTorAlgClass ideal (x^2,y^2,z^2)",
	"resLengthThreeTorAlgClass ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3)",
	"resLengthThreeTorAlgClass ideal (x*z+y*z,x*y+y*z,x^2-y*z,y*z^2+z^3,y^3-z^3)",
	"resLengthThreeTorAlgClass ideal (x^2,y^2,z^2,x*z)",
	"resLengthThreeTorAlgClass ideal (x^2,y^2,z^2,x*y*z)",
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
      "d1" => Matrix => {"of the differential in degree 1"} ,
      "d2" => Matrix => {"of the differential in degree 2"},
      "d3" => Matrix => {"of the differential in degree 3"},
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
--===============================d===========================================

uninstallPackage "ResLengthThree"
restart
installPackage "ResLengthThree"
debug loadPackage "ResLengthThree"
check "ResLengthThree"
viewHelp "ResLengthThree"
