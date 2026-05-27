-- -*- coding: utf-8 -*-

newPackage(

    "ResultantComplexes",

    Version => "1.0",
    Date => "August 16, 2025",
    Authors => { {Name => "Friedemann Groh" }},
    Headline => "calculates a resultants using subdivisions and Canny Emiris shift of Newton polytopes",
    Keywords => {"Toric Geometry"},
    DebuggingMode => false,
    PackageImports => {"Polyhedra","LLLBases","Complexes"},
    Certification => {
	"journal name" => "Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "Resultant complexes of toric systems",
	"acceptance date" => "2025-08-16",
	"published article URI" => "https://msp.org/jsag/2026/16-1/p01.xhtml",
	"published article DOI" => "10.2140/jsag.2026.16.1",
	"published code URI" => "https://msp.org/jsag/2026/16-1/jsag-v16-n1-x01-ResultantComplexes.zip",
	"version at publication" => "1.0",
	"volume number" => "16",
	"volume URI" => "https://msp.org/jsag/2026/16-1/"
	}
    )

export {"calcResultant","resultantComplex","CayleyFormula","mixedSubdivision","randomLift","CannyEmirisCoef","polynomials"}

importFrom(Core, "concatCols");

------------------------------------------------------------------------------------------------------------
-- utility functions
------------------------------------------------------------------------------------------------------------

-- Cayley embedding of the family of supports 
cayleyMatrix = grad -> ( n := numgens target grad#0;
      
      concatCols( for k from 0 to n list grad#k || concatCols( toList( numgens source grad#k: matrix id_(ZZ^(n+1))_k) ) ) );


-- get sparse polynomial by its exponents, which is the support set

polynomials = method()

polynomials (List,List,List) := (c, x, supp) -> (
  
  -- indexes of coefficients in sparse polynomials
  m := {0} | accumulate( plus, 0, supp / (i -> #i) );

  for i from 0 when i < #supp list(  
  
    sum( for j from 0 when j < #(supp#i) list c#(m#i+j) * product( for k from 0 when k < #x list x#k ^ (supp#i#j#k))) ));


-- cells of mixed subdivision of list of supports with lift vector (combined)

mixedSubdivision = method()

mixedSubdivision (List,Matrix) := (grad,Lift) -> (
  
   n := numgens target grad#0;
   Cmatrix := cayleyMatrix grad;
   tria := regularSubdivision( Cmatrix^{0..<(2*n)}, Lift );
   term := flatten apply( grad, i -> for j from 0 when j < numgens source i list j );

   table( #tria, toList(n..2*n), (k,i) -> ((term_(tria#k))_(positions( flatten entries Cmatrix^{i}_(tria#k), m -> m == 1 )))) );


-- get random lift vectors for coherent sub-divisions polytope vertices

randomLift = method()

randomLift (List,ZZ) := (f,m) -> ( for k from 0 when k < #f list (
  
  for i from 0 when i < (numgens source vertices newtonPolytope f#k) list random(-m,0)) );


------------------------------------------------------------------------------------------------------------
-- Canny Emiris translation as shift of degrees
------------------------------------------------------------------------------------------------------------

CannyEmirisCoef = method()

CannyEmirisCoef (Sequence,Matrix) := ( facetP, shiftP ) -> (
  
  n := numgens target shiftP;

  for i from 0 when i < numgens target facetP#0 list(

      H := hermite ( lift( matrix facetP#0^{i}, ZZ ), ChangeMatrix => true );
      q := det( H#1_{0..n-2} |  shiftP ) / det( H#1 ); 
                      
      -- check the Canny Emiris shift vector: 
      if q == round(q) then error "invalid Canny Emiris vector: lattice points meet shifted facet of a cell";

      -- calculate coefficient of Canny Emiris vector by Cramers rule 
      if (entries H#0_(n-1))#0 > 0 then floor(q) else floor(-q) ) );


------------------------------------------------------------------------------------------------------------
-- lattice points in shifted polytopes determine terms of the complex
------------------------------------------------------------------------------------------------------------

basisComplex = ( grad, cell, shiftP ) -> (

  -- characters inside shifted cell domains specify all terms in the Koszul complex
  n := numgens target grad#0;
  mon := new MutableList;
  choice := new MutableList;

  -- Newton polytopes of cell systems
  cellPolytope := for j from 0 when j < #cell list apply( for i from 0 to n list grad#i_(cell#j#i), convexHull );

  -- dimension of Newton polytopes in each cell
  type := for j from 0 when j < #cell list apply( cellPolytope#j, i -> dim i );

  -- check if sub-division is tight (TCMD)
  if apply( type, i -> sum i ) != toList (#cell:n) then error "the lift vector didn't produce a tight mixed sub-division"; 

  mon#0 = for j from 0 when j < #cell list( 

            facetP := facets fold( minkowskiSum, cellPolytope#j );

            -- Canny Emiris translation as shift of degrees 
            degP := CannyEmirisCoef( facetP, shiftP );

            -- Polytope reduced by Canny Emiris shift
            redP := polyhedronFromHData( facetP#0, facetP#1 + (transpose matrix {degP} ) ); 
            
            if isEmpty redP then {} else latticePoints( redP ) );

  -- lattice points in partial Minkowski sums form bases of the other terms

  for r from 1 to n do(

      -- index of r-forms in Koszul complex
      comb := sort subsets( toList(0..n), r ); 

      -- characters inside shifted cell domains
      mon#r = for s from 0 when s < #comb list( flatten for k from 0 when k < #cell list (

                  if type#k_(comb#s) == toList(r:0) then (

                      -- sum of vertices to translate domain
                      vertexP := matrix ( sum ( for i from 0 when i < r list grad#(comb#s#i)_(cell#k#(comb#s#i)) ) );
                      
                      for i from 0 when i < #(mon#0#k) list mon#0#k#i - vertexP

                  ) else continue
              ) );

      -- choose first vertex in each cell, Canny Emiris strategy to determine regular minors
      choice#r = for s from 0 when s < #comb list( flatten for k from 0 when k < #cell list (
              
                    if type#k_(comb#s) == toList(r:0) then ( 

                        -- choose first vertex in each cell, Canny Emiris strategy to determine regular minors
                        choose := member( position( type#k, i -> i == 0 ), comb#s );

                        for i from 0 when i < #(mon#0#k) list choose
                    
                    ) else continue ) );
  );

  -- combine all cells:
  mon#0 = {flatten mon#0};

  sequence(toList mon, toList choice) );

------------------------------------------------------------------------------------------------------------
-- determine differentials in resultant complex
------------------------------------------------------------------------------------------------------------

differentialsComplex = ( mon, L, A  ) -> (

  -- indexing wedge products in Koszul complex
  idx := hashTable( {{} => 0} );

  -- family of support sets of sparse families
  grad := for k from 0 when k < #L list transpose matrix ( apply (L#k, i->i#0) );
  n := numgens target grad#0;

  -- sub-blocks of rows in the differentials
  block := for j from 0 to n list {0} | accumulate( (a,b) -> a+b, {0} | for i from 0 when i < #mon#j list #mon#j#i );

  complex for r from 1 to n list(

      -- Define sub-matrices of the first differential of resultant complex
      D := new MutableList;

      -- index of r-forms in Koszul complex
      comb := sort subsets( toList(0..n), r ); 

      for s from 0 when s < #(mon#r) do(

        D#s = mutableMatrix( A, #(flatten mon#(r-1)), #(mon#r#s) );

        -- sub-matrix of the differential                
        table( #(mon#r#s), #comb#s, (j,k) -> ( t := idx#(comb#s - set {comb#s#k});
        
                for m from 0 when m < #L#(comb#s#k) do (                                  
                  D#s_( block#(r-1)#t + position( mon#(r-1)#t, i -> ( mon#r#s#j + matrix grad#(comb#s#k)_m ) == i), j ) = (-1)^k * L#(comb#s#k)#m#1; ); 
                )
              )                
      );

      idx = hashTable( for i from 0 when i < #comb list comb#i => i );

      -- concatenate the sub-matrices to differential
      map( A^(#(flatten mon#(r-1))), A^#(flatten mon#(r)), concatCols( toList D / matrix )) )      
);

------------------------------------------------------------------------------------------------------------
-- calculate resultant complex
------------------------------------------------------------------------------------------------------------

resultantComplex = method()

resultantComplex (List,List,List) := ( f, Lift, shiftP ) -> (

  -- check dimension of inputs 
  if #f != #shiftP + 1 then error "dimension of shift vector does not match supports";

  -- ring of coefficients:
  A := coefficientRing ring(f#0);

  -- get Newton polytopes of input polynomials to get their vertices and check dimension 
  Q := f / newtonPolytope;

  if #shiftP != dim fold(minkowskiSum, Q) then error "Minkowski sum of Newton polytope is not full dimensional";

  -- vertices of Newton polynomials: 
  vertex := apply( Q, i -> lift(vertices i,ZZ) );

  -- check if lift function is defined on vertices
  if apply( vertex, i -> numColumns i) != apply( Lift, i -> #i) then error "lift function does not match the vertices of the Newton polytopes";

  -- sub-divide vertices of Newton polynomials:
  cell := mixedSubdivision( vertex, matrix {flatten Lift} );

  -- characters inside shifted cell domains specify all modules in the Koszul complex 
  (mon,choice) := basisComplex( vertex, cell, transpose matrix {shiftP} );

  -- determine resultant complex
  complexRes := differentialsComplex( mon, apply( f, i -> listForm i ), A ); 

  complexRes, toList mon, toList choice
);

-- input support set of algebraic system
resultantComplex (PolynomialRing,List,List,List) := ( A, supp, Lift, shiftP ) -> (

  -- check dimension of inputs 
  if #supp != #shiftP + 1 then error "dimension of shift vector does not match supports";

  -- get Newton polytopes of input polynomials to get their vertices and check dimension
  Q := supp / matrix / transpose / convexHull;

  if #shiftP != dim fold(minkowskiSum, Q) then error "Minkowski sum of Newton polytope is not full dimensional";

  -- vertices of Newton polynomials: 
  vertex := apply( Q, i -> lift(vertices i,ZZ) );

  -- check if lift function is defined on vertices
  if apply( vertex, i -> numColumns i) != apply( Lift, i -> #i) then error "lift function does not match the vertices of the Newton polytopes";

  -- sub-divide vertices of Newton polynomials:
  cell := mixedSubdivision( vertex, matrix {flatten Lift} );

  -- characters inside shifted cell domains specify all modules in the Koszul complex 
  (mon,choice) := basisComplex( vertex, cell, transpose matrix {shiftP} );

  -- indexes of coefficients in sparse polynomials and generate output of M2-listForm 
  m := {0} | accumulate( plus, 0, supp / (i -> #i) );
  suppCoef := for i from 0 when i < #supp list for j from 0 when j < #supp#i list {supp#i#j,(gens A)_(m#i+j)};

  -- determine resultant complex
  complexRes := differentialsComplex( mon, suppCoef, A ); 

  complexRes, toList mon, toList choice
);


------------------------------------------------------------------------------------------------------------
-- select rows and columns of regular minors in complex
------------------------------------------------------------------------------------------------------------

CayleyFormula = method()

CayleyFormula (Complex, List, List) := ( C, mon, choice ) -> (

  -- index of rows and columns defining regular minors of the differentials
  row := {toList (0..(numgens C_0 - 1))} |
          for j from 1 to length C - 1 list toList select( 0..(numgens C_j - 1), i -> not (flatten choice#j)#i );

  col :=  for j from 1 to length C list toList select( 0..(numgens C_j  - 1), i -> (flatten choice#j)#i );

  -- calculate determinant of complex via Caley's formula
  Res := det submatrix( dd^C_1, row#0, col#0);

  for i from 1 when i < length C and (numgens C_(i+1) > 0 ) do(
    if odd i then
      Res = Res / det submatrix( dd^C_(i+1), row#i, col#i)
    else
      Res = Res * det submatrix( dd^C_(i+1), row#i, col#i);
  );

  -- cast fraction to polynomial and output
  lift( Res, ring C )
);

------------------------------------------------------------------------------------------------------------
-- calculate resultant as determinant of a complex
------------------------------------------------------------------------------------------------------------

calcResultant = method()

calcResultant (List, List, List) := ( f, Lift, shiftP ) -> CayleyFormula( resultantComplex( f, Lift, shiftP ) );

------------------------------------------------------------------------------------------------------------

beginDocumentation()

document { 
      Key => ResultantComplexes,
      Subnodes => {
        TO resultantComplex,
        TO calcResultant,
        TO CayleyFormula,
        TO randomLift,
        TO polynomials,
        TO mixedSubdivision,
        TO CannyEmirisCoef},

      "Systems of sparse polynomials with indeterminate coefficients are defined by the family of their supports
       which contain the exponents of terms. The Minkowski sum of their Newton polytopes specifies a toric variety. 
       To calculate resultants Canny and Emiris shifted this sum by a rational vector. This displacement can also
       be viewed as a twist of line bundles associated with lattice polytopes by a suitable rank one-bundle.
       
       This way, a complex of finitely generated modules over the ring of coefficients is obtained as proposed by
       Gelfand, Kapranov and Zelevinsky via global sections of twisted sheafs in the Koszul complex generated by the
       given system. Following Canny and Emiris again, tight mixed subdivisions are utilized to obtain regular minors
       of the corresponding differentials, so that the determinant can be calculated by means of the Cayley formula.
       
       Besides the assumption that the Minkowski sum of all Newton polytopes in the system must be full dimensional,
       there are no further conditions for the family of supports. Consequently, the determinant of the complex agrees
       with the resultant, redefined by D'Andrea and Sombra.
       
       The example illustrates a system that contains the first three supports as unique essential sub-family.
       It results from the system considered in ", TO resultantComplex, " by multiplying the exponents with a
       unimodular integer matrix.

       Furthermore, the length of the fourth support in the factor lattice over the sub-lattice generated by the first three
       supports doubles the multiplicity of the resultant, as described by D'Andrea, C. and Sombra.",
       
       EXAMPLE{ "A = QQ[c_1..c_10];",
                "supp = {{{0,0,0},{0,2,4},{-2,5,8}}, {{-2,4,6},{1,0,1},{4,-4,-4}}, {{3,-3,-3},{0,1,2}}, {{0,0,0},{2,-4,-4}}};",
                "Lift = {{0,-12,-4},{-2,-1},{-3,0},{-5,-2}};",

                "(complexRes,mon,choice) = resultantComplex( A, supp, Lift, {-1/8,1/3,4/7} );",

                "complexRes",                
                "Res = factor CayleyFormula(complexRes,mon,choice)" },

       References => UL{  "Canny, John F. and Emiris, Ioannis Z., \"A Subdivision-based Algorithm for the Sparse Resultant\",
	                         Journal of ACM, May 2000, volume 47 number 3, 2000",
                          
                          "D'Andrea, C. and Sombra, M., A Poisson formula for the sparse resultant.,
                           Proc. Lond. Math. Soc. (3), ISSN 0024-6115; 1460-244X/e,
                           Volume 110, Number 4, Pages 932--964, 2015",
                           
                          "Sturmfels, Bernd, \"On the Newton Polytope of the Resultant\" ,
	                         Journal of Algebraic Combinatorics, 1994, Apr, volume 3, number 2, pages 207-236 ISSN 1572-9192",
                           
                          "Gelfand, I.M. and Kapranov, M. and Zelevinsky, A., \"Discriminants, Resultants, and Multidimensional
                           Determinants\", ISBN 9780817647711, Modern Birkhäuser Classics, 1994"} }

document {
      Key => {resultantComplex,(resultantComplex,List,List,List),(resultantComplex,PolynomialRing,List,List,List)},
      Usage => "(complexRes, mon, choice) = resultantComplex(f, Lift, shiftP) or (coefRing, supp, Lift, shiftP)",
      Headline => "Resultant complex of sparse system.",
      Inputs => { "f" => List => "List of sparse polynomials (specified in the algebraic torus)",
                  "coefRing" => Ring => "Ring of the coefficients of the input polynomials",
                  "supp" => List => "Support set containing the exponents defining the polynomials with indeterminate coefficients",
                  "Lift" => List => "additional coordinate to lift the vertices of the system's Newton polytopes",
                  "shiftP" => List => "QQ-coordinates of Canny Emiris vector to shift Minkowski sums of Newton polytopes"}, 
      Outputs => {"complexRes" => Complex => "ChainComplex contains modules over the coefficient ring and the differentials ",
                  "mon" => List => "exponents of characters which yield bases of its modules",
                  "choice" => List => "flags specify admissible sub-modules required for the Cayley formula" },

      "This example yields a resultant with a multiple of 7. (Since the family of supports do not satisfy any particular conditions here,
       the determinant of the complex may have multiplicities. It agrees with the resultant, redefined by D'Andrea
       and Sombra.)
       We check the lengths of the co-homologies of the resultant complex localized at the eliminant, which is irreducible 
       and thus generates a prime ideal. Resultant complexes consist of modules over the coefficient ring of the sparse
       polynomials, they are determined by the lattice points inside sums of Newton polytopes shifted by the Canny Emiris vector,
       which are stored in mon. The second list choice specifies admissible sub-modules required for the Cayley formula.
       They are obtained from the lift vectors given in the second argument, provided that these define a tight coherent
       mixed subdivision (TCMD) of the Newton polytopes: ", TO CayleyFormula, " and ", TO mixedSubdivision, ". To reduce the
       number of cells in the sub-division, we specify the lift function only at the vertices of the Newton
       polytopes and not on the entire support sets.",

      EXAMPLE{  "needsPackage \"LocalRings\";",
                "A = QQ[c_1..c_8];",
                "R = A[t_1..t_2];",
                "supp = { {{0,0},{2,2},{1,3}}, {{0,2},{2,1},{4,0}}, {{3,0},{1,1}} };",
                "f = polynomials( gens A, gens R, supp )",

                "(complexRes,mon,choice) = resultantComplex( f, {{0,-12,-4},{-2,-1},{-3,0}}, {1/8,1/3} );",
                "Res = CayleyFormula(complexRes,mon,choice);",
                "factor Res",
                "CP = complexRes ** A_(radical ideal {Res});",
                "for i from 0 to length CP list length HH^i CP"} }

document {
     	Key => {calcResultant,(calcResultant,List,List,List)}, 
      Usage => "Res = calcResultant( f, Lift, shiftP )",
      Headline => "Calculate resultant of sparse system.",
      Inputs => { "f" => List => "List of sparse polynomials (specified in the algebraic torus)",
                  "Lift" => List => "additional coordinate to lift the vertices of the system's Newton polytopes",
                  "shiftP" => List => "QQ-coordinates of Canny Emiris vector to shift Minkowski sums of Newton polytopes"},                 
      Outputs => { "Res" => RingElement => "resultant of the sparse system contained in the (multi-homogeneous) ring of coefficients" },

      "Once we have chosen lifting vectors which produce a tight coherent subdivision of the system's Newton polytopes
       and specified a Canny Emiris vector, the resultant of the sparse system can be calculated as determinant of a complex
       via Cayley formula ", TO resultantComplex, " and ", TO CayleyFormula, ". As the number of terms in resultants can grow
       rapidly, this method is only useful for smaller systems. Except that the Minkowski sum of the Newton polytopes must
       be full dimensional, there are no restrictions on the family of support sets. Hence resultants may have multiplicities,
       they agree with the definition of D'Andrea and Sombra. Further, to reduce the number of cells in the sub-division,
       we specify the lift function only at the vertices of the Newton polytopes and not on the entire support sets.",

      EXAMPLE{  "supp = { {{0,0},{2,2},{1,3}}, {{0,0},{2,0},{1,2}}, {{3,0},{1,1}} };",
        
                "A = QQ[c_1..c_8];",
                "R = A[t_1..t_2];",
                "f = polynomials( gens A, gens R, supp )", 

                "Res = calcResultant( f, {{-1, 0, 0},{-7, -13, 0},{0, 0}}, {0,1/3} )" } }

document {
      Key => {CayleyFormula,(CayleyFormula, Complex, List, List)},
      Usage => "Res = CayleyFormula(complexRes, mon, choice)", 
      Headline => "Calculate determinant of resultant complex.",
      Inputs => {"complexRes" => Complex => "contains modules over the coefficient ring and the differentials",
                  "mon" => List => "exponents of characters which yield bases of its modules",
                  "choice" => List => "flags specify admissible sub-modules required for the Cayley formula" },
      Outputs => { "Res" => RingElement => "resultant evaluated as determinant of resultant complex" }, 

      "A resultant complex must first be determined for this command. Its terms are modules over the coefficient ring
       of the sparse polynomials. They are determined by the lattice points inside sums of Newton polytopes shifted by
       the Canny Emiris vector, which are stored in mon. The second list choice specifies admissible sub-modules required
       for the Cayley formula.
       
       Finally, a system with two essential sub-families is considered. Even in such cases, the definition of toric
       resultant complexes and their admissible sub-modules is valid. Evaluating the Cayley formula yields the unit,
       without checking for exceptions. ",

       EXAMPLE{ "supp = { {{2,0,0},{1,2,2},{0,4,4}}, {{1,0,0},{0,2,2}}, {{0,5,3}}, {{0,0,0},{1,3,2},{4,1,3}} };",
                "A = QQ[c_1..c_(#flatten supp)];",
                "(complexRes, mon, choice) = resultantComplex( A, supp, {{0,-4},{-2,-1},{-5},{-13,0,-3}}, {-1/8,1/3,4/7} );",
                "complexRes",
                "Res = CayleyFormula(complexRes, mon, choice)"} }

document {
     	Key => mixedSubdivision,
      Usage => "cell = mixedSubdivision( grad, Lift )",
      Headline => "computes a coherent mixed sub-division",
      Inputs => { "grad" => List => "of matrices with coordinates of lattice points in its rows",
                  "Lift" => Matrix => "additional coordinate to lift these points" },
      Outputs => { "cell" => List => " list of cells of the coherent mixed sub-division (CMD)" },

      " Determine the tight mixed coherent subdivision (TCMD) of sparse system given in 'On the Newton Polytope of the Resultant' by Sturmfels [1994].
        We get 7 cells, each containing a vertex.",

      EXAMPLE{  "grad = { {{0,2,1},{0,2,3}}, {{0,2,1},{0,0,2}}, {{3,1},{0,1}} } / matrix",
                "Lift = {{-1, 0, 0},{-7, -13, 0},{0, 0}};",
                "cell = mixedSubdivision( grad, matrix {flatten Lift} )" } }

document {
      Key => CannyEmirisCoef,
      Usage => "coefsDiv = CannyEmirisCoef( facetPolytope, shiftPolytope )", 
      Headline => "determine rank one bundle to twist Koszul complex",
      Inputs => {"facetPolytope" => List => "specifies facets of polytope",
                 "shiftPolytope" => Matrix => "vector with rational coefficients move polytope" },
      Outputs => { "coefsDiv" => List => "coefficients of a divisor equivalent to translation of polytope" }, 
         
      "Following Gelfand, Kapranov and Zelevinsky, we calculate resultants via Cayley Formula as determinant
       of a complex formed by global sections of a Koszul complex of sheafs ", TO CayleyFormula, " and ",
       TO resultantComplex, ". To ensure that higher sheaf cohomologies vanish, we twist this complex by a
       reflexive rank one bundle, which corresponds to the shift of Newton polytopes by a rational vector,
       introduced by Canny and Emiris. We cannot assume the Weil divisor corresponding to this bundle
       being Q-Cartier.",
      
      EXAMPLE{ "needsPackage \"NormalToricVarieties\"; ",
      
               "supp = {{1,1,3},{0,2,1},{2,0,4},{2,5,0},{3,2,7}};",
               
               "P = convexHull transpose matrix supp;",
               "X = normalToricVariety P;",

               "a = CannyEmirisCoef( facets P, matrix {{-2/7},{1/3},{3/5}} )",           
               "D = sum( #rays X, i -> a_i * X_i );",

               "isQQCartier D"}}

document {
      Key => polynomials,
      Usage => "f = polynomial(coef, vars, supp)", 
      Headline => "convert support set of exponents to polynomial",
      Inputs => {"coefs" => RingElement => "coefficients of the polynomial",
                 "vars" => RingElement => "variables of output polynomial",
                 "supp" => List => "family of support sets determining a system of polynomials" },
      Outputs => { "f" => List => "of polynomial with fixed or indeterminate coefficients" }, 

      "Polynomials with indeterminate coefficients are determined by the exponents of their terms, which are elements 
       in an integer lattice. These form a set which is referred to as support of the polynomial. Resultants depend on
       a system of such polynomials ", TO calcResultant, ".",

       EXAMPLE{ "supp = { {{1,1,3},{0,2,1},{2,0,4}}, {{0,1,3},{2,5,0}} };",

                "A = QQ[c_1..c_(#flatten supp)];",
                "R = A[t_1..t_(#(supp#0#0))];",
            
                "f = polynomials(gens A, gens R, supp)" } }

document {
      Key => randomLift,
      Usage => "Lift = randomLift( f, m )", 
      Headline => "Get random lift-vectors to produce coherent subdivisions.",
      Inputs => {"f" => List => "of polynomials",
                 "m" => ZZ => "range of random numbers assigned to vertices of Newton polytopes", },
      Outputs => { "Lift" => List => "values of lifting function at the vertices of Newton polytopes" }, 

      "Randomly selects additional coordinates for the list of points whose sub-division is to be determined.
       Tight coherent mixed sub-divisions (TCMD) determine the admissible sub-modules of the resultant complex 
       required for the Cayley formula: ", TO CayleyFormula, " and ", TO mixedSubdivision, ". To reduce the
       number of cells in the sub-division, we specify the lift function only at the vertices of the Newton
       polytopes and not on the entire support set. Randomly chosen liftings almost always result in tight
       mixed sub-divisions.",
       
       EXAMPLE{ "supp = {{{2,1,0},{0,2,4},{4,2,0}},{{1,0,0},{2,2,0}},{{0,1,2},{0,0,0},{0,2,4}},{{0,1,0},{0,0,2}}};",

                "A = QQ[c_1..c_(#flatten supp)];",
                "R = A[t_1..t_(#(supp#0#0))];",

                "f = polynomials(gens A, gens R, supp)",
                "Lift = randomLift(f, 80)" } }

------------------------------------------------------------------------------------------------------------

TEST ///
A = QQ[c_1..c_8];
R = A[x,y];
f = {c_1+c_2*(x*y)^2+c_3*x*y^3, c_4*y^2+c_5*x^2*y+c_6*x^4, c_7*x^3+c_8*x*y};

Res = calcResultant( f, {{0,-12,-4},{-2,-1},{-3,0}}, {1/8,1/3}  );

assert( Res == (c_4*c_7^2 - c_5*c_7*c_8 + c_6*c_8^2)^7 );
///

TEST ///
A = QQ[c_1..c_8];
R = A[x,y];
f = {c_1 + c_2*x^2*y^2 + c_3*x*y^3, c_4 + c_5*x^2 + c_6*x*y^2, c_7*x^3 + c_8*x*y};

(complexRes, mon, choice) = resultantComplex( f, {{-1, 0, 0},{-7, -13, 0},{0, 0}}, {0,1/3} );

assert( mon#2#0 == {} and mon#2#1#0 == matrix {{1},{1}} and mon#2#1#1 == matrix {{1},{2}} );
assert( choice#1#2 == {false, false, true, true, true, true, true, true, true, false, false} );
assert( rank complexRes_0 == 23 and rank complexRes_1 == 27 and rank complexRes_2 == 4 );
assert( dd^complexRes_1 * dd^complexRes_2 == 0 );
///

TEST ///
supp = {{{0,0},{4,2},{3,1},{1,3}}, {{0,1},{2,0},{1,2}}, {{0,0}}};
A = QQ[c_1..c_(#flatten supp)];

(complexRes, mon, choice) = resultantComplex( A, supp, {{0,-4,-7,-1},{-2,-1,-7},{0}}, {3/8, -5/3} );
Res = CayleyFormula(complexRes, mon, choice);

assert( Res == c_8^(12) );
///

TEST ///
grad = {{{0,2,1},{0,2,3}}, {{0,2,1},{0,0,2}}, {{3,1},{0,1}}} / matrix;
cell = mixedSubdivision( grad, matrix {{-1, 0, 0,-7, -13, 0,0, 0}} );
 
assert( cell == {{{0, 1, 2}, {1}, {0}},
                 {{0, 2}, {0, 1}, {1}},
                 {{0, 2}, {1}, {0, 1}},
                 {{0}, {0, 1}, {0, 1}},
                 {{1, 2}, {1, 2}, {0}},
                 {{2}, {0, 1, 2}, {1}},
                 {{2}, {1, 2}, {0, 1}}} )
///

TEST ///
supp = { {{0,0},{1,1},{1,2},{1,3},{2,2}}, {{0,0},{1,0},{2,0},{1,1},{1,2}}, {{3,0},{1,1}} };
A = QQ[c_1..c_(#flatten supp)];
R = A[t_1..t_(#(supp#0#0))];

vertex = supp / matrix / transpose / convexHull / vertices;
Lift = randomLift( polynomials(gens A, gens R, supp), 80 );

assert( apply( vertex, i -> numColumns i) == apply( Lift, i -> #i) )
///

TEST ///
supp = { {{0,0},{2,2},{1,3}}, {{0,0},{2,0},{1,2}}, {{3,0},{1,1}} };
shiftP = {-1/4,2/3};

P = fold( minkowskiSum, supp / matrix / transpose / convexHull); 
a_div = CannyEmirisCoef( facets P, transpose matrix {shiftP} );

assert( a_div == {-2, -1, -1, 1, 1, 0, 0, 1} ); 
///

TEST ///
supp = { {{1,1,3},{0,2,1},{2,0,4}}, {{0,1,3},{2,5,0}} };
A = QQ[c_1..c_(#flatten supp)];
R = A[t_1..t_(#(supp#0#0))];

f = polynomials(gens A, gens R, supp);

assert( f#0 == c_1*t_1*t_2*t_3^3 + c_2*t_2^2*t_3 + c_3*t_1^2*t_3^4 );
assert( f#1 == c_5*t_1^2*t_2^5 + c_4*t_2*t_3^3 );
assert( length f == 2 ); 
///

end--
