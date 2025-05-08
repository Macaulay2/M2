newPackage(
        "SagbiGbDetection",
        Headline => "finding term orders for which the given generators of an ideal/algebra form a Gröbner/SAGBI basis",
        Version => "0.1",  
        Date => "April 11, 2023",
        Authors => {
            {Name => "Viktoriia Borovik", Email => "vborovik@uni-osnabrueck.de", HomePage => "https://sites.google.com/view/viktoriia-borovik/home"},
            {Name => "Timothy Duff", Email => "timduff@uw.edu", HomePage => "https://timduff35.github.io/timduff35/"},
            {Name => "Elima Shehu", Email => "shehu@mis.mpg.de", HomePage => "https://sites.google.com/view/elimawebsite/home"}
            },
        DebuggingMode => false, 
        PackageExports => { "Polyhedra", "ReesAlgebra", "SubalgebraBases" },   
        PackageImports => { "SubalgebraBases"},   
	Keywords => {"Commutative Algebra"}
)   


export {"weightVectorsRealizingGB", "weightVectorsRealizingSAGBI"}

-- the S-pair of two polynomials in the same ring
Spair = (g1, g2) -> (
    lcm12 := lcm(leadTerm(g1), leadTerm(g2));
    sub(lcm12/leadTerm(g1), ring g1) * g1 - sub(lcm12/leadTerm(g2), ring g2) * g2
    )

--  IN: GS, a list of polynomials
-- OUT: true/false, all s-pairs reduce to zero
reducedSpairs = GS -> apply(subsets(#GS, 2), Si -> (
	i := first Si;
	j := last Si;
	division(Spair(GS#i, GS#j), GS)
	)
)

--   IN: 
--      f, a polynomial.
--      G, a list of polynomials (not necessarily forming a Groebner basis.)
--  OUT: The "remainder" of f upon division by leading terms of G
-- Note: This is not always the same as % in general, which would compute the actual normal form modulo the ideal <G>
division = (f, G) -> (
    S := ring f;
    p := f;
    r := 0_S;
    m := #G;
    Q := new MutableHashTable;
    for j from 0 to m-1 do Q#j = 0_S;
    while p != 0 do (i := 0;
    while i < m and leadTerm(p) % leadTerm(G#i) != 0 do i = i+1;
    if i < m then (Q#i = Q#i + (leadTerm(p) // leadTerm(G#i));
        p = p - (leadTerm(p) // leadTerm(G#i)*G#i))else (r = r + leadTerm(p);
        p = p - leadTerm(p)));
    L := apply(m, j -> Q#j);
    r
    )

-- check Buchberger criterion for all S pairs
checkBuchbergerCriterion = GS -> all(reducedSpairs GS, x -> x==0_(ring x))

extractWeightVectors = method(Options => {Verbose => false})
extractWeightVectors List := o -> G -> (
    if o.Verbose then << "now computing newton polytope " << endl;
    P := newtonPolytope product G;
    if o.Verbose then << "now extracting vertices of Newton polytope" << endl;
    V := vertices P;
    n := numcols V;
    m := numrows V;
    if o.Verbose then << "now computing normal cones" << endl;
    normalCones := for i from 0 to n-1 list normalCone(P, convexHull(V_{i}));
    negativeOrthant := coneFromVData(-id_(QQ^m));
    if o.Verbose then << "now intersecting normal cones" << endl;
    intersectedNormalCones := apply(normalCones, c -> intersection(c, negativeOrthant));
    goodCones := select(intersectedNormalCones, c -> dim c > 0);
    weightVectors := apply(select(sum \ entries \ transpose \ rays \ goodCones, w -> all(w, e -> e < 0)), w -> -w);
    if o.Verbose then << "number of weight vectors to check: " << length(weightVectors) << endl;
    weightVectors
    )

-- GB detection algorithm of Gritzmann/Sturmfels
-- Check the Buchberger Criterion for all possible sets of S-pairs using finitely-many weight vectors
weightVectorsRealizingGB = method(Options => {Verbose => false})
weightVectorsRealizingGB List := o -> G -> (
    R := ring first G;
    assert all(drop(G, 1), p -> ring p === R);
    weightVectors := extractWeightVectors(G, o);
    select(weightVectors, w -> (
    	    S := newRing(R, MonomialOrder => {Weights => w});
    	    GS := G/(g->sub(g,S));
    	    if o.Verbose then << "now checking Buchberger's criterion for the weight vector" << w << endl;
    	    checkBuchbergerCriterion GS
    	    )
	)
    )

weightVectorsRealizingGB Matrix := o -> M -> weightVectorsRealizingGB(flatten entries M, o)


-- a SAGBI detection algorithm, based on the Gritzmann/Sturmfels approach
-- uses the function isSAGBI in the package SubalgebraBases
weightVectorsRealizingSAGBI = method(Options => {Verbose => false})
weightVectorsRealizingSAGBI List := o -> G -> (
    R := ring first G;
    assert all(drop(G, 1), p -> ring p === R);
    weightVectors := extractWeightVectors(G, o);
    select(weightVectors, w -> (
    	    S := newRing(R, MonomialOrder => {Weights => w});
    	    GS := G/(g->sub(g,S));
    	    if o.Verbose then << "now checking for SAGBI basis with the weight vector" << w << endl;
    	    isSAGBI GS
    	    )
	)
    )
weightVectorsRealizingSAGBI Matrix := o -> M -> weightVectorsRealizingSAGBI(flatten entries M, o)

beginDocumentation()

doc ///
    Key
      SagbiGbDetection
    Headline
      A package for finding term orders for which the given generators of an ideal/algebra form a Gröbner/SAGBI basis (resp.)
    Description
     Text
       The main functions of this package implement the detection algorithms for Gröbner bases of ideals and SAGBI 
       bases of finitely generated subalgebras of a polynomial ring. More precisely, 
       for given generators of an ideal or a polynomial algebra, we find weight vectors representing all possible term orders for which these generators 
       form a Gröbner basis or a SAGBI basis, respectively. If no weight vectors are returned, the given generators do not form a 
       Gröbner/SAGBI basis for any order.
     Text
        Documentation nodes for the main functions @TO "weightVectorsRealizingGB"@ and @TO "weightVectorsRealizingSAGBI"@ illustrate typical usage of this package.
    Subnodes
      weightVectorsRealizingSAGBI
      weightVectorsRealizingGB
    References	
      @UL {
      {"Gritzmann, Peter, and Bernd Sturmfels. Minkowski addition of polytopes: computational complexity and applications to Gröbner bases. SIAM Journal on Discrete Mathematics 6.2 (1993): 246-269."},
      {"Sturmfels, Bernd. Grobner bases and convex polytopes. Vol. 8. American Mathematical Soc., 1996."}
      }@
///

doc ///
    Key
      weightVectorsRealizingGB
      (weightVectorsRealizingGB, List)
      (weightVectorsRealizingGB, Matrix)
    Headline
      The main function for detecting Gröbner bases
    Usage
      S = weightVectorsRealizingGB G
    Inputs 
      G:List
        of polynomials in the same ring
    Outputs
      S:List
        (possibly empty) containing weight vectors for which Q forms a Gröbner basis
    Description
     Text
       We give three examples of ideal generators that are Gröbner bases for the indicated term orders below:
     Example
       R1 = QQ[x,y, MonomialOrder=>Lex];
       G1 = {y^2-x, x^2-1};
       weightVectorsRealizingGB G1

       R2 = QQ[x,y,z, MonomialOrder=>Lex]
       G2 = {x^3-y, x^5-z}
       weightVectorsRealizingGB G2

       R3 = QQ[x,y,z,w, MonomialOrder=>Lex]
       G3 = {x^2-z, x*y-z^2, x*z-y, w-z^2, y^2-z^3}
       weightVectorsRealizingGB G3
     Text
       Here are two examples of generating sets which are not Gröbner bases:
     Example 
       R4 = QQ[x,y, MonomialOrder=>Lex];
       G4 = {x^2+y^2-1, 2*x*y-1};
       weightVectorsRealizingGB G4

       R5 = QQ[x,y,z, MonomialOrder=>Lex];
       G5 = {x*y^2-x*z+y, x*y-z^2, x-y*z^4};
       weightVectorsRealizingGB G5
///

doc ///
    Key
      weightVectorsRealizingSAGBI
      (weightVectorsRealizingSAGBI, List)
      (weightVectorsRealizingSAGBI, Matrix)
    Headline
      The main function for detecting SAGBI bases
    Usage
      S = weightVectorsRealizingSAGBI Q
    Inputs 
      Q:List
        of polynomials in the same ring
    Outputs
      S:List
        (possibly empty) containing weight vectors for which Q forms a SAGBI basis
    Description
     Text
       Here are two examples where we find one or more weight vectors for which the polynomials form a SAGBI basis.
     Example 
       S1 = QQ[x,y,z, MonomialOrder=>Lex];
       Q1 = {z, z*x, z*y, z*x*(x^2+y^2),z*y*(x^2+y^2)};
       weightVectorsRealizingSAGBI Q1

       S2 = QQ[x, MonomialOrder=>Lex];
       Q2 = {x^4+x^3, x^2+x, x^3 + x^2};
       weightVectorsRealizingSAGBI Q2
     Text
       Here is an example where the algebra generators are not a SAGBI basis for any term order.
     Example
       S3 = QQ[x,y, MonomialOrder=>Lex];
       Q3 = {x+y, x*y, x*y^2};
       weightVectorsRealizingSAGBI Q3
///

TEST ///
-* Example 3.9 in Sturmfels' text *-
R = QQ[x_1..x_3];
F = {x_1^5 + x_2^3 + x_3^2 - 1, x_1^2 + x_2^2 + x_3 - 1, x_1^6 + x_2^5 + x_3^3 - 1};
goodWeights = weightVectorsRealizingGB F;
assert(1 == length goodWeights);
w1 = first goodWeights;
w2 = {3, 4, 7};
R1 = newRing(R, Weights => w1)
R2 = newRing(R, Weights => w2)
R2ToR1 = map(R1, R2, gens R1);
assert(all(F, f -> leadTerm(sub(f,R1)) == R2ToR1 leadTerm(sub(f,R2))))
///

TEST ///
-* Algebra of Grassmannian of 2-planes is a universal SAGBI basis*-
(m, n) = (4, 2);
R = QQ[x_(1,1)..x_(m,n)];
X = transpose genericMatrix(R, n, m);
Q = apply(subsets(m,n), S -> det X^S);
assert(24 == length weightVectorsRealizingSAGBI Q)
///
end--

    
restart
needsPackage "SagbiGbDetection"
check "SagbiGbDetection"
capture examples weightVectorsRealizingGB
capture examples weightVectorsRealizingSAGBI

uninstallPackage "SubalgebraBases"
restart
installPackage("SubalgebraBases", FileName => "./SubalgebraBases.m2")
viewHelp "SubalgebraBases"

uninstallPackage "SagbiGbDetection"
restart
installPackage("SagbiGbDetection", MakeDocumentation => true)
viewHelp "SagbiGbDetection"
viewHelp "SubalgebraBases"


uninstallPackage "SagbiGbDetection"


check "SagbiGbDetection"
viewHelp "SagbiGbDetection"
