-- In this file, we include Macaulay2 code which verifies the examples
-- of the paper.  The main emphasis is on codim 4, Arithmetically Gorenstein
-- ideals, of regularity 4.

newPackage(
    "QuaternaryQuartics",
    Version => "0.99",
    Date => "11 Nov 2021",
    Headline => "code to support the paper 'Quaternary Quartic Forms and Gorenstein Rings'",
    Authors => {
        {Name => "Gregorz Kapustka"},
        {Name => "Michal Kapustka"},
        {Name => "Kristian Ranestad"},
        {Name => "Hal Schenck"},  
        {Name => "Mike Stillman",  
            Email => "mike@math.cornell.edu", 
            HomePage => "http://www.math.cornell.edu/~mike"},
        {Name => "Beihui Yuan"}
        },
    PackageExports => {
        "InverseSystems", -- used in 'quartic'
        "StronglyStableIdeals", -- used in 'nondegenerateBorels'
        "GroebnerStrata" 
        },
    AuxiliaryFiles => true,
    DebuggingMode => false
    )

export {
    "quarticType",
    "randomBlockMatrix",
    "randomHomomorphism",
    "pointsIdeal", -- pointsIdeal Matrix -- uses the ring of the matrix.
    "randomPoints", -- matrix whose columns are random points
    "Count",
    "Normalize",

    "nondegenerateBorels",
    "doubling",
    "quartic",
    "smallerBettiTables",
    "bettiStrataExamples"
    }

randomBlockMatrix = method()
randomBlockMatrix(List, List, List) := (tar, src, mats) -> (
    if #tar == 0 or #src == 0 then error "expected lists of free modules";
    if not all(tar, x -> instance(x, Module))
      or
      not all(src, x -> instance(x, Module))
      then error "expected lists of free modules";
    R := ring tar_0;
    if not all(tar, x -> ring x === R)
      or
      not all(src, x -> ring x === R)
      then error "expected lists of free modules over a common ring";
    nrowblocks := #tar;
    ncolblocks := #src;
    if #mats != nrowblocks or not all(mats, r -> #r == ncolblocks)
    then error "wrong number of matrices given";
    matrix for i from 0 to nrowblocks-1 list
      for j from 0 to ncolblocks-1 list (
          if mats#i#j === random then random(tar#i, src#j)
          else map(tar#i, src#j, mats#i#j)
          -- else if mats#i#j === 0 then map(tar#i, src#j, 0)
          -- else if mats#i#j === 1 then map(tar#i, src#j, 1)
          )
    )

random(List, Ideal) :=
random(ZZ, Ideal) := RingElement => opts -> (d, I) -> (
    R := ring I;
    b := super basis(d, I);
    (b * random(R^(numcols b), R^1))_(0,0)
    )

pointIdeal = method()
pointIdeal Matrix := Ideal => (m) -> (
    v := transpose vars ring m;
    trim minors(2, v|m)
    )

pointsIdeal = method()
pointsIdeal Matrix := Ideal => (m) -> (
    intersect for i from 0 to numcols m - 1 list pointIdeal(m_{i})
    )
pointsIdeal(Ring, Matrix) := Ideal => (S, mkk) -> (
    m := mkk ** S;
    intersect for i from 0 to numcols m - 1 list pointIdeal(m_{i})
    )
pointsIdeal(Matrix, Ring) := Ideal => (mkk, S) -> pointsIdeal(S, mkk)

randomPoints = method(Options => {Normalize => false})
randomPoints(Ring, ZZ, ZZ) := Matrix => opts -> (kk, n, d) -> (
    -- n is the number of variables 
    -- d is the number of points
    -- returns a d by n matrix over kk.
    if not opts.Normalize then return random(kk^n, kk^d);
    I := id_(kk^n) | matrix apply(n, i -> {1});
    if d <= n+1 then return I_{0..d-1};
    rand := random(kk^n, kk^(d-n-1));
    I | rand
    )
randomPoints(Ring, ZZ) := Matrix => opts -> (S, d) -> randomPoints(S, numgens S, d, opts)

randomHomomorphism = method()
randomHomomorphism(List, Module, Module) :=
randomHomomorphism(ZZ, Module, Module) := Matrix => (deg, tar, src) -> (
    H := Hom(src, tar);
    B := basis(deg, H); -- map HomModule <--- graded free
    rand := random(source B, (ring B)^{-deg});
    homomorphism(B * rand)
    )

nondegenerateBorels = method(Options => {Sort => false})
nondegenerateBorels(ZZ, Ring) := List => opts -> (d, S) -> (
    Bs := stronglyStableIdeals(d, S);
    Bs = select(Bs, i -> all(i_*, f -> degree f =!= {1}));
    if opts.Sort then
      Bs = Bs/(i -> ideal sort(gens i, MonomialOrder => Descending, DegreeOrder => Ascending));
    Bs
    )

doubling = method(Options => {Count => 10})
doubling(ZZ, Ideal) := Ideal => opts -> (deg, I) -> (
    c := codim I;
    wR := Ext^c(comodule I, ring I);
    H := Hom(wR, comodule I);
    count := 0;
    while count < opts.Count do (
        f := randomHomomorphism(deg, comodule I, wR);
        if ker f == 0 then (
            if debugLevel > 0 then (
                if count == 1 then << "took 1 try" << endl;
                if count > 1 then << "took " << count << " tries" << endl;
                );
            return trim ideal presentation coker f;
            );
        count = count+1;
        );
    null
    );

quartic = method()
quartic(Matrix, Ring) := RingElement => (pts, S) -> (
    if numgens S =!= numrows pts then error ("expected a matrix with "|toString numgens S|" rows");
    linforms := flatten entries((vars S) * pts);
    sum for ell in linforms list ell^4
    )
quartic Matrix := RingElement => (pts) -> quartic(pts, ring pts)

bettiType = method()
bettiType Ideal := (I) -> (
    B := betti res(I, DegreeLimit => 2);
    B
    )

topRow = method()
topRow BettiTally := List => B -> (
    for i from 1 to 3 list if B#?(i, {i+1}, i+1) then B#(i, {i+1}, i+1) else 0
    )

quarticType = method()
quarticType RingElement := String => F -> (
    -- returns either "[has linear form]", or one of the 19 strata that this quartic sits on
    R := ring F;
    if numgens R =!= 4 then error "expected a polynomial ring in 4 variables";
    if degree F =!= {4} then error "expected a quartic polynomial";
    I := trim inverseSystem F;
    if any(I_*, f -> degree f === {1}) then return "[has linear form]";
    Q := ideal select(I_*, f -> degree f === {2}); -- quadratic part
    CQ := res Q;
    twolinear := topRow betti CQ;
    if twolinear === {3,0,0} then (
        -- 3 possible types: abc.
        if codim Q === 2 then 
            "[300c]"
        else if codim Q === 3 then 
            "[300ab]" -- how to detect the difference?
        else
            error "internal error: should not reach this line"
        )
    else if twolinear === {4,4,1} then (
        syz2 := ideal CQ.dd_3_{0};
        if codim syz2 === 3 then 
            "[441a]"
        else if codim syz2 === 4 then 
            "[441b]"
        )
    else -- the easy case: the quadric strand determines the type
       "["|twolinear#0|twolinear#1|twolinear#2|"]"
    )

-- Keep this??
kustinMiller = () -> (
    kk := ZZ/32003;
    a := getSymbol "a";
    v := getSymbol "v";
    x := getSymbol "x";
    S := kk[a_(1,1)..a_(3,4), v, x_1..x_4, Degrees => {12:1, 2, 4:1}] ;
    M := transpose genericMatrix(S, S_0, 4, 3);
    xvec := genericMatrix(S, S_13, 4, 1);
    xvec2 := transpose matrix{{S_16, -S_15, S_14, -S_13}};
    M3 := exteriorPower(3, transpose M);
    ideal(M*xvec) + ideal(S_12*xvec2 + M3)
    )

bettiStrataExamples = method()
bettiStrataExamples Ring := HashTable => (kk) -> new HashTable from {
    "[683]" => {randomPoints(kk, 4, 4, Normalize => true), "4 general points"},
    "[550]" => {randomPoints(kk, 4, 5, Normalize => true), "5 general points"},
    "[420]" => {randomPoints(kk, 4, 6, Normalize => true), "6 general points"},
    "[300a]" => {transpose matrix{{1,2,3,1},{1,2,3,-1},{1,2,-3,1},{1,2,-3,-1},{1,-2,3,1},{1,-2,3,-1},{1,-2,-3,1},{1,-2,-3,-1}}**kk, "8 points which forms a CI"},
    "[300b]" => {randomPoints(kk, 4, 7, Normalize => true), "7 general points"},
    "[300c]" => {transpose matrix{{1,0,0,0},{0,1,0,0},{1,1,0,0}}|randomPoints(kk,4, 4, Normalize => false), "7 points, 3 on a line"},
    "[200]" => {randomPoints(kk, 4, 8, Normalize => true), "8 general points"},
    "[100]" => {randomPoints(kk, 4, 9, Normalize => true), "9 general points"},
    "[000]" => {randomPoints(kk, 4, 10, Normalize => true), "10 general points"},
    "[562]" => {id_(kk^4) | transpose matrix{{1,1,0,0}}, "5 points, 3 on a line"},
    "[551]" => {id_(kk^4) | transpose matrix{{1,1,1,0}}, "5 points, 4 on a plane"},
    "[430]" => {id_(kk^4) | transpose matrix{{1,1,0,0}, {1,0,1,1}}, "6 points, 3 on a line"},
    "[441a]" => {id_(kk^4) | transpose matrix{{1,1,0,0}, {1,0,1,0}}, "6 points, 5 on a plane"},
    "[441b]" => {id_(kk^4) | transpose matrix{{1,1,0,0}, {0,0,1,1}}, "6 points, 3 each on 2 skew lines"},
    "[320]" => {id_(kk^4) | transpose matrix{{1,1,0,0}, {1,0,1,0}, {1,0,0,1}}, "7 points on a twisted cubic curve"},
    "[310]" => {id_(kk^4) | transpose matrix{{1,1,1,0}, {1,1,1,1}, {1,0,0,1}}, "7 points with 5 on a plane"},
    "[331]" => {id_(kk^4) | (randomPoints(kk, 3, 3)||matrix{{0,0,0}}), "7 points with 6 on a plane"},
    "[210]" => {id_(kk^4) | transpose matrix{
            {1,1,0,0}, {1,0,1,0}, {0,1,1,0}, {1,1,1,1}}, "8 points with 6 in a plane, or five in a plane and three in a line"}
    }


smallerTables1 = (B, k) -> (
    -- B: BettiTally
    -- k: (i,{d},d), an entry in B, such that (i+1,{d},d) occurs
    ell := (k#0+1, k#1, k#2);
    a := B#k;
    b := B#ell; -- this is assumed to exist
    r := min(a,b);
    others := select(pairs B, x -> x#0 =!= k and x#0 =!= ell);
    for n from 0 to r list (
        these := others;
        if a - n > 0 then these = these | {(k, a - n)};
        if b - n > 0 then these = these | {(ell, b - n)};
        new BettiTally from these
        )
    )

smallerBettiTables = method()
smallerBettiTables BettiTally := (B) -> (
    -- first find the spots where there could be cancellation
    nonminimals := for k in keys B list (
        (i,d,j) := k;
        if B#?(i+1,d,j) then k else continue
        );
    Bs := {B};
    for k in nonminimals do (
        Bs = flatten for B in Bs list smallerTables1(B, k);
        );
    Bs
    )

-* Documentation section *-
beginDocumentation()

load "./QuaternaryQuartics/Section1Doc.m2"
load "./QuaternaryQuartics/Section2Doc.m2"
load "./QuaternaryQuartics/Section3Doc.m2"
load "./QuaternaryQuartics/Section4Doc.m2"
load "./QuaternaryQuartics/Section5Doc.m2"
load "./QuaternaryQuartics/Section6Doc.m2"
load "./QuaternaryQuartics/Section7Doc.m2"
load "./QuaternaryQuartics/Section8Doc.m2"
load "./QuaternaryQuartics/Section9Doc.m2"
load "./QuaternaryQuartics/Appendix2.m2"

doc ///
    Key
        QuaternaryQuartics
    Headline
        code to support the paper 'Quaternary Quartic Forms and Gorenstein Rings'
    Description
        Text
            This package contains code and examples for the paper @TO "[QQ]"@
            {\it Quaternary Quartic Forms and Gorenstein Rings},
            by Grzegorz Kapustka,
            Michal Kapustka, Kristian Ranestad, Hal Schenck, Mike
            Stillman and Beihui Yuan, referenced below.
        
            We study the space of quartic forms in four variables,
            interleaving the notions of: rank, border rank,
            annihilator of the quartic form, Betti tables, and Calabi-Yau varieties
            of codimension 4.

        Text
          @SUBSECTION "Section 1: Generating the Betti tables"@
        Text
          @UL {
              TO "Finding the 16 betti tables possible for quartic forms in 4 variables, and examples"
          }@
        Text
          @SUBSECTION "Section 2: Basic constructions"@
        Text
          @UL {
              TO "Doubling Examples",
              TO "Doubling Examples for ideals of 6 points",
              TO "Example Type [300a]",
              TO "Example Type [300b]",
              TO "Example Type [300c]"
          }@
        Text
          @SUBSECTION "Section 3: betti tables for points in P^3 with given geometry"@ 
        Text
          @UL {
              TO "Finding the possible betti tables for points in P^3 with given geometry"
          }@
        Text
          @SUBSECTION "Section 4: the quadratic part of the apolar ideal"@ 
        Text
          @UL {
              TO "Finding all possible betti tables for quadratic component of inverse system for quartics in 4 variables"
          }@
        Text
          @SUBSECTION "Section 5: VSP(F,9) for a general quadric form of rank 9"@ 
        Text
          @UL {
              TO "VSP(F_Q,9)"
          }@
        Text
          @SUBSECTION "Section 6: Stratification of the space of quaternary quartics"@ 
        Text
          @UL {
              TO "Finding the Betti stratum of a given quartic",
              TO "Noether-Lefschetz examples"
          }@
        Text
          @SUBSECTION "Section 7: Codimension three varieties in quadrics"@ 
        Text
          @UL {
              TO "Pfaffians on quadrics"
          }@
        Text
          @SUBSECTION "Section 8: Irreducible liftings"@ 
        Text
          @UL {
              TO "Type [000], CY of degree 20",
	      TO "Singularities of lifting of type [300b]",
	      TO "Half canonical degree 20"
          }@
        Text
          @SUBSECTION "Section 9: Construction and lifting of AG varieties"@
        Text
          @UL {
              TO "Type [210], CY of degree 18 via linkage",
              TO "Type [310], CY of degree 17 via linkage",
              TO "Type [331], CY of degree 17 via linkage",
              TO "Type [420], CY of degree 16 via linkage",
              TO "Type [430], CY of degree 16 via linkage",
              TO "Type [441a], CY of degree 16",
              TO "Type [441b], CY of degree 16",
              TO "Type [551], CY of degree 15 via linkage",
              TO "Type [562] with lifting of type I, a CY of degree 15 via linkage",
              TO "Type [562] with a lifting of type II, a CY of degree 15 via linkage"
          }@
        Text
          @SUBSECTION "Appendix 2: Components of the Betti table loci in Hilbert schemes of points"@
        Text
          @UL {
              TO "Hilbert scheme of 6 points in projective 3-space"
          }@

    References
        @TO "[QQ]"@ {\it Quaternary Quartic Forms and Gorenstein Rings},
            by Grzegorz Kapustka,
            Michal Kapustka, Kristian Ranestad, Hal Schenck, Mike
            Stillman and Beihui Yuan. (arxiv:2111.05817) 2021.
    SeeAlso
///

doc ///
    Key
        "[QQ]"
    Headline
        Quaternary Quartic Forms and Gorenstein rings (Kapustka, Kapustka, Ranestad, Schenck, Stillman, Yuan, 2021)
    Description
        Text
            [QQ] @arXiv("2111.05817", "Quaternary Quartic Forms and Gorenstein Rings")@
            by Grzegorz Kapustka,
            Michal Kapustka, Kristian Ranestad, Hal Schenck, Mike
            Stillman and Beihui Yuan, 2021.
///

doc ///
    Key
        bettiStrataExamples
        (bettiStrataExamples, Ring)
    Headline
        a hash table consisting of examples for each of the 19 Betti strata
    Usage
        bettiStrataExamples S
    Inputs
        S:Ring
            a polynomial ring with 4 variables
    Outputs
        :HashTable
          Whose keys are strings representing each Betti table strata, and
          whose values are matrices of scalars over the ring $S$
    Description
        Text
            The result is a hash table whose keys are the names of the 
            19 Betti table strata for quaternary quartics.  For each, the
            value is a matrix whose columns represent points.  The quartic
            corresponding to this matrix is the sum of the 4th powers of the
            corresponding linear forms.
        Example
            S = ZZ/101[a..d]
            bettiStrataExamples S
    Caveat
    SeeAlso
///

doc ///
    Key
        randomBlockMatrix
        (randomBlockMatrix, List, List, List)
    Headline
        create a block matrix with zero, identity and random blocks
    Usage
        randomBlockMatrix(tarList, srcList, mats)
    Inputs
        tarList:List
          a non-empty list of modules over a ring $R$
        srcList:List
          a non-empty list of modules over the same ring $R$
        mats:List
          of lists, of length = number of elements in the tarList, and
          each list has {\tt #srcList} entries
    Outputs
        :Matrix
    Description
        Text
            This function creates a block matrix with the block sizes (and degrees) determined by
            the modules in {\tt tarList} and {\tt srcList}.
            Each entry in the {\tt mats} matrix indicates what should be placed at that block of the matrix:
            mats#r#c corresponds to a matrix with target tarList#r, and source srcList#c.
            
            Each entry can be: {\tt random} (giving a block
            which is random), the number 0 (a zero block), the number
            1 (an identity block), or an actual matrix.
        Example
            S = ZZ/101[a..d]
            randomBlockMatrix({S^3, S^1}, {S^3, S^1}, {{random, random}, {0, 1}})
        Example
            S = ZZ/101[a..d]
            randomBlockMatrix({S^3, S^2}, {S^3, S^2, S^{2:-1}}, {{random, random, 0}, {0, 1, random}})
    SeeAlso
      (random, Module, Module)
///

undocumented {    
    (pointsIdeal, Matrix, Ring)
    }

doc ///
  Key
    pointsIdeal
    (pointsIdeal, Matrix)
    (pointsIdeal, Ring, Matrix)
  Headline
    create an ideal of points
  Usage
    pointsIdeal M
    pointsIdeal(R, M)
  Inputs
    M:Matrix
      of size $m \times n$, either over the coefficient ring of $R$, or a polynomial ring $R$
      with $m$ variables
    R:Ring
      either the ring of $M$, or a polynomial ring with $m$ variables with coefficient ring the ring
      of $M$
  Outputs
    :Ideal
      the homogeneous ideal in $R$ of the points which are the columns of $M$
  Description
    Text
      In this example, we find the ideal of 6 general points in $\PP^3$.  Since they are general, we 
      can set the first 5 points to be in standard position (the coordinate points, 
      and the point with all coordinates being 1).
    Example
      S = ZZ/32003[a..d]
      M = randomPoints(S, 6, Normalize => true)
      I = pointsIdeal M
      betti res I
  SeeAlso
    randomPoints
///

doc ///
  Key
    randomPoints
    (randomPoints, Ring, ZZ, ZZ)
    (randomPoints, Ring, ZZ)
    [randomPoints, Normalize]
  Headline
    create a matrix whose columns are random points
  Usage
    randomPoints(kk, m, n)
    randomPoints(S, n)
  Inputs
    S:Ring
      with $m$ variables
    kk:Ring
      a field
    m:ZZ
      number of variables (rows)
    n:ZZ
      the number of points (columns)
    Normalize => Boolean
      whether to set the first $m+1$ to be the coordinate points
      and the point whose coordinates are all one
  Outputs
    M:Matrix
      of size $(m \times n)$ over the ring $S$ or $kk$ consisting of (random scalars)
  Description
    Text
      There are two usages of this function. The first creates a matrix over a base field.
      This is not much different from using {\tt random(kk^m, kk^n)}, unless the Normalize
      option is given, in which case the first set of points are normalized to be the
      coordinate points and the point each of whose coordinates are 1.
    Example
      kk = ZZ/101;
      randomPoints(kk, 5, 10)
      randomPoints(kk, 5, 10, Normalize => true)
    Text
      The second version is perhaps used the most in this package.
      One can leave out the number of variables/rows if the ring given is a polynomial ring.
    Example
      S = kk[a..d];
      M1 = randomPoints(S, 10)
      M2 = randomPoints(S, 6, Normalize=>true)
      pointsIdeal M1
      pointsIdeal M2
    Text
      Another useful way to generate a matrix of points is to use 
      @TO randomBlockMatrix@.
      
      For example, the following creates the ideal of 6 points, 3 on one line
      and 3 on a skew line.
    Example
      M3 = randomBlockMatrix({S^2, S^2}, {S^3, S^3}, {{random, 0}, {0, random}})
      pointsIdeal M3      
  SeeAlso
    pointsIdeal
    random
    (random, List, Ideal)
    randomBlockMatrix
    randomHomomorphism
///

doc ///
  Key
    randomHomomorphism
    (randomHomomorphism, ZZ, Module, Module)
    (randomHomomorphism, List, Module, Module)
  Headline
    create a random homomorphism between graded modules
  Usage
    randomHomomorphism(d, N, M)
  Inputs
    d:List
      or an integer, if the common ring $R$ of $M$ and $N$ is singly graded
    N:Module
      the target module
    M:Module
      the source module
  Outputs
    :Matrix
      a random $R$-module homomorphism from $M$ to $N$ of degree $d$
  Description
    Text
      This function can be useful to find isomorphisms between modules
      (since if there is an isomorphism, a random map between them will be
      such an isomorphism), as well as writing the canonical module as an ideal
      (up to degree shift) in the ring.
      
      We start with a simpler application: duplicating the work of the simpler function 
      @TO (random, ZZ, Ideal)@.  Here are two ways to get a random element of degree 4
      in the ideal $I$.
    Example
      S = ZZ/101[a..d]
      I = monomialCurveIdeal(S, {2,5,9})
      g = randomHomomorphism({4}, module I, S^1)
      isWellDefined g
      super g
      J = ideal image g
      random(4, I)
    Text
      One important application of this function is to find 
      an isomorphism of the canonical module of $R = S/I$
      with an ideal $J \subset R$, up to a degree twist.
      See @TO doubling@ for a function which uses this 
      method.
    Example
      R = S/I
      E = Ext^2(comodule I, S^{{-4}})
      ER = E ** R
      isHomogeneous ER
      f = randomHomomorphism(3, R^1, ER)
      isWellDefined f
      source f == ER
      target f == R^1
      degree f == {3}
      ker f == 0
      J = ideal image f
  SeeAlso
    random
    (random, ZZ, Ideal)
    randomBlockMatrix
    randomPoints
///

doc ///
  Key
    (random, List, Ideal)
    (random, ZZ, Ideal)
  Headline
    a random ring element of a given degree
  Usage
    random(d, I)
  Inputs
    d:List
      or @ofClass ZZ@, if the ring of $I$ is singly graded
    I:Ideal
      homogeneous
  Outputs
    :RingElement
      a random element in the ideal of the given degree
  Description
    Text
      This function should probably be in the Core of Macaulay2.
    Example
      S = ZZ/101[a..d]
      I = ideal(a^2, a*b^3, c*d)
      f = random(3, I)
      f % I == 0 -- so f is in the ideal I
      degree f == {3}
  SeeAlso
    random
    randomBlockMatrix
    randomHomomorphism
    randomPoints
///

doc ///
  Key
    nondegenerateBorels
    (nondegenerateBorels, ZZ, Ring)
    [nondegenerateBorels, Sort]
  Headline
    construct all nondegenerate strongly stable ideals of given length
  Usage
    nondegenerateBorels(d, S)
  Inputs
    d:ZZ
      the length of the desired ideals in S$
    S:Ring
      a polynomial ring
    Sort => Boolean
      whether to sort the generators of each ideal in a slightly more natural way
  Outputs
    :List
      of all strongly stable ideals in $S$ which are saturated, are (affine) dimension one,
      have degree $d$, and have no linear forms in their ideal
  Description
    Text
      This is a simplified interface to the @TO StronglyStableIdeals$stronglyStableIdeals@ function.
      
      
      For example, the following are all of the strongly stable ideals with degree 7, and their Betti
      tables.
    Example
      S = ZZ/101[a..d];
      Bs = nondegenerateBorels(7, S);
      netList Bs
      netList pack(4, Bs/minimalBetti)
    Text
      Using the {\tt Sort} option as follows gives a somewhat more natural ordering.  Sometimes
      computations involving the groebnerSratum ideal will be either much faster or 
      slower with this option.  But it is often worth trying it both ways, if your computations
      are slow.
    Example
      Bs2 = nondegenerateBorels(7, S, Sort => true);
      netList Bs2
    Text
      This is a convenience function.  Here is the simple code:
    Example
      code methods nondegenerateBorels
  SeeAlso
    random
    randomBlockMatrix
    randomHomomorphism
    randomPoints
///

doc ///
  Key
    smallerBettiTables
    (smallerBettiTables, BettiTally)
  Headline
    Find all (potentially) smaller Betti tables that could degenerate to given table
  Usage
    smallerBettiTables B
  Inputs
    B:BettiTally
      a possible table of some (singly) graded module
  Outputs
    :List
      a list of all Betti tables where cancellation could possibly occur
  Description
    Text
      Given a complex over a graded ring, with Betti table $B$, whenever there is an
      entry of degree zero, if that entry is nonzero, then one can use that as a pivot,
      and cancel that row and column
      creating a smaller complex.  This function returns the Betti tables of all possible 
      such cancellations that may be able to occur.  Some of these might not be valid for 
      actual complexes, as one might obtain a complex with no non-zero scalar entries.
      But, the list of every smaller Betti table that could possibly be the minimal Betti diagram 
      of such a module is returned.
    Example
      S = ZZ/101[a..d]
      I = ideal(a*c, a*b, a^2, c^3, b*c^2, b^2*c, b^3)
      B = betti res I
      smallerBettiTables B
      netList pack(4, oo)
    Text
      Note that from the Betti table there are 2 maps of degree 0.  The first is a $4 \times 3$
      matrix, and the second is a $7 \times 1$ matrix.  There are 4 possible ranks for the first matrix,
      and 2 for the second, giving 8 Betti tables in the result.  No further 
      knowledge is used to remove possible tables from the output list.
    Text
      All actual Betti diagrams of ideals with $I$ as its initial ideal will be among this list.
      Clearly, some of these cannot occur.  The ones indexed 2, 4 and 6 cannot occur.
      One can use the package @TO "GroebnerStrata"@ to help determine which can possibly occur.
  SeeAlso
    nondegenerateBorels
    "GroebnerStrata::GroebnerStrata"
///

doc ///
  Key
    doubling
    (doubling, ZZ, Ideal)
    [doubling, Count]
  Headline
    implement the doubling construction
  Usage
    doubling(d, I)
  Inputs
    d:ZZ
      the degree of the map
    I:Ideal
      homogeneous, in a singly graded polynomial ring $S$
    Count => ZZ
      number of random maps to generate before giving up
      and returning null
  Outputs
    :Ideal
      an ideal $J$ containing $I$ such that the canonical module of $S/I$ is $J/I \otimes S(-d)$,
      or null, if either one doesn't exist or one cannot be found
  Description
    Text
      Let $R = S/I$, and $w_R = \operatorname{Ext}^c(R, S^{-n-1})$, where $c$ is the codimension
      of $I$ and $n+1$ is the number of variables of the polynomial ring $S$.
      
      If there exists an injective  homomorphism $f \colon w_R \to R$ of degree $d$, this
      function returns the ideal defining the cokernel of a random such map.  If none exist, null is returned.  If after
      trying the number of trials given by the optional argument {\tt Count}, none that
      are injective can be found (this is very unlikely), null is also returned.
      Setting the global variable @TO "debugLevel"@ to a positive value will let you know
      how many times it took to find one (if it didn't find it right away).
      
      If $S/I$ is arithmetically Cohen-Macaulay of codimension $c$, then the cokernel of $f$
      will be arithmetically Gorenstein of codimension $c+1$.
      
      See section 2.5 of @TO "[QQ]"@ for more details and references.
    Example
      S = ZZ/101[a..d];
      I = pointsIdeal randomPoints(S, 6)
      betti res I
      doubling(5, I)
      J = doubling(8, I)
      betti res J
    Text
      Here are some doublings of the Veronese surface
    Example
      S = ZZ/101[x_0..x_5];
      M = genericSymmetricMatrix(S, 3)
      I = trim minors(2, M)
      doubling(4, I) -- no such map exists
      betti res doubling(6,I)
      betti res doubling(7,I)
      betti res doubling(8,I)
      J = doubling(8, I);
      (dim J, degree J)
      (dim I, degree I)
    Example
      S = ZZ/101[x_0..x_8];
      M = genericMatrix(S, 3, 3)
      I = trim minors(2, M)
      betti res doubling(8,I)
      J = doubling(8, I);
      (dim J, degree J)
      (dim I, degree I)
  SeeAlso
    randomHomomorphism
///

doc ///
    Key
        (quarticType, RingElement)
        quarticType
    Headline
        the Betti stratum a specific quartic lies on
    Usage
        quarticType F
    Inputs
        F:RingElement
            A homogeneous quartic polynomial in a polynomial ring $S$ in 4 variables
            (over a field)
    Outputs
        :String
          one of the strings: [has linear form],
          [000], [100], [200], [210], [300ab], [300c], 
          [310], [320], [331], [420], [430], [441a], [441b], 
          [550], [551], [562], [683].
    Description
        Text
            If the inverse system $F^\perp$ of $F$ contains a
            linear form, then [has linear forms] is returned.
            There are 19 strata for $F$ which do not
            have a linear form in their inverse system.  This function
            determines which one of these 19 strata the quartic lives
            on.  However, it cannot distinguish easily between [300a]
            and [300b], so instead it returns [300ab] in this case.
            Note that the function can detect [300c], as this is the situation
            when the 3 quadrics are not a complete intersection (instead, they
            form the ideal of a length 7 subscheme of $\PP^3$).
          
            All other cases can be determined by the free resolution
            of the ideal of quadrics in the inverse system $F^\perp$,
            although in cases [300abc] and [441ab], a slightly finer
            analysis must be made, which depends on the syzygies of
            the quadratic ideal.
            
            See section 6 of [QQ] for the inclusion relations on the closures of these
            strata, and their dimensions.
            
            The 2 cases that cannot be determined easily are [300a] and [300b].
            The inverse system $F^\perp$ has 3 quadric generators in each case.
            However, in one case the quartic has rank 7 (this is the case [300b], and the other case [300a], the quadric
            generally has rank 8). This is subtle information, which we do not try to compute here.
        Example
            S = ZZ/101[a..d]
            H = bettiStrataExamples S
            keys H
            netList for k in sort keys H list (
                F := quartic first H#k;
                {k, minimalBetti inverseSystem F, quarticType F}
                )
            quarticType(a^4 + b^4 + c^4 + d^4 - 3*a*b*c*d)
            quarticType(a*b*c*d)
    SeeAlso
        bettiStrataExamples
        quartic
///

doc ///
    Key
        quartic
        (quartic, Matrix)
        (quartic, Matrix, Ring)
    Headline
        a quartic given by power sums of linear forms
    Usage
        quartic M
        quartic(M, S)
    Inputs
        M:Matrix
            A matrix of scalars, over a ring $S$, or a field
        S:Ring
            A polynomial ring with the same number of variables as the number of rows of $M$.
            If not given, $S$ is taken to be the ring of $M$.
    Outputs
        :RingElement
            A homogeneous quartic polynomial in $S$
    Description
        Text
            One useful way to generate quartic polynomials is as a sum
            of 4th powers of linear forms.  This function creates an
            linear form from each column of the matrix $M$, and then sums their 4th powers.
        Example
            S = ZZ/101[a..d]
            M = transpose matrix(S, {{1,0,0,0}, {0,1,0,0}})
            quartic M
        Example
            H = bettiStrataExamples S
            keys H
            M = first H#"[420]"
            F = quartic M
        Text
            This is a convenience function.  This is basically
            short hand for the following (which computes the
                linear forms corresponding to each column of $M$,
                and then sums their 4th powers.
        Example
            lins := flatten entries((vars S) * M)
            F1 = sum for g in lins list g^4
            F1 == F
        Example
            I = inverseSystem F
            (degree I, codim I, regularity(S^1/I))
            minimalBetti I
    SeeAlso
        (inverseSystem, RingElement)
        bettiStrataExamples
///

doc ///
  Key
    Normalize
  Headline
    an option name
  Description
    Text
      Used in @TO randomPoints@.
///

doc ///
  Key
    Count
  Headline
    an option name
  Description
    Text
      Used in @TO doubling@.
///

TEST ///
-*
  restart
  needsPackage "QuaternaryQuartics"
*-
  S = ZZ/101[a..d]
  M = randomPoints(S, 7)
  assert(numrows M === 4 and numcols M === 7 and ring M === S)
  assert(source M == S^7 and target M == S^4)
  assert(isHomogeneous M)

  S = QQ[a..d]
  M = randomPoints(S, 7)
  assert(numrows M === 4 and numcols M === 7 and ring M === S)
  assert(source M == S^7 and target M == S^4)
  assert(isHomogeneous M)

  S = (ZZ/101[t])[a..d, Join => false]
  M = randomPoints(S, 7) -- notice no t's though
  assert(numrows M === 4 and numcols M === 7 and ring M === S)
  assert(source M == S^7 and target M == S^4)
  assert(isHomogeneous M)

  kk = ZZ/101
  S = kk[a..d]
  M = transpose matrix(S, {
          {1,1,1,1}, {1,2,4,8}, {1,3,9,27}, {1,4,16,64}, {1,5,25,125}})
  I = pointsIdeal M
  assert(degree I == 5)
  assert(regularity I == 3)
  assert(dim I == 1)

  M = transpose matrix(kk, {
          {1,1,1,1}, {1,2,4,8}, {1,3,9,27}, {1,4,16,64}, {1,5,25,125}})
  I = pointsIdeal(M, S)
  I1 = pointsIdeal(S, M)
  I2 = pointsIdeal(M ** S)
  assert(I == I1)
  assert(I1 == I2)
  assert(degree I == 5)
  assert(regularity I == 3)
  assert(dim I == 1)
///

TEST ///
  S = ZZ/101[a..e]
  M = randomBlockMatrix({S^2, S^3}, {S^4, S^5}, {{random, 0}, {random, random}})
  I = pointsIdeal M  
  assert(degree I == 9 and dim I == 1)
  assert(numcols syz gens I == 15)
  J = doubling(10, I)
  assert(degree J == 30 and dim J == 0)
  assert(regularity comodule J == 5)
  assert(pdim comodule J == 5)
///

TEST ///
  S = ZZ/101[a..d]
  Bs = nondegenerateBorels(10, S)
  assert(#Bs == 14)
  for i in Bs do assert(degree i == 10 and isBorel monomialIdeal i)
  
  S = ZZ/101[a..f]
  Bs = nondegenerateBorels(10, S);
  assert(#Bs == 7)
  for i in Bs do assert(degree i == 10 and isBorel monomialIdeal i)

  S = QQ[a..f]
  Bs = nondegenerateBorels(10, S);
  assert(#Bs == 7)
  for i in Bs do assert(degree i == 10 and isBorel monomialIdeal i)
///

TEST ///
  S = ZZ/101[a..d]
  H = bettiStrataExamples S
  for k in keys H do (
      M = first H#k;
      F = quartic M;
      assert(degree F === {4});
      assert(quarticType F === k or k === "[300a]" or k === "[300b]");
      if k === "[300a]" then assert(quarticType F === "[300ab]");
      if k === "[300b]" then assert(quarticType F === "[300ab]");
      )
///

TEST ///
  S = ZZ/101[a..d]
  H = bettiStrataExamples S
  I = inverseSystem quartic first H#"[551]"
  B = betti res I
  assert(# smallerBettiTables B == 16)
///

TEST ///
  kk = ZZ/101
  S = kk[a..d]
  H = bettiStrataExamples S

  K = sort keys H
  F4 = hashTable for k in sort keys H list k => quartic(H#k#0, S)
  pts4 = hashTable for k in keys F4 list k => pointsIdeal(S, first H#k)
  I4 = hashTable for k in keys F4 list k => inverseSystem F4#k
  B4 = hashTable for k in keys F4 list k => betti res inverseSystem F4#k
  netList pack(4, sort pairs oo)

  I4 = (pairs pts4)//sort/last
  I4_0
  assert(doubling(-2, I4_1) === null)
  L = for i from 6 to 8 list (a := doubling(i, I4_0); if a === null then continue else i => a)
  assert(#L == 2)
  assert(L/first == {7,8})
  
  for k in sort keys H list k => minimalBetti doubling(8, pts4#k)
///

end--

-* Development section *-
restart
debug needsPackage "QuaternaryQuartics"
check "QuaternaryQuartics"

restart
installPackage "GroebnerStrata"
uninstallPackage "QuaternaryQuartics"
restart
installPackage "QuaternaryQuartics"
viewHelp "QuaternaryQuartics"
check QuaternaryQuartics

