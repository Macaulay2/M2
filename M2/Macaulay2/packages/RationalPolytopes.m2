newPackage(
    "RationalPolytopes",
    Version => "1.0",
    Date => "",
    Headline => "A package for Ehrhart theory of rational polytopes",
    Authors => {
        {Name => "Oliver Clarke", Email => "oliver.clarke@ed.ac.uk", HomePage => "https://www.oliverclarkemath.com/"},
        {Name => "Victoria Schleis", Email => "victoria.m.schleis@durham.ac.uk", HomePage => "https://victoriaschleis.github.io/"},
        {Name => "Vincenzo Reda", Email => "redav@tcd.ie", HomePage => "https://sites.google.com/tcd.ie/vincenzo-reda/home-page"},
        {Name => "Mike Cummings", Email => "mike.cummings@uwaterloo.ca", HomePage => "https://mikecummings.ca"}
        },
    AuxiliaryFiles => false,
    DebuggingMode => true,
    PackageExports => {"Polyhedra", "Normaliz"},
    HomePage => "https://github.com/Macaulay2/Workshop-2025-Madison/tree/EhrhartTheory/Durham_2024" -- Temporary Homepage
    )

export {
    "hStarPolynomial",
    "hStarVector",
    "ehrhartQuasiPolynomial",
    "isPeriod",
    "quasiPolynomial",
    "period",
    "display",
    "coefficientMonomial",
    "ehrhartSeries",
    "ReturnDenominator",
    "QuasiPolynomial",
    "toPolynomial"
    }


-- define the denominator of polyhedron to be the lcm of all denominators of
-- coordinates of its vertices
denominator Polyhedron := ZZ => P -> (
    if not P.cache#?"denominator" then (
        P.cache#"denominator" = lcm for j in flatten entries vertices P list denominator promote(j,QQ);
        );
    P.cache#"denominator"
    )


-- isPeriod(M, q)
-- M: Matrix
-- q: ZZ
-- returns true if for every j:
-- M^{j*q .. (j+1)*q-1} == M^{0 .. q-1}
isPeriod = method()
isPeriod(Matrix, ZZ) := Boolean => (M, q) -> (
    if numRows M % q != 0 then false
		else (
        result := for j from 0 to numRows M // q - 1 do (
            if M^{0 .. q-1} != M^{j*q .. (j+1)*q-1} then break false;
            );
				if result === null then true else false
        )
    )


-- collapseMatrix M
-- M: Matrix
-- returns the smallest matrix that represents the
-- same quasi-polynomial as M
collapseMatrix = method()
collapseMatrix Matrix := Matrix => M -> (
    q := for p from 1 to (numRows M)//2 do (
        if isPeriod(M, p) then break p;
        );
		if q =!= null then (
        M^{0 .. q-1}
        ) else M
    )


-* QuasiPolynomial Type *-

-- Definition of the Type QuasiPolynomial
QuasiPolynomial = new Type of HashTable

quasiPolynomial = method()
quasiPolynomial Matrix := M -> (
    MCollapsed:= collapseMatrix M;
    new QuasiPolynomial from {
        period => numRows MCollapsed,
        coefficients => MCollapsed,
        cache => new CacheTable,
        }
    )

net QuasiPolynomial := QP -> (
    description := "QuasiPolynomial of degree " | net(numColumns(QP#coefficients)-1) | " and of period " | net(QP#period);
    displayQP := net display(QP, Truncate => 2);
    stack {description, displayQP}
    )

-- construction of quasi-polynomial from a list of constituents
-- i.e. 'rows' of the matrix of coefficients
quasiPolynomial List := L -> (
    if not isMember(false, for l in L list instance(l,List)) then (
        D:=max for p in L list length p;
        L1:=for p in L list ((for i in 0..D-length p -1 list 0)|p);
        quasiPolynomial(matrix(L1))
        )
    else if not isMember(false, for l in L list instance(class l,PolynomialRing)) then(
        if not isMember(false, for l in L list numgens class l==1) then (
            D1:=max for p in L list (degree p)#0;
            lM:=for p in L list sub( (coefficients(p, Monomials=>for d in 0..D1 list ((generators class p)#0)^(D1-d)))#1, QQ);
            M:=transpose fold((a,b) -> a|b, lM);
            quasiPolynomial(M)
            )
        )
    )

-- note that we borrow === function to determine equality of
-- QuasiPolynomials because we collapse the coefficients matrix
QuasiPolynomial == QuasiPolynomial := (QP1, QP2) -> QP1 === QP2


-- QuasiPolynomial as a function
QuasiPolynomial ZZ := (QP, t) -> (
    internalQuasiPolynomial(QP, t)
    )


internalQuasiPolynomial = method()
internalQuasiPolynomial(QuasiPolynomial, ZZ) := QQ => (QP, i) -> (
    local j;
    if not QP#cache#?"polynomialList" then (
        M := QP#coefficients;
        RMonoid := monoid[getSymbol "t"];
        R := QQ RMonoid;
        t := first gens R;
        r := numRows M;
        c := numColumns M;
        polynomials := for i from 0 to r - 1 list (
            p := 0_R;
            for j from 0 to c - 1 do (
                p = p + M_(i,j) * t^(c - 1 - j);
                );
            p
            );
        QP#cache#"polynomialList" = polynomials;
        j = i % QP#period;
        polynomials_j(i)
        )
    else (
        j = i % QP#period;
        QP#cache#"polynomialList"_j(i)
        )
    )

---------------------------------------------------
-- Various methods associated to a QuasiPolynomial
---------------------------------------------------

-- display QuasiPolynomial in a friendly way
display = method(
    Options => {
        Truncate => null -- null or integer
        }
    )

display(QuasiPolynomial) := opts -> QP -> (
    R := monoid[getSymbol "t"];
    t := R_0;
    if opts.Truncate === null or QP#period - 2 <= opts.Truncate or QP#period < 3 then (
        if QP#period == 1 then (
            sum for d in 0 .. (numColumns QP#coefficients)-1 list (
                (QP#coefficients)_(0,d) expression if d < (numColumns QP#coefficients)-1 then (
                    t^(numColumns (QP#coefficients)-d-1)
                    )
                else ""
                )
            )
        else (
            sum for d in 0 .. (numColumns QP#coefficients)-1 list (
                (QP#coefficients)_{d} expression if d < (numColumns QP#coefficients)-1 then (
                    t^(numColumns (QP#coefficients)-d-1)
                    )
                else ""
                )
            )
        )
    else (
        sum for d in 0 .. (numColumns QP#coefficients)-1 list (
            M := (QP#coefficients)_{d};
            Mcut := M^{0 .. opts.Truncate - 1, numRows QP#coefficients - 1};
            Mnet := net Mcut;
            netRows := (for i from 0 to opts.Truncate - 1 list Mnet#i) | {
                "| " | concatenate(width Mnet - 4: ".") | " |"} | {Mnet#(-1)};
            (stack netRows) expression if d < (numColumns QP#coefficients)-1 then (
                t^(numColumns (QP#coefficients)-d-1)
                )
            else ""
            )
        )
    )


degree QuasiPolynomial := QP -> (
    numColumns(QP#coefficients)-1
    )

period = method()
period QuasiPolynomial := QP -> (
    QP#period
    )

coefficients QuasiPolynomial := opts -> QP -> (
    QP#coefficients
		)

-- coefficientMonomial(QP, i)
-- QP: QuasiPolynomial
-- i : ZZ
-- returns the coefficient of t^i in the QuasiPolynomial
coefficientMonomial = method()
coefficientMonomial(QuasiPolynomial,ZZ) := (QP,i) -> (
    if i < degree(QP)+1 then M:=QP#coefficients_{degree(QP)-i};
    if i > degree(QP) then M=0;
    M
    )


toPolynomial = method();

toPolynomial QuasiPolynomial := RingElement => QP -> (
    if QP#period != 1 then
        error "QuasiPolynomial does not have period 1";
    R := QQ[getSymbol "t"];
    t := R_"t";
    M := QP#coefficients;
    d := numColumns M - 1;
    polynomial := 0_R;
    for j from 0 to d do (
        polynomial = polynomial + M_(0,j) * R_0^(d-j);
    );
    polynomial
)


-* Ehrhart Polynomial part *-
------------------------------

-- ehrhartConstituents --
-- if p is the period of the polytope P then
-- ehrhartConstituents(P, i) gives the i-th part of the
-- of the Ehrhart quasi-polynomial of P
-- in other words the (i mod p)th row of the matrix of
-- coefficients of the Quasi polyomial

ehrhartConstituents = method(TypicalValue=>RingElement)
ehrhartConstituents(Polyhedron, ZZ):=(P, i) -> (
    n:=dim P;
    k:=denominator P;
    R:=QQ[getSymbol "x"];
    x:=R_"x";
    S:=for j from 0 to n list i+j*k;
    if n==0 and (not isEmpty P) then return 1+0*x;
    if isEmpty P then return 0+0*x;
    v:=matrix apply(S,h->(
            if h == 0 then {0}
            else {-1+#latticePoints(h*P)}
            )
        );
    v=promote(v,QQ);
    M:=promote(matrix apply(S,a->reverse apply(n+1,j->( a^j ))),QQ);
    M=flatten entries((inverse M)*v);
    1+sum apply(n+1,a->M_(a)*x^(n-a))
    )


ehrhartQuasiPolynomial = method(
    Options => {
        Strategy => "Normaliz" -- Normaliz or M2
        }
    )

ehrhartQuasiPolynomial Polyhedron := QuasiPolynomial => opts -> P -> (
    if not P#cache#?"ehrhartQP" then (
        QP := if opts.Strategy == "Normaliz" then (
            ehrhartQPNormaliz P
            )
        else if opts.Strategy == "M2" then (
            ehrhartQPM2 P
            )
        else error("unknown Strategy option: " | toString opts.Strategy | "; allowable options are Normaliz (default), M2");
        QP#cache#"OriginalPolyhedron" = P;
        P#cache#"ehrhartQP" = QP;
        );
    P#cache#"ehrhartQP"
    )

ehrhartQPM2 = method()
ehrhartQPM2 Polyhedron := P -> (
    k := denominator P;
    quasiPolynomial(for i from 0 to k-1 list ehrhartConstituents(P,i))
    )

-- use the EhrharhtSeries of P (computed with Normaliz)
-- to construct the Quasi-Polynomial

ehrhartQPNormaliz = method()
ehrhartQPNormaliz Polyhedron := P -> (
    ES := value ehrhartSeries(P, Strategy => "Normaliz");
    R := ring ES;
    t := R_0;
    n := dim P;
    k := denominator P;
    latticePointCounts := for i from 0 to (n+1)*k -1 list (
        numberOfLatticePoints := sub(ES,t=>0);
        ES = (ES - numberOfLatticePoints)/t;
        numberOfLatticePoints
        );
    R' := QQ[getSymbol "x"];
    x := R'_0;
    QuasiPolyList := for i from 0 to k-1 list (
        S:=for j from 0 to n list i+j*k;
        if n==0 and (not isEmpty P) then return 1+0*x;
        if isEmpty P then return 0+0*x;
        v := matrix(QQ, apply(S,h -> {-1+latticePointCounts_h}));
        M := matrix(QQ, apply(S,a -> reverse apply(n+1,j ->  a^j )));
        M = flatten entries((inverse M)*v);
        1 + sum apply(n+1 , a -> M_a*x^(n-a))
        );
    quasiPolynomial(QuasiPolyList)
    )



hStarPolynomial = method(
    Options => {
        ReturnDenominator => false, --returns a pair of polys (h, d) s.t. Ehrhart series is h/d
        Strategy => "Normaliz" -- either Normaliz or M2
        })

hStarPolynomial(Polyhedron, Ring) := RingElement => opts -> (P, R) -> (
    if numgens R < 1 then error("ring must have at least one generator");
    if not P#cache#?"ehrhartSeriesNumerator" then (
        if opts.Strategy == "M2" then (
            hStarPolynomialM2(P, R);
            )
        else if opts.Strategy == "Normaliz" then (
            hStarPolynomialNormaliz(P, R);
            )
        else error("unknown Strategy option: " | toString opts.Strategy | "; allowable options are Normaliz (default), M2");
        );

    if opts.ReturnDenominator then (
        P#cache#"ehrhartSeriesNumerator",
        P#cache#"ehrhartSeriesDenominator"
        )
    else P#cache#"ehrhartSeriesNumerator"
    )

hStarPolynomial Polyhedron := RingElement => opts -> P -> (
    if P#cache#?"ehrhartSeriesNumerator" and P#cache#?"ehrhartSeriesDenominator" then (
        if opts.ReturnDenominator then (
            P#cache#"ehrhartSeriesNumerator",
            P#cache#"ehrhartSeriesDenominator"
            )
        else P#cache#"ehrhartSeriesNumerator"
    )
    else (
        R:=QQ[getSymbol "T"]; -- potentially redundant if hStarPolynomial has already been computed
        hStarPolynomial(P, R, opts)
    )
    )

------------------------------------------------
-- Note on hStar computation storage in cache --
--                                            --
-- Once the Ehrhart series has been computed, --
-- the result is a rational function N/D      --
-- which should be stored in P#cache with:    --
--                                            --
-- P#cache#"ehrhartSeriesNumerator"   = N     --
-- P#cache#"ehrhartSeriesDenominator" = D     --
--                                            --
-- Both or neither entry should be present.   --
------------------------------------------------


-- M2 version of hStarPolynomial polynomial
-- once computed, it updates the cache
hStarPolynomialM2 = method()
hStarPolynomialM2(Polyhedron, Ring) := (P, R) -> (
    n:=dim P;
    dnom := denominator P;
    p:=1;
    t:=R_0;
    for i from 1 to (n+1)*dnom do (p=p + #latticePoints(i*P) * t^i);
    r:=(1-t^dnom)^(n+1);
    rhold := (hold 1-t^dnom)^(n+1);
    f := (p*r) % t^((n+1)*dnom);
    P#cache#"ehrhartSeriesNumerator" = f;
    P#cache#"ehrhartSeriesDenominator" = rhold;
    (f, rhold)
    )


-- Normaliz version of hStarPolynomial polynomial
-- once computed, it updates the cache

hStarPolynomialNormaliz = method()
hStarPolynomialNormaliz(Polyhedron, Ring) := (P, R) -> (
    t := R_0;
    C := normaliz(transpose vertices P, "polytope"); -- Maybe all of this data can be stored for later use
    numeratorCoefficients := C#"inv"#"hilbert series num";
    denominatorFactors := C#"inv"#"hilbert series denom";
    denomP := denominator P;
    f := sum for i from 0 to #numeratorCoefficients -1 list (numeratorCoefficients#i) * t^i;
    r := product for i from 0 to #denominatorFactors -1 list 1 - t^(denominatorFactors#i);
    d := (1 - t^denomP)^(dim P + 1);
    D := (hold 1 - t^denomP)^(dim P + 1);
    h := (d // r) * f;
    P#cache#"ehrhartSeriesNumerator" = h;
    P#cache#"ehrhartSeriesDenominator" = D;
    (h, D)
    )


hStarVector = method(
    Options => {
        Strategy => "Normaliz" -- either Normaliz or M2
    })

hStarVector(Polyhedron) := List => opts -> P -> (
    h := if P#cache#?"ehrhartSeriesNumerator" then P#cache#"ehrhartSeriesNumerator" else hStarPolynomial(P, Strategy=>opts.Strategy);
    if #(support h) > 1 then error("expected single-variable polynomial");
    x := first support h;
    reverse apply(terms h, m -> sub(m, x => 1))
)

ehrhartSeries = method(
    Options => {
        Strategy => "Normaliz" -- Normaliz or M2
        }
    )

ehrhartSeries(Polyhedron, Ring) := opts -> (P, R) -> (
    if not P#cache#?"ehrhartSeries" then (
        (h, d) := hStarPolynomial(P, R, ReturnDenominator => true, Strategy => opts.Strategy);
        P#cache#"ehrhartSeries" = h/d;
        );
    P#cache#"ehrhartSeries"
    )

ehrhartSeries Polyhedron := opts -> P -> (
    if P#cache#?"ehrhartSeriesNumerator" then (
        P#cache#"ehrhartSeries" = P#cache#"ehrhartSeriesNumerator" / P#cache#"ehrhartSeriesDenominator"
        )
    else (
        ehrhartSeries(P, QQ[getSymbol "T"], opts)
        )
    )


---------------------------------------
-* Documentation section *-
---------------------------------------
beginDocumentation()

doc ///
  Key
    RationalPolytopes
  Headline
    Ehrhart theory of rational polytopes
  Description
    Text
      A polytope $P \subseteq \RR^n$ is rational if its vertices
      have rational coordinates. This package provides methods
      for computing Ehrhart quasi-polynomials and Ehrhart series
      for rational polytopes.

  References
    @UL{
    {"[BR] M. Beck and S. Robins, ", HREF("https://doi.org/10.1007/978-1-4939-2969-6", EM "Computing the continuous discretely"), ", second edition, Undergraduate Texts in Mathematics, Springer, New York, 2015"}
    }@

  Contributors
    In addition to the authors below, we thank the
    following participants of the
    HREF("https://sites.google.com/view/durhamcompalggeom/home","Computational Algebraic Geometry Workshop")
    at the University of Durham (November 2024)
    for their contributions to the package.

    @UL {
    {"Danny Henderson"},
    {HREF("https://alexandermilner.github.io/", "Alex Milner")},
    {"Benoît Guerville-Ballé"},
    {"Charlotte Satchwell"}
    }@

  Subnodes
    -- Methods
    hStarPolynomial
    hStarVector
    ehrhartQuasiPolynomial
    isPeriod
    quasiPolynomial
    period
    display
    coefficientMonomial
    ehrhartSeries
    toPolynomial
    -- Optional arguments
    ReturnDenominator
///


doc ///
  Key
    ehrhartQuasiPolynomial
    (ehrhartQuasiPolynomial, Polyhedron)
    [ehrhartQuasiPolynomial, Strategy]
  Headline
    Ehrhart quasi-polynomial of a rational polytope
  Usage
    QP = ehrhartQuasiPolynomial(P)
  Inputs
    P : Polyhedron
      the polyhedron for which we calculate the Ehrhart quasi-polynomial
    Strategy => String
      either "Normaliz" or "M2", selects the method for computing the Ehrhart quasi-polynomial
  Outputs
    QP : QuasiPolynomial
      the Ehrhart quasi-polynomial of P
  Description
    Text
      The function calculates the Ehrhart quasi-polynomial of the polyhedron P and returns
      a @TO "QuasiPolynomial"@ object representing it. We can treat the quasi-polynomial
      as a function and evaluate it at integers, which gives the number of lattice points in
      the $n$th dilate of the polytope.
    Example
      QP = ehrhartQuasiPolynomial(convexHull transpose matrix "0,0;1/2,0;0,1/2")
      QP 5
      QP 10
///

doc ///
  Key
    toPolynomial
    (toPolynomial, QuasiPolynomial)
  Headline
    convert a quasi-polynomial of period one to a polynomial
  Usage
    p = toPolynomial(QP)
  Inputs
    QP : QuasiPolynomial
      a quasi-polynomial with period equal to one
  Outputs
    p : RingElement
      the corresponding polynomial in a univariate polynomial Ring over QQ
  Description
    Text
      The function calculates the corresponding polynomial to a period one quasi-polynomial.
    Example
      QP = ehrhartQuasiPolynomial(convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}})
      p = toPolynomial QP
  Caveat
    An error is raised if the period is not one.
///

doc ///
  Key
    hStarPolynomial
    (hStarPolynomial, Polyhedron)
    (hStarPolynomial, Polyhedron, Ring)
    [hStarPolynomial, Strategy]
    [hStarPolynomial, ReturnDenominator]
  Headline
    $h^*$-polynomial of a polytope
  Usage
    hStarPolynomial P
    hStarPolynomial(P,R)
  Inputs
    P : Polyhedron
      a convex polyhedron which must be compact
    R : Ring
      a ring in at least one variable
    ReturnDenominator => Boolean
      whether to return the denominator of the Ehrhart series
    Strategy => String
      either "Normaliz" or "M2", selects the method for computing
      the Ehrhart series
  Outputs
    f : RingElement
      the $h^*$-polynomial (in the ring R) of $P$
  Description
    Text
      If $P$ is a lattice polytope then this function returns the
      $h^*$-polynomial of $P$. Result is cached in P.
    Example
      hStarPolynomial convexHull transpose matrix "-1,0; 0,-1; 1,0; 0,1"
      hStarPolynomial convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}}
    Text
      To return the denominator of the Ehrhart series, set the
      optional argument @TO ReturnDenominator@ to @TT "true"@.
      In this case, the result is a pair that consists of the numerator (as a RingElement)
      and denominator (as an @TO Expression@) of the Ehrhart series.
    Example
      hStarPolynomial(convexHull transpose matrix "0; 1/2",
      Strategy => "Normaliz", ReturnDenominator => true)
  SeeAlso
    RationalPolytopes
    ehrhartSeries
///


doc ///
  Key
    hStarVector
    (hStarVector, Polyhedron)
    [hStarVector, Strategy]
  Headline
    $h^*$-vector of a polytope
  Usage
    hStarVector P
  Inputs
    P : Polyhedron
      a convex polyhedron which must be compact
    Strategy => String
      either "Normaliz" or "M2", selects the method for computing
      the Ehrhart series
  Outputs
    : List
      the $h^*$-vector of $P$
  Description
    Text
      If $P$ is a lattice polytope then this function returns the
      $h^*$-vector of $P$.
      The $i$-th entry in the $h^*$-vector is the coefficient
      of the $x^{i-1}$ term of the $h^*$-polynomial.
    Example
      hStarVector convexHull transpose matrix "-1,0; 0,-1; 1,0; 0,1"
      P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}};
      hStarPolynomial P
      hStarVector P
  SeeAlso
    RationalPolytopes
    hStarPolynomial
    ehrhartSeries
///


doc ///
  Key
    isPeriod
  Headline
    check the period of a rational polytope
  Usage
    result = isPeriod(M,q)
  Inputs
    M : Matrix
    q : ZZ
  Outputs
    result : Boolean
      result is true if q is the period and false otherwise
  Description
    Text
      It returns true if q is the period and false otherwise
    Example
      isPeriod(matrix "1,1;2,2;1,1;2,2",2)
      isPeriod(matrix "1,1;2,2;1,1;3,2",2)
      isPeriod(matrix "1,2,5;2,4,3;1,2,5;3,4,5;1,2,5;2,4,3;1,2,5;3,4,5",3)
  SeeAlso
    RationalPolytopes
///

doc ///
  Key
    quasiPolynomial
    QuasiPolynomial
    (symbol SPACE, QuasiPolynomial, ZZ)
  Headline
    Ehrhart quasi-polynomial of a rational polytope
  Usage
    QP = quasiPolynomial(M)
    QP = quasiPolynomial(L)
    s = QP n
  Inputs
    M : Matrix
    L : List
    QP : QuasiPolynomial
    n : ZZ
  Outputs
    QP : QuasiPolynomial
      a quasi-polynomial whose coefficient matrix is M, or whose constituent polynomials are listed in L
    s : QQ
      the evaluation of the quasi-polynomial QP at $n$
  Description
    Text
      Computes the quasi-polynomial with coefficient matrix M.
      Computes the quasi-polynomial with constituent polynomials listed in L.
    Example
      R = QQ[getSymbol "t"]
      t = R_0
      quasiPolynomial(matrix{{1/18,1/2,1},{1/18,7/18,5/9},{1/18,5/18,2/9}})
      quasiPolynomial({1/18*t^2+1/2*t+1,1/18*t^2+7/18*t+5/9,1/18*t^2+5/18*t+2/9})
    Text
      To evaluate the quasi-polynomial at an integer $n$, we evaluate the $n$th (mod period)
      constituent polynomial of the quasi-polynomial at $n$
    Example
      P = convexHull transpose matrix "0,0; 1/5,0; 0,1/5";
      QP = ehrhartQuasiPolynomial P;
      QP 2
      QP 7
      QP 9
    Text
      If $n$ is a positive integer, then the evaluation coincides with the number of lattice points
      in the $n$th dilation of the polytope.
  SeeAlso
    RationalPolytopes
///

doc ///
  Key
    period
  Headline
    period of a rational polytope
  Usage
    p = period(QP)
  Inputs
    QP : QuasiPolynomial
      a quasi-polynomial of which we want to compute the period
  Outputs
    p : ZZ
      the period of QP
  Description
    Text
      Computes the period of a quasi-polynomial
    Example
      period(quasiPolynomial(matrix{{1,2,3},{1,4,5}}))
      QP1 = quasiPolynomial(matrix{{1,1},{1,2},{1,1},{1,2}})
      period QP1
      QP2 = quasiPolynomial(matrix{{1,1},{1,2},{1,1},{1,1}})
      period QP2
  SeeAlso
    RationalPolytopes
///

doc ///
  Key
    display
    (display, QuasiPolynomial)
    [display, Truncate]
  Headline
    display a quasi-polynomial
  Usage
    display QP
  Inputs
    QP : QuasiPolynomial
      a quasi-polynomial to display
    Truncate => ZZ
      the starting row index of the coefficient matrix truncated from the output   
  Outputs
    : Sum
      a display of the quasi-polynomial
  Description
    Text
      Displays the quasi-polynomial in a friendly way.
    Example
      P = convexHull transpose matrix {{0,0},{1/7,0},{0,1/7}};
      p = ehrhartQuasiPolynomial P;
      display(p)
      display(p, Truncate => 4)
  SeeAlso
    RationalPolytopes
    quasiPolynomial
///


doc ///
  Key
    coefficientMonomial
  Headline
    list of coefficients in a quasi-polynomial
  Usage
    L = coefficientMonomial(QP,i)
  Inputs
    QP : QuasiPolynomial
      a quasi-polynomial of which we want to know the coefficients
    i : ZZ
      the degree of the monomials of QP of which we want to know the coefficients
  Outputs
    L : List
      the coefficients of the monomials of degree i appearing in QP
  Description
    Text
      Computes the coefficients of the monomials of degree i appearing in QP
    Example
      coefficientMonomial(quasiPolynomial(matrix{{1,2,3},{1,4,5}}),2)
      coefficientMonomial(quasiPolynomial(matrix{{1,1},{1,2},{1,1},{1,2}}),0)
      coefficientMonomial(quasiPolynomial(matrix{{3,6,7,2},{3,4,4,2},{3,2,5,6}}),2)
  SeeAlso
    RationalPolytopes
    quasiPolynomial
///

doc ///
  Key
    ehrhartSeries
    (ehrhartSeries, Polyhedron, Ring)
    (ehrhartSeries, Polyhedron)
    [ehrhartSeries, Strategy]
  Headline
    Ehrhart series of a polytope
  Usage
    ES = ehrhartSeries(P, R)
    ES = ehrhartSeries(P)
  Inputs
    P : Polyhedron
      the (rational) polyhedron whose Ehrhart series we wish to know
    R : Ring
      a ring in at least one variable
    Strategy => String
      either "Normaliz" or "M2", selects the method for computing the Ehrhart series
  Outputs
    ES : RingElement
      Ehrhart series in the ring R, or in frac(QQ[t]) if R not specified
  Description
    Text
      Computes the Ehrhart series of the (rational) polyhedron P. Result is cached in P.
    Example
      P = convexHull transpose matrix {{-1,0},{0,1/2},{0,-1/2},{1,0}}
      ehrhartSeries P
  Caveat
    To be consistent with definitions in the literature, the rational function is not necessarily simplified: the numerator and the denominator may share common factors.
    The output is an @TO Expression@. For further algebraic computation, use value().
  SeeAlso
    hStarPolynomial
    RationalPolytopes
  ///



-* Test section *-
TEST /// -- (hStarPolynomial)
R = QQ[t]
assert(1_R == hStarPolynomial(convexHull transpose matrix "0,0,0;1,0,0;0,1,0;0,0,1",R))
assert(t^5+3*t^4+4*t^3+4*t^2+3*t+1 == hStarPolynomial(convexHull transpose matrix "1,0;-1,0;0,1/2;0,-1/2",R, Strategy => "Normaliz"))
assert(t+1 == hStarPolynomial(convexHull transpose matrix "0; 1/2",R, Strategy => "M2"))
assert(t^5+t^3+t^2+1 == hStarPolynomial(convexHull transpose matrix "1/4; 1/2",R))
///

TEST /// -- (isPeriod)
assert(true == isPeriod(matrix "1,1;2,2;1,1;2,2",2))
///

TEST /// -- (period)
assert(2 == period(quasiPolynomial(matrix{{1,1},{1,2},{1,1},{1,2}})))
///

TEST /// -- (coefficientMonomial)
assert(matrix{{1},{1}} == coefficientMonomial(quasiPolynomial(matrix{{1,2,3},{1,4,5}}),2))
///

TEST /// -- (coefficients QuasiPolynomial)
P = convexHull transpose matrix "0,0;0,1;1,0";
p = ehrhartQuasiPolynomial P;
assert(coefficients p == matrix "1/2, 3/2, 1")
///

TEST /// -- (ReturnDenominator)
P = convexHull transpose matrix "0,0;0,1/2;1/3,0";
H = hStarPolynomial(P, ReturnDenominator => true);
assert(class H === Sequence);
assert(#H == 2);
///

TEST /// -- (ReturnDenominator) using cached result
P = convexHull transpose matrix "0,0;0,1/2;1/3,0";
H = hStarPolynomial(P, ReturnDenominator => true);
H = hStarPolynomial(P, ReturnDenominator => true); -- second call from cache
assert(class H === Sequence);
assert(#H == 2);
///

TEST /// -- (denominator)
P = convexHull transpose matrix "0,0;0,1/3;1/2,0";
assert(denominator P == 6)
///

TEST /// -- (ehrhartSeries)
P = convexHull transpose matrix "0,0;0,1/3;1/2,0";
ES = ehrhartSeries P;
assert(class ES === Divide);
(N, D) = hStarPolynomial(P, ReturnDenominator => true);
assert value (ES#0 == N);
assert value (ES#1 == D);
///

TEST /// -- (hStarVector)
P = convexHull transpose matrix "-1,0; 0,-1; 1,0; 0,1";
assert(hStarVector P == {1,2,1})
///

TEST /// -- (ehrhartQuasiPolynomial)
P = convexHull transpose matrix "0,0; 1/2,0; 0,1/2";
p = ehrhartQuasiPolynomial P;
q = quasiPolynomial matrix{{1/8, 3/4, 1}, {1/8, 1/2, 3/8}};
assert(p==q)
///

TEST /// -- (display)
P = convexHull transpose matrix "0,0; 1/5,0; 0,1/5";
R := monoid[getSymbol "t"];
t := R_0;
p = ehrhartQuasiPolynomial P;
q = (p#coefficients)_{0} expression t^2 + (p#coefficients)_{1} expression t + (p#coefficients)_{2} expression "";
assert(class q === Sum)
assert(toString q == toString display(p))
///

TEST /// -- (Truncate)
P = convexHull transpose matrix "0,0; 1/7,0; 0,1/7";
R := monoid[getSymbol "t"];
t := R_0;
p = ehrhartQuasiPolynomial P;
trunc = 4;
q = sum for d in 0 .. (numColumns p#coefficients)-1 list (
            M := (p#coefficients)_{d};
            Mcut := M^{0 .. trunc - 1, numRows p#coefficients - 1};
            Mnet := net Mcut;
            netRows := (for i from 0 to trunc - 1 list Mnet#i) | {
                "| " | concatenate(width Mnet - 4: ".") | " |"} | {Mnet#(-1)};
            (stack netRows) expression if d < (numColumns p#coefficients)-1 then (
                t^(numColumns (p#coefficients)-d-1)
                )
            else ""
            );
assert(class q === Sum)
assert(toString q == toString display(p, Truncate => trunc))
///

TEST /// --(toPolynomial)
P = convexHull transpose matrix "0,0;1,6/7;7,0";
QP = ehrhartQuasiPolynomial P;
p = toPolynomial QP;
q = 3*t^2+4*t+1;
assert(instance(p,RingElement))
assert(q == p)
///

TEST /// -- (internalQuasiPolynomial)
P = convexHull transpose matrix "0,0; 1/5,0; 0,1/5";
QP = ehrhartQuasiPolynomial P;
assert(1==QP 2)
assert(3==QP 7)
assert(3==QP 9)
///

end
----

restart
needsPackage "RationalPolytopes"

--------------------------------------------------

-* Development section *-
restart

uninstallPackage "RationalPolytopes"

restart
installPackage "RationalPolytopes"

viewHelp "RationalPolytopes"
debug needsPackage "RationalPolytopes"

check "RationalPolytopes"


-- Small examples
P=convexHull transpose matrix "0;1/2"
EQP=ehrhartQP(P)


P=convexHull transpose matrix "1,0;-1,0;0,1/2;0,-1/2"

display ehrhartQuasiPolynomial(P) -- period collapse


P=convexHull transpose matrix "-1/2; 1/2"
ehrhartQP(P)


-- Test of the constructor of the Type QuasiPolynomial


S=matrix({{1,2,3},{1,4,5}})
M=matrix({{1,2,3},{0,1,0},{1,2,3},{0,1,0}})

QP=quasiPolynomial(M)
QP#period
print QP
display QP
degree QP
period QP
coefficientMonomial(QP,0)

R=QQ[x]
R1=QQ[t]
L={x^2+3,2*t}
quasiPolynomial(L)

L={{1,0,3},{2,0}}
quasiPolynomial(L)

-----------------------------
restart
needsPackage "RationalPolytopes"

debug RationalPolytopes

P = convexHull transpose matrix "1,0;-1,0;0,1/20;-1,11/20"
latticePoints P
ehrhartQuasiPolynomial(P)
display(ehrhartQP P, Truncate => 1000)

hStarPolynomial P
ehrhartSeries P

hStarPolynomial(P, Strategy => "M2")
ehrhartSeries(P, Strategy => "M2")
