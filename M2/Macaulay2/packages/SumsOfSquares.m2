newPackage(
    "SumsOfSquares",
    Version => "2.2",
    Date => "May 2021",
    Authors => {
     {Name => "Diego Cifuentes",
      Email => "diegcif@mit.edu",
      HomePage => "http://www.mit.edu/~diegcif/"},
     {Name => "Thomas Kahle",
      Email => "thomas.kahle@ovgu.de",
      HomePage => "https://thomas-kahle.de/"},
     {Name => "Pablo A. Parrilo",
      Email => "parrilo@mit.edu",
      HomePage => "http://www.mit.edu/~parrilo/"},
     {Name => "Helfried Peyrl",
      Email => "peyrl@control.ee.ethz.ch",
      HomePage => "https://scholar.google.com/citations?user=cFOV7nYAAAAJ&hl=de"},
     {Name => "Special thanks: Ilir Dema, Nidhi Kaihnsa, Anton Leykin"}
    },
    Headline => "sums of squares",
    Keywords => {"Real Algebraic Geometry"},
    AuxiliaryFiles => true,
    PackageImports => {"FourierMotzkin"},
    PackageExports => {"SemidefiniteProgramming"},
    Certification => {
        "journal name" => "The Journal of Software for Algebra and Geometry",
        "journal URI" => "http://j-sag.org/",
        "article title" => "Sums of squares in Macaulay2",
        "acceptance date" => "6 January 2020",
        "published article URI" => "https://msp.org/jsag/2020/10-1/p03.xhtml",
        "published article DOI" => "https://doi.org/10.2140/jsag.2020.10.17",
        "published code URI" => "https://msp.org/jsag/2020/10-1/jsag-v10-n1-x03-SumsOfSquares.zip",
        "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/....m2",
        "release at publication" => "7623915208e2f52445cd73c9acd680027bba1d9c",     -- git commit number in hex
        "version at publication" => "2.1",
        "volume number" => "10",
        "volume URI" => "https://msp.org/jsag/2020/10-1/"
        }
)

export {
--Types
    "SOSPoly",
    "SDPResult",
--Methods/Functions
    "sosPoly",
    "solveSOS",
    "sosdecTernary",
    "sosInIdeal",
    "lowerBound",
    "checkSolver",
    "recoverSolution",
    "library",
--Method options
    "GramMatrix",
    "MomentMatrix",
    "Status",
    "RoundTol",
    "TraceObj"
}

--##########################################################################--
-- GLOBAL VARIABLES
--##########################################################################--

-- Constants
MaxRoundTol = 32 --maximum rounding tolerance
HighPrecision = 1e-10 --e.g. for numerical linear algebra
MedPrecision = 1e-6 --e.g. for SDP solutions
LowPrecision = 1e-4

--##########################################################################--
-- TYPES
--##########################################################################--

SOSPoly = new Type of HashTable

sosPoly = method()
sosPoly (Ring, List, List) := (R, polys, coeffs) -> (
    new SOSPoly from {
        ring => R,
        gens => polys,
        coefficients => coeffs
        }
    )
sosPoly (List, List) := (polys, coeffs) -> sosPoly(ring polys#0,polys,coeffs)

ring SOSPoly := S -> S#ring

gens SOSPoly := o -> S -> S#gens

coefficients SOSPoly := o -> S -> S#coefficients

length SOSPoly := S -> #(S#gens)

substitute (SOSPoly,Ring) := (S,R) ->
    sosPoly(for g in S#gens list sub(g,R), S#coefficients)

expression SOSPoly := S ->
    new Sum from apply (S#coefficients,S#generators,(c,g) -> hold c * (hold g)^2)

net SOSPoly := S -> (
    if #gens S == 0 then "0"
    else net expression S
    )

Number * SOSPoly := (a,S) -> (
    if a<0 then error "scalar must be nonnegative";
    if a==0 then sosPoly(ring S, {}, {})
    else sosPoly(ring S, gens S, a * coefficients S)
    )

SOSPoly + SOSPoly := (S,S') -> (
    R := ring S;
    if R =!= ring S' then error "cannot add elements of different rings";
    sosPoly(R,S#gens|S'#gens, S#coefficients|S'#coefficients)
    )

SOSPoly * SOSPoly := (g1,g2)-> (
    if g1#ring =!= g2#ring then error "cannot multiply elements of different rings";
    q1:=for i in g1#gens list(
        for j in g2#gens list i*j);
    q2:=for i in g1#coefficients list(
        for j in g2#coefficients list i*j);
    sosPoly(g1#ring, flatten(q1),flatten(q2))
    )

SOSPoly ^ ZZ := (p1,D)->(
    if D<=0 then error "power should be a positive integer.";
    if odd D then error "power should be an even integer.";
    p2 := (value p1)^(D//2);
    sosPoly(ring p1,{p2},{1})
    )

SOSPoly == RingElement := (S, f) -> (
    if ring S=!=ring f then
        error "Cannot compare elements of different rings. Try to use 'sub'.";
    value S == f
    )

RingElement == SOSPoly := (f, S) -> S == f

SOSPoly == SOSPoly := (S, S') -> S == value S'

SOSPoly == Matrix := (S, F) -> (
    if numRows F!=1 or numColumns F!=1 then
        error "matrices have different shapes"
    else S == F_(0,0)
    )

Matrix == SOSPoly := (F, S) -> S == F

value SOSPoly := a -> sum for i to #(a#gens)-1 list a#gens_i^2 * a#coefficients_i

clean(RR,SOSPoly) := (tol,s) -> (
    if s===null then return (,);
    R := ring s;
    kk := coefficientRing R;
    if kk === QQ then tol=0.;
    g := gens s;
    d := coefficients s;
    I := positions(d, di -> di>tol);
    d = d_I;
    g = g_I;
    if kk =!= QQ then g = clean_tol \ g;
    sosPoly(R,g,d)
    )

SDPResult = new Type of HashTable

sdpResult = (mon,Q,X,tval,sdpstatus) -> (
    new SDPResult from {
        Monomials => mon,
        GramMatrix => Q,
        MomentMatrix => X,
        Parameters => tval,
        Status => sdpstatus
        }
    )

status SDPResult := o -> sol -> sol#Status

net SDPResult := sol -> (
    mat2str := M ->
        if M===null then "null"
        else numRows M | "x" | numColumns M | " matrix over " | toString ring M;
    str := {
        {"MomentMatrix", mat2str sol#MomentMatrix},
        {"GramMatrix", mat2str sol#GramMatrix},
        {"Monomials", mat2str sol#Monomials}
        };
    tval := sol#Parameters;
    if tval=!=null and numRows tval>0 then
        str = append(str,{"Parameters",mat2str tval});
    str = append(str,{"Status",sol#Status});
    netList(str,HorizontalSpace=>3,Alignment=>Left,Boxes=>false)
    )

-- Shortcut to extract keys from SDPResult:
readSdpResult = sol -> (sol#Monomials, sol#GramMatrix, sol#MomentMatrix, sol#Parameters, sol#Status)


--##########################################################################--
-- METHODS
--##########################################################################--

verbose1 = (s,o) -> if o.Verbosity>=1 then print s
verbose2 = (s,o) -> if o.Verbosity>=2 then print s

-- library of nonnegative polynomials
library = method()
library(String,List) := (name,X) -> (
    if #X<3 then error "Insufficient variables";
    x := X_0; y := X_1; z := X_2;
    if name=="Motzkin" then
        return  x^4*y^2 + x^2*y^4 - 3*x^2*y^2*z^2 + z^6;
    if name=="Robinson" then
        return x^6 + y^6 + z^6 - (x^4*y^2 + x^2*y^4 + x^4*z^2 + x^2*z^4 + y^4*z^2 + y^2*z^4) + 3*x^2*y^2*z^2;
    if name=="Schmuedgen" then
        return 200*(x^3 - 4*x*z^2)^2 + 200*(y^3 - 4*y*z^2)^2 +
           (y^2 - x^2)*x*(x + 2*z)*(x^2 - 2*x*z + 2*y^2 - 8*z^2);
    if name=="Scheiderer" then
        return x^4 + x*y^3 + y^4 - 3*x^2*y*z - 4*x*y^2*z + 2*x^2*z^2 + x*z^3 + y*z^3 + z^4;
    if name=="Harris" then(
        (a,b,c,d,e) := (16,-36,20,57,-38);
        return a*( x^10 + y^10 + z^10)+
            b*( x^8* y^2 + x^2* y^8 + x^8* z^2 + x^2* z^8 + y^8* z^2 + y^2* z^8 ) +
            c*( x^6* y^4 + x^4* y^6 + x^6* z^4 + x^4* z^6 + y^6* z^4 + y^4* z^6 ) +
            d*( x^6* y^2* z^2 + x^2* y^6* z^2 + x^2* y^2* z^6) +
            e*( x^4* y^4* z^2 + x^4* y^2* z^4 + x^2* y^4* z^4);
        );
    if #X<4 then error "Insufficient variables";
    w := X_3;
    if name=="Lax-Lax" then
        return (x-y)*(x-z)*(x-w)*x+(y-x)*(y-z)*(y-w)*y+(z-x)*(z-y)*(z-w)*z+(w-x)*(w-y)*(w-z)*w+x*y*z*w;
    if name=="Choi-Lam" then
        return x^2*y^2 + y^2*z^2 + x^2*z^2 + w^4 - 4*x*y*z*w;
    error "Name was not recognized.";
    )
library(String,Ring) := (name,R) -> library(name,gens R)

--###################################
-- Transition QQ <=> RR
--###################################

isExactField = kk -> (
    try (kk = ring kk);
    kk = ultimate(coefficientRing,kk);
    precision 1_kk == infinity
    )

isZero = (tol,x) -> if isExactField x then x==0 else norm x<tol

-- rounds real number to rational
roundQQ = method()
roundQQ(ZZ,RR) := (d,x) -> round(x*2^d)/2^d
roundQQ(ZZ,Matrix) := (d,X) ->
     matrix(QQ, applyTable (entries X, roundQQ_d ));

changeRingField = (kk,R) -> kk(monoid[gens R])

changeMatrixField = (kk, M) -> (
    -- M is a matrix whose entries are polynomials whose coefficient
    -- ring should be changed.
    R := changeRingField(kk, ring M);
    matrix applyTable(entries M, m -> toRing(R,m))
    )

toRing = method ()
toRing (Ring, RingElement) := (S,f) -> (
    -- maps f to ring S
    R := ring f;
    kk := coefficientRing R;
    phi := map(S,R);
    -- QQ => RR
    if kk===QQ then return phi(f);
    -- RR => QQ
    if not instance(kk, RealField) then error "Expecting conversion from real here";
    (mon,coef) := coefficients f;
    mon = matrix {liftMonomial_S \ flatten entries mon};
    prec := precision kk;
    coef = matrix(QQ, {for c in flatten entries coef list roundQQ(prec,sub(c,RR))});
    (mon*transpose coef)_(0,0)
    )

toRing (Ring, SOSPoly) := (S, s) -> (
    -- maps s to ring S
    R := ring s;
    kk := coefficientRing R;
    -- QQ => RR
    if kk===QQ then
        return sosPoly (S, (x -> sub (x, S)) \ gens s,
            (q -> sub (q, kk)) \ coefficients s);
    -- RR => QQ
    if not (instance (kk, RealField) and coefficientRing S===QQ) then
        error "Error: only conversion between real and rational coefficient fields is implemented.";
    g' := toRing_S \ gens s;
    prec := precision kk;
    c' := for c in coefficients s list roundQQ(prec,sub(c,RR));
    sosPoly (S, g', c')
    )

liftMonomial = (S,f) -> (
    -- maps monomial f to ring S
    n := numgens S;
    e := first exponents f;
    e = e_(toList(0..n-1)); -- ignore some variables
    S_e
    )


--###################################
-- basicLinearAlgebra
--###################################

linsolve = (A,b) -> (
    -- This function becomes obsolete when solve
    -- has a threshold for infeasibility.
    if isExactField A then return try solve(A,b);
    tol := HighPrecision;
    x := solve(A,b,ClosestFit=>true);
    if norm(A*x-b) > tol then return;
    x
    )

truncatedSVD = (A,tol) -> (
    -- truncate small (or big) singular values
    (S, U, Vt) := SVD A;
    idx := positions(S, s->(s > abs tol));
    if tol>0 then(
        S = take(S,#idx);
        U = submatrix(U,,idx);
        Vt = submatrix(Vt,idx,);
    )else(
        S = drop(S,#idx);
        U = submatrix'(U,,idx);
        Vt = submatrix'(Vt,idx,); );
    (S,U,Vt)
    )

kernelGens = A -> (
    -- Kernel up to a precision.  Becomes obsolete when M2 gives the
    -- kernel of a numerical matrix.
    if isExactField A then return gens kernel A;
    tol := HighPrecision;
    (S,U,Vt) := truncatedSVD(A,-tol);
    transpose Vt
    )

zeros = (kk,m,n) -> map(kk^m,kk^n,{})

--###################################
-- solveSOS
--###################################

sosPoly(Matrix,Matrix) := (mon,Q) -> (
    if mon===null or Q===null then return;
    (L,D,P) := PSDdecomposition(Q);
    if L===null then (
        print "Gram Matrix is not positive semidefinite";
        return
        );
    n := numRows Q;
    g := toList flatten entries (transpose mon * P * L);
    d := for i to n-1 list D_(i,i);
    idx := positions (d, i->i!=0);
    d = d_idx;
    g = g_idx;
    sosPoly(ring mon,g,d)
    )
sosPoly(SDPResult) := sol -> if sol#GramMatrix=!=null then sosPoly(sol#Monomials, sol#GramMatrix)

-- internal way to call solveSOS
rawSolveSOS = method(
     Options => {RoundTol => 3, Solver=>null, Verbosity => 0, TraceObj => false} )

rawSolveSOS(Matrix,Matrix,Matrix) := o -> (F,objP,mon) -> (
    -- Consider a parametric problem f = f_0 + f_1 p_1 + ... + f_s p_s
    -- This minimizes a function over the p_i
    -- F is a column vector with the f_i
    -- objP is a column vector specifying the objective function
    -- mon is a vector of monomials
    -- (see parameterVector)

    kk := coefficientRing ring F;

    -- checkInputs --
    if numColumns mon > 1 then error("Monomial vector should be a column.");
    isMonomial := max(length \ terms \ flatten entries mon)==1;
    if not isMonomial then error("Vector must consist of monomials.");

    -- build SOS model --
    (C,Ai,p0,V,A,B,b) := createSOSModel(F,mon,Verbosity=>o.Verbosity,RoundTol=>o.RoundTol);
    if C===null then return sdpResult(mon,,,,);

    ndim := numRows C;
    np := numRows objP;

    obj :=
        if o.TraceObj then
            map(kk^(#Ai),kk^1,(i,j)-> trace Ai#i)
        else
            (transpose V * objP);
    if obj==0 then verbose2( "Solving SOS feasibility problem...", o)
    else verbose2("Solving SOS optimization problem...", o);

    (X,my,Q,sdpstatus) := optimize(sdp(C,Ai,obj), Solver=>o.Solver, Verbosity=>o.Verbosity);
    if Q===null then return sdpResult(mon,Q,X,,sdpstatus);
    y := -my;
    pVec0 := (p0 + V*y);

    if not isExactField kk then return sdpResult(mon,Q,X,pVec0,sdpstatus);
    if o.RoundTol > MaxRoundTol then(
        if o.RoundTol < infinity then
            verbose1("Warning: RoundTol is too high. Rounding will be skipped.", o);
        return sdpResult(changeMatrixField(RR,mon),Q,X,pVec0,sdpstatus);
        );

    -- rational rounding --
    (ok,Qp,pVec) := roundSolution(pVec0,Q,A,B,b,RoundTol=>o.RoundTol,Verbosity=>o.Verbosity);
    if ok then return sdpResult(mon,Qp,X,pVec,sdpstatus);
    verbose1("rounding failed, returning real solution", o);
    sdpResult(changeMatrixField(RR,mon),Q,X,pVec0,sdpstatus)
    )

-- Choose monomials internally:
rawSolveSOS(Matrix,Matrix) := o -> (F,objP) -> (
    mon := chooseMons (F,Verbosity=>o.Verbosity);
    if mon===null then return sdpResult(mon,,,,);
    rawSolveSOS(F,objP,mon,o)
    )
rawSolveSOS(Matrix) := o -> (F) ->
    rawSolveSOS(F,zeros(QQ,numRows F-1,1),o)

solveSOS = method(
     Options => {RoundTol => 3, Solver=>null, Verbosity => 0, TraceObj => false} )

solveSOS(RingElement,RingElement,Matrix) := o -> (f,objFcn,mon) -> (
    (F,objP) := parameterVector(f,objFcn);
    rawSolveSOS(F,objP,mon,o)
    )
solveSOS(RingElement,Matrix) := o -> (f,mon) ->
    solveSOS(f,0_(ring f),mon,o)

solveSOS(RingElement,RingElement) := o -> (f,objFcn) -> (
    (F,objP) := parameterVector(f,objFcn);
    mon := chooseMons (F,Verbosity=>o.Verbosity);
    if mon===null then return sdpResult(mon,,,,);
    rawSolveSOS(F,objP,mon,o)
    )
solveSOS(RingElement) := o -> (f) ->
    solveSOS(f,0_(ring f),o)

solveSOS(RingElement,RingElement,ZZ) := o -> (f,objFcn,D) -> (
    (F,objP) := parameterVector(f,objFcn);
    mon := chooseMons(F,D);
    if mon===null then return sdpResult(mon,,,,);
    solveSOS(f,objFcn,mon,o)
    )
solveSOS(RingElement,ZZ) := o -> (f,D) ->
    solveSOS(f,0_(ring f),D,o)

-- Main method to setup an SOS problem as an SDP problem
createSOSModel = method(
    Options => {Verbosity => 0, RoundTol => 3 } )
createSOSModel(RingElement,Matrix) := o -> (f,v) -> (
    F := parameterVector(f);
    createSOSModel(F,v)
    )
createSOSModel(Matrix,Matrix) := o -> (F,v) -> (
    kk := coefficientRing ring F;
    np := numRows F - 1;
    n := numRows v;
    n2 := n*(n+1)//2;

    -- monomials in vvT
    vvT := entries(v* transpose v);
    mons := g -> set first entries monomials g;
    K1 := toList \\ sum \\ mons \ flatten vvT;

    -- monomials in F and not in vvT
    lmf := sum \\ mons \ flatten entries F;
    K2 := toList(lmf - K1);
    K := K1 | K2;

    -- Linear constraints: b
    b := map(kk^#K, kk^1, (i,j) -> coefficient(K#i,F_(0,0)) );

    -- Linear constraints: A, B
    constructA := (K1,vvT,k2) -> (
        vvT' := smat2vec(vvT, Scaling=>2);
        k1 := #K1;
        idx := hashTable for i to k1-1 list K1_i => i;
        a := new MutableList;
        for j to n2-1 do (
            (M,C) := coefficients(vvT'#j);
            C = sub(C,kk);
            for l to numRows(C)-1 list(
                i := idx#(M_(0,l));
                a#(#a) = (i,j)=>C_(l,0);
                )
            );
        map(kk^(k1+k2), kk^n2, toList a)
        );
    constructAold := (K1,vvT,k2) -> (   --old construction is slow
        coeffMat := (x,A) -> applyTable(A, a -> coefficient(x,a));
        A := matrix(kk, for i to #K1-1 list smat2vec(coeffMat(K1_i, vvT),Scaling=>2) );
        A || zeros(kk,k2,n2)
        );
    A := constructA(K1,vvT,#K2);

    -- Consider search-parameters:
    constructB := (K,F) -> (
        F0 := F^(toList(1..np));
        (M,C) := coefficients transpose F0;
        C0 := map(kk^1,kk^(numcols C),{}) || sub(C,kk);
        idx := hashTable for j to numcols(M)-1 list M_(0,j) => j+1;
        rows := for i to #K-1 list if idx#?(K#i) then idx#(K#i) else 0;
        -(C0^rows)
        );
    constructBold := (K,F) ->   --old construction is slow
        map(kk^#K, kk^np, (i,j) -> -coefficient(K#i, F_(j+1,0)) );
    B := constructB(K,F);

    if isExactField(A) and o.RoundTol==infinity then(
        A = sub(A,RR); B = sub(B,RR); b = sub(b,RR);
        );

    (C,Ai,p0,V) := getImageModel(A,B,b,o);

    (C,Ai,p0,V,A,B,b)
    )

getImageModel = (A,B,b,o) -> (
    -- given the affine subspace {Aq + Bp = b}
    -- find a parametrization of the form
    -- Q = C + sum_i ti Ai
    -- p = p0 + V t

    -- compute the C matrix
    n1 := numColumns A;
    n2 := numColumns B;
    AB := A|B;
    x := linsolve(AB,b);
    if x===null then(
        verbose1("No Gram matrix exists. Terminate.", o);
        return (,,,) );
    c := x^{0..n1-1};
    p0 := x^{n1..n1+n2-1};
    C := vec2smat(c);

    -- compute the A_i matrices
    W := - kernelGens AB;
    r := numColumns W;
    U := W^{0..n1-1};
    V := W^{n1..n1+n2-1};
    Ai := toSequence for k to r-1 list vec2smat(U_{k});

    (C,Ai,p0,V)
    )

parameterVector = method()
parameterVector(RingElement,RingElement) := (f,objFcn) -> (
    -- given a polynomial f = f_0 + \sum_i p_i * f_i
    -- the method returns the vector with the f_i's
    R := ring f;
    kk := coefficientRing R;
    if isField kk then (
        if objFcn!=0 then
            error "Objective must be zero if there are no parameters.";
        return (matrix{{f}},zeros(kk,0,1));
        );
    if first degree f > 1 then
        error("Polynomial should depend affinely on the parameters.");
    p := gens R;
    F := matrix for t in {1_R}|p list {coefficient(t,f)};
    if degree objFcn > {1,0} then
        error("Objective should be a linear function of the parameters.");
    kk = coefficientRing kk;
    objP := matrix for t in p list {sub(coefficient(t,objFcn),kk)};
    (F,objP)
    )
parameterVector(RingElement) := (f) -> first parameterVector(f,0_(ring f))

-- Choose monomial basis based on Newton polytope
chooseMons = method(
    Options => {Verbosity => 0} )
chooseMons(RingElement) := o -> (f) -> (
    F := parameterVector(f);
    mon := chooseMons(F,o);
    if mon===null then return;
    sub(mon,ring f)
    )
chooseMons(Matrix) := o -> (F) -> (
    R := ring F;
    if F==0 then error "Expected nonzero inputs.";
    if isQuotientRing R then error("A monomial vector or degree bound must be provided in quotient rings.");
    n:= numgens R;
    monsPoly := g -> set first entries monomials g;
    monsList := G -> if #G>0 then sum(monsPoly\G) else {};
    filterVerts := (verts) -> (
        -- only consider those without parameters
        lmpars := monsList drop(flatten entries F,1);
        return select(verts, v -> not member(R_v,lmpars));
        );
    lmf := monsList flatten entries F;
    falt := sum lmf;

    -- Get exponent-points for Newton polytope:
    points := substitute(matrix (transpose exponents falt),QQ);
    numpoints := numColumns points;
    if numpoints==1 then(
        points = flatten entries sub(points,ZZ);
        if any(points,odd) then(
            verbose1("Newton polytope has odd vertices. Terminate.", o);
            return;
            );
        return matrix{{R_(points//2)}};
        );
    maxdeg := first degree falt;
    mindeg := floor first min entries (transpose points*matrix map(ZZ^n,ZZ^1,i->1));
    maxdegs := apply(entries points, i-> max i);
    mindegs := apply(entries points, i-> min i);

    -- Regard exponent-points in a possible subspace
    shift := first entries transpose points;
    V := matrix transpose apply(entries transpose points, i -> i - shift);
    basV := mingens image V;
    basVdim := numgens image basV;
    if basVdim != n then T := id_(QQ^n)//basV else T = id_(QQ^n);
    basVtrans := kernelGens transpose basV;

    -- Compute Newton polytope:
    liftedpts := T*V || map (QQ^1,QQ^(size falt),i->1);
    dualpolytope := transpose substitute(first fourierMotzkin(liftedpts),QQ);
    bidual := first fourierMotzkin transpose dualpolytope;
    polytope := basV * matrix drop(entries bidual,-1);
    polytope = matrix transpose apply(entries transpose polytope, i -> i + shift);
    polytope = sub(polytope,ZZ);
    oddverts := select(entries transpose polytope, i->any(i,odd));
    if #filterVerts(oddverts)>0 then(
        verbose1("Newton polytope has odd vertices. Terminate.", o);
        return;
        );

    -- Get candidate points
    cp := pointsInBox(mindeg,maxdeg,mindegs,maxdegs);
    verbose2("#candidate points: " | #cp, o);
    -- Only the even ones
    cpf := select(cp,i-> all(i,even));
    verbose2("#even points: " | #cpf, o);
    -- Drop points that do not live on the subspace:
    cpf2 := select(cpf,i-> matrix{i-shift}*basVtrans==0);
    verbose2("#points in subspace of exponent-points: " | #cpf2, o);

    -- Find points within the polytope:
    lexponents := select(cpf2, i->
          max flatten entries (dualpolytope * ((T * transpose matrix {i-shift})||1)) <=0)/2;
    isInteger := l -> denominator l == 1;
    assert all(flatten lexponents, isInteger );
    lexponents = apply(lexponents, i -> numerator \ i);
    lmSOS := for i in lexponents list R_i;
    verbose2("#points inside Newton polytope: " | #lmSOS, o);

    if #lmSOS==0 then return;
    matrix transpose {lmSOS}
    )

-- Choose monomials, given a degree bound
chooseMons(Matrix,ZZ) := o -> (F,D) -> (
    if D<=0 or odd D then error "Expected even positive integer";
    R := ring F;
    var := support F;
    if isQuotientRing R then(
        var' := flatten for x in support ideal R list support sub(x,R);
        var = unique(var | var');
        );
    if #var==0 then return matrix(R,{{1}});
    mon := if isHomogeneous R and isHomogeneous F then
        basis(D//2,R,Variables=>var)
        else basis(0,D//2,R,Variables=>var);
    verbose2("#monomials: " | numColumns mon, o);
    transpose mon
    )

pointsInBox = (mindeg,maxdeg,mindegs,maxdegs) -> (
    -- integer vectors within specified bounds
    n := #mindegs;
    -- Get candidate points
    local x; x= symbol x;
    R0 := QQ(monoid[x_0..x_(n-1)]);
    mon := flatten entries basis(mindeg,maxdeg,R0);
    e := apply (mon, i -> flatten exponents i);
    -- Only those within the box of degrees[mindegs:maxdegs]:
    select(e,i-> all(i-mindegs,j->j>=0) and all(maxdegs-i,j->j>=0))
    )

--###################################
-- Rational Rounding
--###################################

roundSolution = {RoundTol=>3,Verbosity=>1} >> o -> (pVec0,Q,A,B,b) -> (
    -- round and project --
    d := o.RoundTol;
    np := numColumns B;
    pVec := null;

    verbose1("Start rational rounding", o);
    while (d < MaxRoundTol) do (
        verbose2("rounding step #" | d, o);
        if np!=0 then (
            pVec = roundQQ(d,pVec0);
            bPar := b - B*pVec;
            )
        else bPar= b;

        (ok,Qp) := roundPSDmatrix(Q,A,bPar,d);
        if ok then break else d = d + 1;
        );
    (ok,Qp,pVec)
    )

--###################################
-- SOS in IDEAL
--###################################

makeMultiples = (h, D, homog) -> (
    -- h is a list of polynomials
    -- multiplies each hi with monomials up to degree D
    if #h==0 then return ({},{});
    R := ring h#0;
    -- compute monomials
    mon := for i to #h-1 list (
        di := D - first degree h#i;
        b := if homog then basis(di,R) else basis(0,di,R);
        flatten entries b
        );
    H := for i to #h-1 list h#i * mon#i;
    (flatten H, mon)
    )

sosInIdeal = method(
     Options => {RoundTol => 3, Solver=>"CSDP", Verbosity => 0} )
sosInIdeal (Ring, ZZ) := o -> (R,D) -> (
    -- find sos polynomial in a quotient ring
    if odd D then error "D must be even";
    mon := chooseMons(matrix{{0_R}}, D);
    sol := solveSOS (0_R, mon, o);
    (mon',Q,X,tval,sdpstatus) := readSdpResult sol;
    if Q===null or isZero(MedPrecision,Q) then (
        verbose1("no sos polynomial in degree "|D, o);
        return sdpResult(mon,,X,,sdpstatus);
        );
    sol
    )
sosInIdeal (Matrix,ZZ) := o -> (h,D) -> (
    -- h is a row vector of polynomials
    -- D is a degree bound
    -- returns sos polynomial in <h>

    -- The output is an SDPResult and the multipliers that
    -- express the SOS in terms of the generators.
    -- We have them independent of Gröbner bases.

    if numRows h > 1 then error "h must be a row vector";
    if odd D then error "D must be even";
    homog := isHomogeneous h;
    (H,m) := makeMultiples(first entries h, D, homog);
    F := matrix transpose {{0}|H};
    sol := rawSolveSOS (F, o);
    (mon,Q,X,tval,sdpstatus) := readSdpResult sol;
    if Q===null or isZero(MedPrecision,Q) then (
        verbose1("no sos polynomial in degree "|D, o);
        return (sdpResult(mon,,X,,sdpstatus),null);
        );
    tval = flatten entries tval;
    mult := getMultipliers(m,tval,ring mon);
    (sol,mult)
    )

getMultipliers = (mon,tval,S) -> (
    if #mon==0 then return zeros(S,0,1);
    mon = applyTable(mon, i -> sub(i,S));
    k := -1;
    mult := matrix(S, for m in mon list
        {sum for i in m list( k=k+1; i*tval#k)} );
    mult
    )

sosdecTernary = method(
     Options => {RoundTol => 3, Solver=>"CSDP", Verbosity => 0} )
sosdecTernary(RingElement) := o -> (f) -> (
    -- Implements Hilbert's algorithm to write a non-negative ternary
    -- form as sos of rational functions.
    -- Returns two lists of SOSPolys, the numerator and the denomenator polys
    if numgens ring f =!= 3 then error "polynomial must involve 3 variables";
    if not isHomogeneous f then error "polynomial must be homogeneous";
    fi := f;
    S := {};
    di := first degree fi;
    while di > 4 do(
        (sol,mult) := sosInIdeal(matrix{{fi}},2*di-4,o);
        Si := sosPoly sol;
        if Si===null then return (,);
        fi = mult_(0,0);
        if fi==0 then return (,);
        di = first degree fi;
        S = append(S,Si);
        );
    (mon,Q,X,tval,sdpstatus) := readSdpResult rawSolveSOS matrix{{fi}};
    if Q===null or isZero(MedPrecision,Q) then return (,);
    Si = sosPoly(mon,Q);
    if Si===null then return (,);
    S = append(S,Si);
    nums := for i to #S-1 list if odd i then continue else S#i;
    dens := for i to #S-1 list if even i then continue else S#i;
    (nums, dens)
    )

--###################################
-- SOS OPTIMIZATION
--###################################

recoverSolution = method()
recoverSolution(Matrix,Matrix) := (mon,X) -> (
    if X===null then return {};
    I := positions(flatten entries mon, y -> sum degree y==1);
    if #I==0 then(
        print "The monomial vector does not contain variables";
        return {};
        );
    var := mon^I;
    Y := submatrix(X,I,I);
    (e,V) := eigenvectors(Y,Hermitian=>true);
    if e#(-1)<=0 or e#0/e#(-1) < -HighPrecision then
        error "Moment matrix is not positive semidefinite";
    if #I>=2 and e#(-2) > LowPrecision then
        print "Moment matrix is not rank one, solution might not be correct.";
    vv := sqrt(e#(-1)) * V_{#I-1};
    sol := for j to #I-1 list ( var_(j,0) => vv_(j,0) );
    if #I < numRows X then(
        x0 := sub(mon,sol);  X0 := x0 * transpose x0;
        sol1 := for j to #I-1 list ( var_(j,0) => -vv_(j,0) );
        x1 := sub(mon,sol1);  X1 := x1 * transpose x1;
        if norm(X-X1) < norm(X-X0) then sol = sol1;
        );
    sol
    )
recoverSolution(SDPResult) := sol -> recoverSolution(sol#Monomials,sol#MomentMatrix)

-- Unconstrained minimization
-- sos lower bound for the polynomial f
lowerBound = method(
     Options => {RoundTol => 3, Solver=>null, Verbosity => 0} )
lowerBound(RingElement) := o -> (f) -> lowerBound(f,-1,o)
lowerBound(RingElement,ZZ) := o -> (f,D) -> drop(lowerBound(f,zeros(ring f,1,0),D,o),-1)

-- Minimize a polynomial on an algebraic set
lowerBound(RingElement,Matrix,ZZ) := o -> (f,h,D) -> (
    numdens := (f) -> (
        R := ring f;
        (num,den) := (f, 1_R);
        if isField R then(
            (num,den) = (numerator f, denominator f);
            R = last R.baseRings;
            );
        return (R,num,den)
        );
    checkInputs := (D,num,den,h,R) -> (
        if numRows h > 1 then error "h must be a row vector";
        if D<0 then(
            if numColumns h>0 or isQuotientRing R then
                error "Degree bound must be provided"
        )else(
            if odd D then error "degree bound must be even";
            );
        );

    (R,num,den) := numdens(f);
    checkInputs(D,num,den,h,R);
    -- prepare input
    (H,m) := makeMultiples(flatten entries h, D, false);
    F := matrix transpose {{num,-den}|H};
    objP := matrix{{-1}} || zeros(ZZ,#H,1);

    -- call solveSOS
    o' := new OptionTable from
        {RoundTol=>o.RoundTol, Solver=>o.Solver, Verbosity=>o.Verbosity};
    mon := if D>=0 then chooseMons(F,D,Verbosity=>o.Verbosity)
        else chooseMons (F,Verbosity=>o.Verbosity);
    if mon===null then return (,sdpResult(mon,,,,),);
    sol := rawSolveSOS(F,objP,mon,o');
    (mon',Q,X,tval,sdpstatus) := readSdpResult sol;
    (bound,mult) := (,);
    if tval=!=null then(
        tval = flatten entries tval;
        bound = tval#0;
        mult = getMultipliers(m,drop(tval,1),ring mon');
        );
    (bound,sol,mult)
    )


--###################################
-- Methods for testing
--###################################

checkSolver = method( Options => {Verbosity => 0} )
checkSolver(String,String) := o -> (solver,fun) -> (
    checkMethod := hashTable {
        "optimize" => checkOptimize,
        "solveSOS" => checkSolveSOS,
        "sosdecTernary" => checkSosdecTernary,
        "sosInIdeal" => checkSosInIdeal,
        "lowerBound" => checkLowerBound
        };
    if checkMethod#?fun then
        return checkMethod#fun(solver);
    if fun != "AllMethods" then
        error "No test implemented for this function";
    T := for f in keys checkMethod list(
        print "################################";
        print("checking method "|f);
        print "--------------------------------";
        t := checkMethod#f(solver,o);
        informAboutTests t;
        {f, testsString t}
        );
    print "################################";
    print("SUMMARY");
    print "--------------------------------";
    print netList(T,Boxes=>false);
    )
checkSolver(String,Function) := o -> (solver,fun) -> checkSolver(solver,toString fun,o)
checkSolver(String) := o -> (solver) -> checkSolver(solver,"AllMethods",o)

-- A method to inform about the results of the tests in one function
testsString = t -> concatenate apply(t, i -> if i then " ✓ " else " ✘ ")
informAboutTests = t -> (
    print("Results: " | testsString t);
    )


--checkSolveSOS
checkSolveSOS = method( Options => {Verbosity => 0} )
checkSolveSOS(String) := o -> (solver) -> (
    local x; x= symbol x;
    local y; y= symbol y;
    local z; z= symbol z;
    local w; w= symbol w;
    local t; t= symbol t;
    isGram := (f,mon,Q) -> (
        if Q===null then return false;
        e := eigenvalues(Q,Hermitian=>true);
        tol := MedPrecision;
        if min e < -tol then return false;
        S := ring mon;
        return isZero(tol, sub(f,S) - transpose(mon)*Q*mon);
        );
    isGramParam := (f,mon,Q,tval) ->
        if tval===null then false else isGram(sub(f,t=>tval_(0,0)),mon,Q);

    ---------------GOOD CASES1---------------
    t0:= (
        R := QQ[x,y];
        f := 4*x^4+y^4;
        (mon,Q,X,tval,sdpstatus) := readSdpResult solveSOS(f,Solver=>solver,Verbosity=>o.Verbosity);
        isGram(f,mon,Q)
        );

    t1:= (
        f = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
        (mon,Q,X,tval,sdpstatus) = readSdpResult solveSOS(f,Solver=>solver,Verbosity=>o.Verbosity);
        isGram(f,mon,Q)
        );

    t2:= (
        R = QQ[x,y,z];
        f = x^4+y^4+z^4-4*x*y*z+x+y+z+3;
        (mon,Q,X,tval,sdpstatus) = readSdpResult solveSOS(f,Solver=>solver,Verbosity=>o.Verbosity);
        isGram(f,mon,Q)
        );

    t3:= (
        R = QQ[x,y,z,w];
        f = 2*x^4 + x^2*y^2 + y^4 - 4*x^2*z - 4*x*y*z - 2*y^2*w + y^2 - 2*y*z + 8*z^2 - 2*z*w + 2*w^2;
        (mon,Q,X,tval,sdpstatus) = readSdpResult solveSOS(f,Solver=>solver,Verbosity=>o.Verbosity);
        isGram(f,mon,Q)
        );

    ---------------PARAMETRIC1---------------
    t4:= (
        R = QQ[x][t];
        f = (t-1)*x^4+1/2*t*x+1;
        (mon,Q,X,tval,sdpstatus) = readSdpResult solveSOS (f,Solver=>solver,Verbosity=>o.Verbosity);
        isGramParam(f,mon,Q,tval)
        );

    ---------------QUOTIENT1---------------
    t5:= (
        R = QQ[x,y];
        S := R/ideal(x^2 + y^2 - 1);
        f = sub(10-x^2-y,S);
        (mon,Q,X,tval,sdpstatus) = readSdpResult solveSOS (f, 2, TraceObj=>true, Solver=>solver,Verbosity=>o.Verbosity);
        isGram(f,mon,Q) and rank Q == 2
        );

    ---------------BAD CASES1---------------
    t6:= (
        R = QQ[x,y][t];
        f = x^4*y^2 + x^2*y^4 - 3*x^2*y^2 + 1; --Motzkin
        (mon,Q,X,tval,sdpstatus) = readSdpResult solveSOS(f,Solver=>solver,Verbosity=>o.Verbosity);
        ( Q === null )
        );

    t7:= (
        (mon,Q,X,tval,sdpstatus) = readSdpResult solveSOS(f-t,-t, Solver=>solver,Verbosity=>o.Verbosity);
        ( Q === null )
        );

    {t0,t1,t2,t3,t4,t5,t6,t7}
    )

-- check sosdecTernary
checkSosdecTernary = method( Options => {Verbosity => 0} )
checkSosdecTernary(String) := o -> (solver) -> (
    local x; x= symbol x;
    local y; y= symbol y;
    local z; z= symbol z;

    cmp := (f,p,q) -> (
        if p===null then return false;
        d := product(value\p) - f*product(value\q);
        isZero(LowPrecision, d)
        );

    t0:= (
        R:= QQ[x,y,z];
        f := x^6 + y^6 +z^6;
        (p,q) := sosdecTernary (f, Solver=>solver,Verbosity=>o.Verbosity);
        cmp(f,p,q)
        );

    t1:= (
        R = QQ[x,y,z];
        f = x^4*y^2 + x^2*y^4 + z^6 - 4*x^2 *y^2 * z^2;
        (p,q) = sosdecTernary (f, Solver=>solver,Verbosity=>o.Verbosity);
        (p===null)
        );

    t2:= (
        R = RR[x,y,z];
        f = x^4*y^2 + x^2*y^4 + z^6 - 3*x^2 *y^2 * z^2; --Motzkin
        (p,q) = sosdecTernary (f, Solver=>solver,Verbosity=>o.Verbosity);
        cmp(f,p,q)
        );

    {t0,t1,t2}
    )


-- check sosInIdeal
checkSosInIdeal = method( Options => {Verbosity => 0} )
checkSosInIdeal(String) := o -> (solver) -> (
    local x; x= symbol x;
    local y; y= symbol y;
    local z; z= symbol z;
    local sol; local s; local mult;
    cmp := (h,s,mult) -> (
        if s===null then return false;
        h = sub(h,ring s);
        d := (h*mult)_(0,0) - value s;
        isZero(MedPrecision, d)
        );

    t0:= (
        R:= QQ[x];
        h:= matrix {{x+1}};
        (sol,mult) = sosInIdeal (h,2, Solver=>solver,Verbosity=>o.Verbosity);
        s = sosPoly sol;
        cmp(h,s,mult)
        );

    t1:= ( --similar to test0
        R= RR[x];
        h= matrix {{x+1}};
        (sol,mult) = sosInIdeal (h,4, Solver=>solver,Verbosity=>o.Verbosity);
        s = sosPoly sol;
        cmp(h,s,mult)
        );

    t2:= (
        R = RR[x,y,z];
        h = matrix {{x-y, x+z}};
        (sol,mult) = sosInIdeal (h,2, Solver=>solver,Verbosity=>o.Verbosity);
        s = sosPoly sol;
        cmp(h,s,mult)
        );

    t3:= ( --similar to test2
        R = RR[x,y,z];
        h = matrix {{x-y, x+z}};
        (sol,mult) = sosInIdeal (h,6, Solver=>solver,Verbosity=>o.Verbosity);
        s = sosPoly sol;
        cmp(h,s,mult)
        );

    t4:= (
        R = QQ[x,y];
        h = matrix {{y^2+y, x*y, -x^2*y-x^2-y-1}};
        (sol,mult) = sosInIdeal(h,2, Solver=>solver,Verbosity=>o.Verbosity);
        (sol#GramMatrix===null)
        );

    -----------------QUOTIENT1-----------------
    t5:= (
        R = QQ[x,y,z]/ideal {x^2+y^2+y, y-z^2};
        s = sosPoly sosInIdeal (R,2,Solver=>solver,Verbosity=>o.Verbosity);
        (s=!=null and ideal gens s == ideal(x_R,y,z))
        );

    t6:= ( --similar to test4
        R = QQ[x,y]/ideal {y^2+y, x*y, -x^2*y-x^2-y-1};
        s = sosPoly sosInIdeal(R,2, Solver=>solver,Verbosity=>o.Verbosity);
        (s=!=null and ideal gens s == ideal(x_R,y+1))
        );

    {t0,t1,t2,t3,t4,t5,t6}
    )


-- check lowerBound
checkLowerBound = method( Options => {Verbosity => 0} )
checkLowerBound(String) := o -> (solver) -> (
    tol := LowPrecision;
    local x; x= symbol x;
    local y; y= symbol y;
    local z; z= symbol z;
    local mult;
    equal := (a,b) -> (
        if a===null then return false;
        d := if abs(b)<1 then abs(a-b) else abs(a-b)/abs(b);
        d < tol
        );
    cmp := (f,h,bound,mon,Q,mult) -> (
        if Q===null then return false;
        d := f - bound + (h*mult - transpose mon * Q * mon)_(0,0);
        isZero(MedPrecision, d)
        );

    --------------UNCONSTRAINED1--------------
    t0:= (
        R := QQ[x];
        f := (x-1)^2 + (x+3)^2;
        (bound,sol) := lowerBound(f, Solver=>solver,Verbosity=>o.Verbosity);
        equal(bound,8)
        );

    t1:= (
        R = RR[x,y];
        f = (x-exp(1)*y)^2 + x^2 + (y-4)^2;
        (bound,sol) = lowerBound(f, Solver=>solver,Verbosity=>o.Verbosity);
        equal(bound,16*exp(2)/(2+exp(2)))
        );

    t2:= (
        R = QQ[x,z];
        f = x^4+x^2+z^6-3*x^2*z^2;
        (bound,sol) = lowerBound (f,Solver=>solver,Verbosity=>o.Verbosity,RoundTol=>infinity);
        equal(bound,-.17798)
        );

    t3:= ( --rational function
        R = QQ[x];
        f = (x^2-x)/(x^2+1);
        (bound,sol) = lowerBound(f, Solver=>solver,Verbosity=>o.Verbosity, RoundTol=>infinity);
        equal(bound,1/2-1/sqrt(2))
        );

    ---------------CONSTRAINED1---------------
    t4:= (
        R = RR[x,y];
        f = y;
        h := matrix {{y-pi*x^2}};
        (bound,sol,mult) = lowerBound (f, h, 2, Solver=>solver,Verbosity=>o.Verbosity);
        (mon,Q,X,tval,sdpstatus) := readSdpResult sol;
        equal(bound,0) and cmp(f,h,bound,mon,Q,mult)
        );

    t5:= (
        R = QQ[x,y,z];
        f = z;
        h = matrix {{x^2 + y^2 + z^2 - 1}};
        (bound,sol,mult) = lowerBound (f, h, 4, Solver=>solver,Verbosity=>o.Verbosity);
        (mon,Q,X,tval,sdpstatus) = readSdpResult sol;
        equal(bound,-1) and cmp(f,h,bound,mon,Q,mult)
        );

    -----------------QUOTIENT1-----------------
    t6:= (
        R = QQ[x,y];
        I := ideal (x^2 - x);
        S := R/I;
        f = sub(x-y,S);
        h = matrix {{sub(y^2 - y,S)}};
        (bound,sol,mult) = lowerBound(f, h, 2, Solver=>solver,Verbosity=>o.Verbosity);
        (mon,Q,X,tval,sdpstatus) = readSdpResult sol;
        equal(bound,-1) and cmp(f,h,bound,mon,Q,mult)
        );

    {t0,t1,t2,t3,t4,t5,t6}
    )

--##########################################################################--
-- Documentation and Tests
--##########################################################################--

beginDocumentation()

load "./SumsOfSquares/SOSdoc.m2"

--0
TEST /// --sosPoly and value
    R = QQ[x,y,z]
    coeff1={3,1,1,1/4,1}
    pol1={-(1/2)*x^3*y-(1/2)*x*y^3+x*y*z^2, x^2*y^2-z^4, x^2*y*z-y*z^3,
      -x^3*y+x*y^3, x*y^2*z-x*z^3}
    p1=sosPoly(R,pol1,coeff1)
    p2=x^6*y^2 + 2*x^4*y^4 + x^2*y^6 - 2*x^4*y^2*z^2 - 2*x^2*y^4*z^2 -
    3*x^2*y^2*z^4 + x^2*z^6 + y^2*z^6 + z^8
    assert(value(p1)===p2)
///

--1
TEST /// --SOSmult
    debug needsPackage "SumsOfSquares"
    R = QQ[x,y,z,w]
    p1=sosPoly(R,{x^2-x*y,y^2+1,x},{1,2,3})
    p2=sosPoly(R,{y^3,x*w*z,y*z^2},{3,1/2,1/4})
    assert(value(p1*p2)==value(p1)*value(p2))
    assert(value(p1^4)==value(p1)^4)

    equal = (f1,f2) -> norm(f1-f2) < HighPrecision;
    R = RR[x,y,z,w]
    p1=sosPoly(R,{x^2-x*y,y^2+1,x},{1.32,1.47,12./7})
    p2=sosPoly(R,{y^3,x*w*z,y*z^2},{3.1,1.31,2.0})
    assert( equal(value(p1*p2),value(p1)*value(p2)) )
    assert( equal(value(p1^4),value(p1)^4) )
///

--2
TEST /// --cleanSOS
    R = RR[x,y];
    s = sosPoly(R, {x^2+.0001*x+1,y}, {2,.0001})
    t1 = clean( .001, s )
    t2 = sosPoly(R, {x^2+1}, {2})
    assert (t1 == t2)

    R = QQ[x,y];
    s = sosPoly(R, {x+1,y}, {2,1/100000})
    t = clean( 0.001, s )
    assert (t == s)
///

--3
TEST ///--substitute SOSPoly
    R = QQ[x,y];
    s = sosPoly(R, {x+1,y}, {2,3})
    S = QQ[x,y,z]
    t1 = sosPoly(S, {x+1,y}, {2,3})
    t2 = sub (s, S)
    assert (t1 == t2)
///

--4
TEST ///--toRing
    debug needsPackage "SumsOfSquares"
    R = QQ[x,y];
    s = sosPoly(R, {x+1,y}, {2,3});
    S = RR[x,y];
    s2 = toRing_S s;
    assert instance(coefficientRing ring s2, RealField)
    s3 = toRing_R s2;
    assert (s==s3)

    tol := HighPrecision;
    f = 0.1*x_S^2 + y^2
    g = 1/10*(symbol x)_R^2 + (symbol y)_R^2
    -- comparison in rationals is complicated:
    resid = sum \\ abs \ (x -> lift (x,QQ)) \ flatten entries last coefficients (toRing_R f - g)
    assert (resid < tol)
    -- comparison in reals:
    assert (norm (toRing_S g - f) < tol)
///

--5
TEST /// --sosdec
    R=QQ[x,y,z]
    Q=matrix{{1,-1/2,1},{-1/2,1,-1/2},{1,-1/2,1}}
    Q=promote(Q,QQ)
    mon=matrix{{x^3},{x^2*z},{y*z^2}}
    f=sosPoly(mon,Q)
    assert(f=!=null and value f==transpose mon * Q *mon)
///

--6
TEST /// --chooseMons
    debug needsPackage "SumsOfSquares"
    R = QQ[x,y];
    f = x^4+2*x*y-x+y^4
    lmsos = chooseMons(f)
    assert( lmsos === null )

    R = QQ[x,y]
    f = (x+2*y)^2 + (x-y)^4
    lmsos = chooseMons f
    assert( lmsos=!=null and numRows lmsos == 5 )

    R = QQ[x,y][t];
    f = x^4+2*x*y-x+y^4
    lmsos = chooseMons(f-t)
    assert( lmsos=!=null and ring lmsos===R and numRows lmsos == 6 )

    R = RR[x,y][t];
    f = x^4+2*x*y-x+y^4
    lmsos = chooseMons(f-t)
    assert( lmsos=!=null and ring lmsos===R and numRows lmsos == 6 )

    R = QQ[x,y][l,t];
    f = y + l *(y-x^2) - t
    mon = chooseMons f
    assert(mon == matrix{{1_R},{x_R}})

    R = QQ[x,y,z]
    F = matrix{{x+1},{x*z^2-1}}
    mon = chooseMons(F,2)
    assert( mon - matrix(R,{{1},{x},{z}}) == 0 )

    S = R/ideal(F)
    F = matrix{{0_S}}
    mon = chooseMons(F,2)
    assert( mon - matrix(S,{{1},{z}}) == 0 )

    R = QQ[x0,x1,x2,x3,MonomialOrder=>Lex]
    S = R/ideal(x1-x2^2,x3-1)
    F = matrix{{1_S}}
    mon = chooseMons(F,4)
    assert( mon - matrix(S,{{1},{x2},{x2^2}}) == 0 )
///

--7
TEST /// --createSOSModel
    debug needsPackage "SumsOfSquares"
    eval = (Q,v) -> (transpose v * Q * v)_(0,0)

    R = QQ[x][t];
    f = x^4 - 2*x + t;
    mon = matrix{{1},{x},{x^2}}
    (C,Ai,p0,V,A,B,b) = createSOSModel(f,mon)
    assert( eval(C,mon) == x^4 - 2*x )
    assert( #Ai==2 and all({0,1}, j -> eval(Ai#j,mon) == V_(0,j)) )

    equal = (f1,f2) -> norm(f1-f2) < HighPrecision;
    R = RR[x][t];
    f = x^4 - 2*x + t;
    mon = matrix{{1},{x},{x^2}}
    (C,Ai,p0,V,A,B,b) = createSOSModel(f,mon)
    assert( equal(eval(C,mon), x^4 - 2*x) )
    assert( #Ai==2 and all({0,1}, j -> equal(eval(Ai#j,mon), V_(0,j))) )

    R = QQ[x,y][t]
    f = x^2+2*x + t*(y+1)
    mon = matrix{{1},{x}}
    (C,Ai,p0,V,A,B,b) = createSOSModel(f,mon) --infeasible
    assert(entries C=={{0,1},{1,1}} and #Ai==0 and p0==0)
///

--8
TEST ///--makeMultiples
    debug needsPackage "SumsOfSquares"
    R = QQ[x,y,z]
    f1 = x + y
    f2 = x^2 + z^2
    h = {f1,f2}
    (H,m) = makeMultiples (h,3, false)
    assert (#H == 14 )
    assert( max(first\degree\H) == 3 )

    (H,m) = makeMultiples (h,3, true)
    assert(#H == 9)
    assert( unique(first\degree\H) == {3} )
///

--9
TEST ///--recoverSolution
    debug needsPackage "SumsOfSquares"
    equal = (sol,xx,vv) ->
        all(xx, vv, (x,v) -> norm(v-sub(x,sol)) < HighPrecision);
    R = RR[x,y];
    mon = matrix {{1},{x},{y}};
    X = matrix(RR, {{1,0,1},{0,0,0},{1,0,1}} );
    sol = recoverSolution(mon,X);
    assert(equal(sol,{x,y},{0,1}))

    X = matrix(RR, {{1,0,-1},{0,0,0},{-1,0,1}} );
    sol = recoverSolution(mon,X);
    assert(equal(sol,{x,y},{0,-1}))

    mon = matrix {{1},{x}};
    X = matrix(RR, {{1,1},{1,1}} );
    sol = recoverSolution(mon,X);
    assert(equal(sol,{x},{1}))

    mon = matrix {{1},{y}};
    X = matrix(RR, {{1,-1},{-1,1}} );
    sol = recoverSolution(mon,X);
    assert(equal(sol,{y},{-1}))

    mon = matrix {{x},{y}};
    s = sqrt 2;
    X = matrix(RR, {{2,-s},{-s,1}} );
    sol = recoverSolution(mon,X);
    assert(equal(sol,{x,y},{s,-1}) or equal(sol,{x,y},{-s,1}))

    mon = matrix {{1},{y},{x*y}};
    X = matrix(RR, {{1,-1,-1},{-1,1,1},{-1,1,1}} );
    sol = recoverSolution(mon,X);
    assert(equal(sol,{y},{-1}))
///

--10
TEST /// --solveSOS
    debug needsPackage "SumsOfSquares"
    results := checkSolveSOS("CSDP")
    assert all(results,t->t=!=false);
///

--11
TEST /// --lowerBound
    debug needsPackage "SumsOfSquares"
    results := checkLowerBound("CSDP")
    assert all(results,t->t=!=false);
///

--12
TEST /// --sosInIdeal
    debug needsPackage "SumsOfSquares"
    results := checkSosInIdeal("CSDP")
    assert all(results,t->t=!=false);
///

--13
TEST /// --sosdecTernary
    debug needsPackage "SumsOfSquares"
    results := checkSosdecTernary("CSDP")
    assert all(results,t->t=!=false);
///

