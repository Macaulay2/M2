newPackage(
    "SumsOfSquares",
    Version => "3.0",
    Date => "November 2026",
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
      HomePage => "https://scholar.google.com/citations?user=cFOV7nYAAAAJ&hl=de"}
    },
    Headline => "sums of squares",
    Keywords => {"Real Algebraic Geometry"},
    AuxiliaryFiles => true,
    PackageImports => {"FourierMotzkin"},
    PackageExports => {"SemidefiniteProgramming"},
    Certification => {
        "journal name" => "The Journal of Software for Algebra and Geometry",
        "journal URI" => "https://msp.org/jsag/",
        "article title" => "Sums of squares in Macaulay2",
        "acceptance date" => "6 January 2020",
        "published article URI" => "https://msp.org/jsag/2020/10-1/p03.xhtml",
        "published article DOI" => "10.2140/jsag.2020.10.17",
        "published code URI" => "https://msp.org/jsag/2020/10-1/jsag-v10-n1-x03-SumsOfSquares.zip",
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
MaxRoundTol = 52 --maximum rounding tolerance (the precision of the solvers)
LoopRoundTol = 32 --the rounding loop tries the precisions RoundTol..LoopRoundTol
HighPrecision = 1e-10 --e.g. for numerical linear algebra
MedPrecision = 1e-6 --e.g. for SDP solutions
LowPrecision = 1e-4

protect NormalizeTrace -- an internal option of rawSolveSOS (sosInIdeal)
protect Multiples -- an internal option of rawSolveSOS (lowerBound with equations)
protect ReduceMonomials -- an internal option of rawSolveSOS (the retry without reductions)
protect KernelForm -- an internal option of rawSolveSOS (the SDP in kernel form, LOG.md 6)
-- the SDP in kernel form (LOG.md 6.5, 6.6): false, true (every exact
-- problem; for tests), or "auto" (feasibility problems where it is smaller)
kernelFormDefault = "auto"

--##########################################################################--
-- TYPES
--##########################################################################--

SOSPoly = new Type of HashTable

sosPoly = method()
sosPoly (Ring, List, List) := (R, polys, coeffs) -> (
    if #polys != #coeffs then
        error "sosPoly: expected as many weights as polynomials";
    if any(coeffs, c -> instance(c, Number) and c < 0) then
        error "sosPoly: expected nonnegative weights";
    new SOSPoly from {
        ring => R,
        gens => polys,
        coefficients => coeffs
        }
    )
sosPoly (List, List) := (polys, coeffs) -> (
    if #polys == 0 then
        error "sosPoly: the ring of an empty list is unknown; use sosPoly(R, {}, {})";
    sosPoly(ring polys#0,polys,coeffs)
    )

ring SOSPoly := S -> S#ring

gens SOSPoly := o -> S -> S#gens

coefficients SOSPoly := o -> S -> S#coefficients

length SOSPoly := S -> #(S#gens)

substitute (SOSPoly,Ring) := (S,R) ->
    sosPoly(R, for g in S#gens list sub(g,R), S#coefficients)

expression SOSPoly := S ->
    new Sum from apply (S#coefficients,S#generators,(c,g) -> hold c * (hold g)^2)

net SOSPoly := S -> (
    if #gens S == 0 then "0"
    else net expression S
    )

Number * SOSPoly := (a,S) -> (
    if a<0 then error "scalar must be nonnegative";
    if a==0 then return sosPoly(ring S, {}, {});
    -- a real scalar must live in the coefficient field, as for polynomials
    kk := coefficientRing ring S;
    if not instance(a,ZZ) and not instance(a,QQ) then
        a = try promote(a, kk) else
            error("scalar cannot be promoted to the coefficient ring " | toString kk);
    sosPoly(ring S, gens S, a * coefficients S)
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
    -- s^(2k) = ((value s)^k)^2 and s^(2k+1) = s * ((value s)^k)^2
    if D<0 then error "power should be a nonnegative integer.";
    R := ring p1;
    if D==0 then return sosPoly(R,{1_R},{1});
    if D==1 then return p1;
    p2 := sosPoly(R,{(value p1)^(D//2)},{1});
    if even D then p2 else p1 * p2
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

value SOSPoly := a -> (
    if #(a#gens)==0 then return 0_(a#ring);
    sum for i to #(a#gens)-1 list a#gens_i^2 * a#coefficients_i
    )

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
    -- no equations (solve of a matrix without rows crashes Macaulay2 1.26.06)
    if numRows A==0 then return map((ring A)^(numColumns A),(ring A)^1,0);
    if isExactField A then return try solve(A,b);
    -- the tolerance is relative to the size of the data
    tol := HighPrecision * max(1, norm b);
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
    -- singular values below HighPrecision, relative to the largest one
    tol := HighPrecision * max(1, norm A);
    (S,U,Vt) := truncatedSVD(A,-tol);
    transpose Vt
    )

zeros = (kk,m,n) -> map(kk^m,kk^n,{})

--###################################
-- solveSOS
--###################################

sosPoly(Matrix,Matrix) := (mon,Q) -> (
    if mon===null or Q===null then return;
    -- the dependency tests a real Q for PSD with an absolute tolerance, so
    -- Q is normalised first
    sc := if isExactField Q or Q==0 then 1 else max(1, max(abs \ flatten entries Q));
    (L,D,P) := PSDdecomposition((1/sc) * Q);
    if L=!=null then D = sc * D;
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
     Options => {RoundTol => 3, Solver=>null, Verbosity => 0, TraceObj => false,
         NormalizeTrace => false, Multiples => {}, ReduceMonomials => true, KernelForm => null} )

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
    if o.TraceObj and objP != 0 then
        print "Warning: TraceObj => true replaces the objective function of the parameters.";

    np := numRows objP;
    -- the zero polynomial: the empty sum of squares, with Gram matrix 0
    -- (parameters, if any, are free; the value 0 is returned)
    if F==0 and not o.NormalizeTrace then (
        if objP != 0 then (
            verbose1("The objective function is unbounded.", o);
            return sdpResult(mon,,,,"SDP solved, primal infeasible");
            );
        return sdpResult(mon, map(kk^(numRows mon),kk^(numRows mon),0), null,
            if np>0 then map(kk^np,kk^1,0), "SDP solved, primal-dual feasible");
        );

    F00 := F;
    -- scale a tiny problem (see scaleFactor) --
    s := scaleFactor F_(0,0);
    if s != 1 then (
        verbose2("Scaling the problem by " | toString s, o);
        F = matrix{{s * F_(0,0)}} || F^(toList(1..np));
        );
    -- a solution (Q,p) of the scaled problem is s times a solution of F
    unscale := M -> if M===null or s==1 then M else (1/s) * M;
    noPars := pVec -> if np==0 then null else unscale pVec;

    -- reduce the monomial vector: absorbed multiples (lowerBound with
    -- equations, see absorbMultiples), then forced zeros (see forcedZeros).
    -- The Gram matrix of the reduced problem is padded with zeros; the moment
    -- matrix X is mapped back by v = M w (zeros for the forced zeros).
    n := numRows mon;
    (A0,B0,b0) := kernelModel(F,mon);
    -- (not in quotient rings over RR, where the exact zero tests would read
    -- floating point noise)
    reduce := o.ReduceMonomials and (isExactField kk or not isQuotientRing ring F);
    (keep,M) := if reduce then absorbMultiples(mon,o.Multiples) else (toList(0..n-1),null);
    (A1,B1,b1) := subModel(A0,B0,b0,n,keep);
    keep' := if reduce then forcedZeros(A1,B1,b1,#keep) else toList(0..#keep-1);
    if keep'===null then (
        -- decided without the solver; the status is the one the solvers give
        -- for an infeasible Gram problem (the dual in SemidefiniteProgramming)
        verbose1("No Gram matrix exists. Terminate.", o);
        return sdpResult(mon,,,,"SDP solved, dual infeasible");
        );
    (A2,B2,b2) := subModel(A1,B1,b1,#keep,keep');
    keep = keep_keep';
    if M===null then M = id_(QQ^n);
    M = M_keep';
    if #keep < n then verbose2("Monomials dropped: " | toString(n - #keep), o);
    padQ := Q -> if Q===null or #keep==n then Q else (
        pos := hashTable apply(#keep, k -> keep#k => k);
        map((ring Q)^n, (ring Q)^n, (i,j) -> if pos#?i and pos#?j then Q_(pos#i,pos#j) else 0));
    padX := X -> if X===null or #keep==n then X else (
        MR := sub(M, ring X); MR * X * transpose MR);
    result := (m,Q,X,p,st) -> sdpResult(m,padQ Q,padX X,p,st);

    -- build SOS model --
    -- (the image model as vectors; the matrices A_i are built only for the
    -- image form, LOG.md 6.8)
    (c,p0,V,U) := getImageBasis(A2,B2,b2,new OptionTable from {Verbosity=>o.Verbosity});
    if c===null then return sdpResult(mon,,,,);
    (A,B,b) := (A2,B2,b2);
    -- without rounding the model is the same, in floating point: the SDP
    -- sent to the solver does not depend on RoundTol (BUGS.md 30)
    if isExactField(A) and o.RoundTol==infinity then (
        (A,B,b,c,p0,V,U) = toSequence apply((A,B,b,c,p0,V,U), M -> sub(M,RR));
        );
    -- Parameters that do not change the polynomial (B p = 0) are free.
    -- An objective on them is unbounded.  A basis vector of the Gram part
    -- 0 (such a free direction) is dropped, since the solvers fail on it.
    if np>0 and objP!=0 and not o.TraceObj then (
        NB := kernelGens B;
        if numColumns NB>0 and not isZero(HighPrecision, transpose NB * objP) then (
            verbose1("The objective function is unbounded.", o);
            return sdpResult(mon,,,,"SDP solved, primal infeasible");
            );
        );
    nonzero := select(numColumns U, k -> not isZero(HighPrecision, U_{k}));
    if #nonzero < numColumns U then (V,U) = (V_nonzero, U_nonzero);
    C := first vecs2smats c;
    -- the trace of the A_i: the sum of the diagonal entries of smat2vec
    nr := (-1 + round sqrt(1 + 8*numRows U))//2;
    diagRow := map((ring U)^1, (ring U)^(numRows U), apply(nr, i -> (0, i*nr - (i*(i-1))//2) => 1));
    -- the image basis before the normalisation of the trace, for the rounding
    (C0,p00,V0,U0) := (C,p0,V,U);
    -- coordinates on it of the solution in the coordinates y of the SDP
    toBasis0 := y -> y;
    if o.NormalizeTrace then (
        -- a cone (f_0 = 0) posed with trace Q = 1, so that Q = 0 is
        -- excluded: Q = C + sum_i t_i A_i with t = t0 + N z, N a basis of
        -- the directions of trace 0
        tr := flatten entries(diagRow * U);
        if all(tr, t -> t==0) then return sdpResult(mon,,,,);
        k := maxPosition apply(tr, abs);
        t0 := (1 - trace C)/tr#k;
        C = C + t0 * first vecs2smats U_{k};
        p0 = p0 + t0 * V_{k};
        N := kernelGens matrix{tr};
        U = U * N;
        V = V * N;
        toBasis0 = y -> (
            e := map(QQ^(numRows N),QQ^1,(i,j) -> if i==k then t0 else 0);
            sub(e,ring y) + sub(N,ring y) * y);
        );

    nb := numColumns U;
    obj :=
        if o.TraceObj then transpose(diagRow * U)
        else (transpose V * objP);
    if obj==0 then verbose2( "Solving SOS feasibility problem...", o)
    else verbose2("Solving SOS optimization problem...", o);

    kform := if o.KernelForm === null then kernelFormDefault else o.KernelForm;
    -- "auto": feasibility problems whose kernel form has fewer constraints
    -- (the coefficient equations less the parameters, plus the trace) than
    -- the image form has basis matrices
    -- (not when f_0 = 0 without the trace normalisation: the kernel form then
    -- has the zero solution, which the image form avoids)
    useKernel := isExactField kk and (kform === true or (kform === "auto" and obj == 0
        and (F_(0,0) != 0 or o.NormalizeTrace)
        and numRows A - numColumns B + (if o.NormalizeTrace then 1 else 0) < nb));
    local X; local Q; local sdpstatus; local pVec0; local coords;
    -- (the elimination of the parameters on the exact model A2, B2, b2:
    -- with RoundTol => infinity A, B, b are floating point copies)
    if useKernel then
        (Q,X,pVec0,coords,sdpstatus) = kernelSolve(A2,B2,b2,objP,U0,V0,smat2vec C0,p00,o)
    else (
        Ai := vecs2smats U;
        (X',my,Q',st') := optimize(sdp(C,Ai,obj), Solver=>o.Solver, Verbosity=>o.Verbosity);
        (X,Q,sdpstatus) = (X',Q',st');
        if Q=!=null and not match("primal infeasible", sdpstatus) then (
            y := -my;
            pVec0 = p0 + V*y;
            -- (the coordinates are needed for the rounding only)
            if isExactField kk and o.RoundTol <= MaxRoundTol then
                coords = flatten entries toBasis0 y;
            );
        );
    -- CSDP can fail on the kernel form where the image form succeeds (the
    -- solution on the boundary of the primal cone): then in image form
    if useKernel and Q===null and not match("infeasible", sdpstatus) then (
        verbose2("The solver failed on the kernel form; solving the image form.", o);
        return rawSolveSOS(F00,objP,mon,o ++ {KernelForm => false});
        );
    -- the solver can fail on a reduced problem where it succeeds on the full
    -- one (the dropped monomials act as a regularisation, BUGS.md 15): then
    -- once more without the reductions
    if Q===null and #keep < n and not match("infeasible", sdpstatus) then (
        verbose2("The solver failed on the reduced problem; solving it without the reductions.", o);
        return rawSolveSOS(F00,objP,mon,o ++ {ReduceMonomials => false});
        );
    -- "primal infeasible": the objective is unbounded, Q is a ray, not a solution
    if Q===null or match("primal infeasible", sdpstatus) then
        return result(mon,,X,,sdpstatus);
    mx := 1;
    if o.NormalizeTrace then (
        -- rescale the solution of the cone: the largest parameter (or else
        -- the largest entry of Q) becomes 1, so that the rounding can find
        -- small denominators
        mx = if np>0 then max(abs \ flatten entries pVec0) else 0;
        if mx==0 then mx = max(abs \ flatten entries Q);
        (Q,pVec0) = ((1/mx) * Q, (1/mx) * pVec0);
        );

    if not isExactField kk then return result(mon,unscale Q,X,noPars pVec0,sdpstatus);
    if o.RoundTol > MaxRoundTol then(
        if o.RoundTol < infinity then
            print("Warning: RoundTol > " | MaxRoundTol | " exceeds the precision of the numerical solution. Rounding will be skipped.");
        return result(changeMatrixField(RR,mon),unscale Q,X,noPars pVec0,sdpstatus);
        );

    -- rational rounding, in the coordinates of the solution on the exact
    -- image basis of the kernel model (for a cone C0 = 0, and the
    -- coordinates are scaled by 1/mx as the solution)
    imageData := new HashTable from {
        "U" => U0, "V" => V0, "c" => smat2vec C0, "p0" => p00,
        "coords" => apply(coords, c -> c/mx),
        "cone" => o.NormalizeTrace};
    optim := np>0 and objP!=0 and not o.TraceObj;
    (ok,Qp,pVec) := roundSolution(imageData,pVec0,Q,A,B,b,if optim then objP,
        RoundTol=>o.RoundTol,Verbosity=>o.Verbosity);
    if ok then return result(mon,unscale Qp,X,noPars pVec,sdpstatus);
    verbose1("rounding failed, returning real solution", o);
    result(changeMatrixField(RR,mon),unscale Q,X,noPars pVec0,sdpstatus)
    )

-- The SDP in kernel form (LOG.md 6): the Gram matrix is the primal variable,
-- the constraints are the coefficient equations with the parameters
-- eliminated exactly (p_J = B1^-1 (b1 - A1 q) for independent columns J of B
-- and rows R1 with B1 invertible; dependent parameters are 0), and for a cone
-- trace Q = 1.  The objective objP^T p is linear in q.  Returns the Gram
-- matrix, the dual slack as moment matrix, the parameters, the coordinates of
-- (q, p) on the exact image basis (least squares, for the rounding), and the
-- status in the convention of the image form (primal and dual exchanged).
kernelSolve = (A,B,b,objP,U0,V0,c0,p00,o) -> (
    n2 := numColumns A;
    n := (-1 + round sqrt(1 + 8*n2))//2;
    np := numColumns B;
    J := if np>0 then columnRankProfile mutableMatrix B else {};
    BJ := B_J;
    R1 := if #J>0 then rowRankProfile mutableMatrix BJ else {};
    R2 := sort toList(set(0..numRows A - 1) - set R1);
    (At,bt) := (A^R2, b^R2);
    B1i := if #J>0 then inverse(BJ^R1);
    if #J>0 then (
        At = At - BJ^R2 * B1i * A^R1;
        bt = bt - BJ^R2 * B1i * b^R1;
        );
    if numRows At > 0 then (
        ind := rowRankProfile mutableMatrix(At|bt);
        (At,bt) = (At^ind, bt^ind);
        );
    half := diagonalMatrix(QQ, flatten for i to n-1 list for j from i to n-1 list (if i==j then 1 else 1/2));
    Ms := toList vecs2smats(transpose(At * half));
    bv := bt;
    if o.NormalizeTrace then (Ms = append(Ms, id_(QQ^n)); bv = bv || matrix{{1_QQ}});
    Cq := if o.TraceObj then id_(QQ^n)
        else if #J>0 and objP != 0 then (
            w := transpose(A^R1) * transpose(B1i) * objP^J;
            first vecs2smats(-half * w))
        else map(QQ^n,QQ^n,0);
    (Xs,yd,Zd,st) := optimize(sdp(Cq,toSequence Ms,bv), Solver=>o.Solver, Verbosity=>o.Verbosity);
    st = if match("primal-dual", st) then st
        else if match("primal", st) then replace("primal", "dual", st)
        else if match("dual", st) then replace("dual", "primal", st) else st;
    if Xs===null or match("infeasible", st) then return (,Zd,,,st);
    q := smat2vec Xs;
    p := map(RR^np, RR^1, 0);
    if #J>0 then (
        pJ := sub(B1i,RR) * (sub(b^R1,RR) - sub(A^R1,RR) * q);
        p = map(RR^np, RR^1, (i,j) -> (k := position(J, jj -> jj==i); if k===null then 0 else pJ_(k,0)));
        );
    coords := if numColumns U0 == 0 then {} else flatten entries solve(
        sub(U0 || V0, RR), (q - sub(c0,RR)) || (p - sub(p00,RR)), ClosestFit=>true);
    (Xs,Zd,p,coords,st)
    )

-- Scaling of tiny problems: if the largest coefficient of f_0 is below 1,
-- the problem is solved for s*f_0 with s = 2^k such that the largest
-- coefficient lies in [1,2).  Solvers work to an absolute accuracy, and the
-- rounding grid 2^-RoundTol is absolute; a power of two keeps exact and
-- dyadic values exact.  Large problems are not scaled down, so the rounding
-- grid is never coarser than without scaling.
scaleFactor = f0 -> (
    if f0 == 0 then return 1;
    m := max apply(terms f0, t -> abs leadCoefficient t);
    s := 1;
    while m * s < 1 do s = 2 * s;
    s
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
    -- with parameters, F lives in the coefficient ring of ring f; so must mon
    if ring mon === ring f and ring F =!= ring f then
        mon = try lift(mon,ring F) else
            error("the monomial vector must not involve the parameters; give it in " | toString ring F);
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
    (A,B,b) := kernelModel(F,v);
    take(imageModel(A,B,b,o), 7)
    )

-- the image model of a kernel model (see createSOSModel)
-- (the eighth value: the basis vectors smat2vec A_i as the columns of U)
imageModel = (A,B,b,o) -> (
    (C,Ai,p0,V,U) := getImageModel(A,B,b,o);
    -- without rounding the model is the same, in floating point: the SDP
    -- sent to the solver does not depend on RoundTol (BUGS.md 30)
    if isExactField(A) and o.RoundTol==infinity then(
        toRR := M -> if M===null then M else sub(M,RR);
        (A,B,b,C,p0,V,U) = (toRR A, toRR B, toRR b, toRR C, toRR p0, toRR V, toRR U);
        if Ai=!=null then Ai = apply(Ai, toRR);
        );

    (C,Ai,p0,V,A,B,b,U)
    )

-- The kernel model A q + B p = b of the SOS problem: q is the upper triangle
-- of the Gram matrix (smat2vec), p the parameters; one row per monomial.
kernelModel = (F,v) -> (
    kk := coefficientRing ring F;
    np := numRows F - 1;
    n := numRows v;
    n2 := n*(n+1)//2;

    -- In a polynomial ring with monic monomials in v, every v_i v_j is a
    -- monomial: K1 and A come from the exponent vectors (fast path); the
    -- rows are in the same order as from the products themselves.
    R := ring v;
    vl := flatten entries v;
    fast := not isQuotientRing R and all(vl, m -> leadCoefficient m == 1);
    local vvT; local K1; local pairRow;
    mons := g -> unique first entries monomials g;
    if fast then (
        ex := apply(vl, m -> first exponents m);
        rowOf := new MutableHashTable;
        K1e := new MutableList;
        for i to n-1 do for j to n-1 do (
            e := ex#i + ex#j;
            if not rowOf#?e then (rowOf#e = #K1e; K1e#(#K1e) = e);
            );
        K1 = apply(toList K1e, e -> R_e);
        pairRow = (i,j) -> rowOf#(ex#i + ex#j);
        )
    else (
        -- monomials in vvT
        vvT = entries(v* transpose v);
        K1 = unique \\ flatten \\ mons \ flatten vvT;
        );

    -- monomials in F and not in vvT
    lmf := unique \\ flatten \\ mons \ flatten entries F;
    K2 := lmf - set K1;
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
    A := if fast then (
            -- column of the pair i <= j (smat2vec order): 1 on the diagonal, 2 off it
            entr := flatten for i to n-1 list for j from i to n-1 list (
                (pairRow(i,j), i*n - (i*(i-1))//2 + j - i) => (if i==j then 1 else 2));
            map(kk^(#K1 + #K2), kk^n2, entr))
        else constructA(K1,vvT,#K2);

    -- Consider search-parameters:
    constructB := (K,F) -> (
        F0 := F^(toList(1..np));
        (M,C) := coefficients transpose F0;
        C0 := map(kk^1,kk^(numcols C),{}) || sub(C,kk);
        idx := hashTable for j to numcols(M)-1 list M_(0,j) => j+1;
        rows := for i to #K-1 list if idx#?(K#i) then idx#(K#i) else 0;
        -(C0^rows)
        );
    B := constructB(K,F);

    (A,B,b)
    )

-- The kernel model of the monomials v_keep, from that of v (length n): the
-- columns of the Gram entries among them; rows that became zero (with right
-- side 0) are dropped.
subModel = (A,B,b,n,keep) -> (
    if #keep==n then return (A,B,b);
    col := (i,j) -> i*n - (i*(i-1))//2 + j - i;
    A = A_(flatten for a to #keep-1 list for c from a to #keep-1 list col(keep#a,keep#c));
    rows := positions(entries(A|B|b), r -> any(r, c -> c != 0));
    (A^rows, B^rows, b^rows)
    )

-- Monomials of v whose diagonal entry of the Gram matrix is forced to 0 by
-- the kernel model (Seiler-Zheng-Balas 2013; Sieve-SDP, Zhu-Pataki-Tran-Dinh
-- 2019): a row without parameters whose entries left are diagonal entries of
-- one sign forces them to 0 if its right side is 0, and is a contradiction if
-- the right side has the other sign; a row with no entries left and a nonzero
-- right side is a contradiction (rows without any entry from the start are
-- left to the linear algebra).  For a PSD Gram matrix a zero diagonal entry
-- forces its row and column to 0, so the monomial is dropped and the rows are
-- read again.  Input: the kernel model of a monomial vector of length n.
-- Returns the positions of the monomials kept (all, if every monomial would
-- be dropped), or null for a contradiction.
forcedZeros = (A,B,b,n) -> (
    exact := isExactField A;
    tol := if exact then 0 else HighPrecision * max(1, norm b);
    pair := flatten for i to n-1 list for j from i to n-1 list (i,j);
    bl := flatten entries b;
    Ae := entries A;
    Be := if numColumns B>0 then entries B else toList(numRows A:{});
    -- (a row without any entry is left to the linear algebra of the image
    -- model, as in the original: no Gram matrix, status null)
    rows := for r to #Ae-1 list (
        if any(Be#r, c -> c != 0) then continue;
        nz := positions(Ae#r, a -> a != 0);
        if #nz==0 then continue;
        (r, for c in nz list (pair#c, Ae#r#c))
        );
    keep := new MutableList from n:true;
    changed := true;
    while changed do (
        changed = false;
        for row in rows do (
            (r, nz) := row;
            live := select(nz, e -> keep#(e#0#0) and keep#(e#0#1));
            if #live==0 then (if abs(bl#r) > tol then return; continue);
            if any(live, e -> e#0#0 != e#0#1) then continue;
            sgns := unique apply(live, e -> e#1 > 0);
            if #sgns > 1 then continue;
            s := if first sgns then 1 else -1;
            if s * bl#r < -tol then return;
            if bl#r == 0 then (
                scan(live, e -> keep#(e#0#0) = false);
                changed = true;
                );
            );
        );
    k := positions(toList keep, i -> i);
    if #k==0 then toList(0..n-1) else k
    )

-- Multiples g of the equations h of degree <= D/2 lie in the span of v, and
-- the multipliers absorb every term of the form g*m (lowerBound(f,h,D)): the
-- Gram matrices then form an unbounded set (BUGS.md 30).  A basis of the span
-- of the g in echelon form has pivot monomials, the largest ones; v without
-- them gives an equivalent problem.  Returns the positions of the monomials
-- kept and, for the moment matrix, the matrix M with v = M w modulo the span
-- of the g (w the monomials kept).
absorbMultiples = (v,G) -> (
    vl := flatten entries v;
    n := #vl;
    R := ring v;
    -- in a quotient ring the equivalence needs normal forms that do not
    -- raise the degree: only for a degree-compatible monomial order
    if isQuotientRing R and not degreeCompatible R then G = {};
    G = select(G, g -> g != 0 and isSubset(flatten entries monomials g, vl));
    if #G==0 then return (toList(0..n-1), null);
    kk := ultimate(coefficientRing, R);
    -- columns ordered from the largest monomial down, so that the pivots are
    -- the largest monomials
    ord := reverse apply(sort apply(n, i -> (vl#i, i)), last);
    Cm := lift(matrix for g in G list for i in ord list coefficient(vl#i,g), kk);
    piv := columnRankProfile mutableMatrix Cm;
    pivots := ord_piv;
    keep := sort toList(set(0..n-1) - set pivots);
    -- rows of Cm in echelon form on the pivots: p_r + sum_{m kept} E_(r,m) m
    rk := rowRankProfile mutableMatrix Cm;
    Cr := Cm^rk;
    E := (inverse Cr_piv) * Cr;
    posOrd := hashTable apply(n, k -> ord#k => k);
    posKeep := hashTable apply(#keep, k -> keep#k => k);
    posPiv := hashTable apply(#pivots, k -> pivots#k => k);
    M := map(kk^n, kk^(#keep), (i,j) ->
        if posKeep#?i then (if posKeep#i==j then 1 else 0)
        else -E_(posPiv#i, posOrd#(keep#j)));
    (keep, M)
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
    (c,p0,V,U) := getImageBasis(A,B,b,o);
    if c===null then return (,,,,);
    (first vecs2smats c, vecs2smats U, p0, V, U)
    )

-- the image model as vectors: q = c + U t, p = p0 + V t (the columns of U
-- are smat2vec of the A_i), without building the matrices
getImageBasis = (A,B,b,o) -> (
    n1 := numColumns A;
    n2 := numColumns B;
    AB := A|B;
    x := linsolve(AB,b);
    if x===null then(
        verbose1("No Gram matrix exists. Terminate.", o);
        return (,,,) );
    c := x^{0..n1-1};
    p0 := x^{n1..n1+n2-1};
    W := - kernelGens AB;
    (c, p0, W^{n1..n1+n2-1}, W^{0..n1-1})
    )

-- the symmetric matrices whose smat2vec vectors are the columns of U, by one
-- product with the matrix that maps smat2vec of a symmetric n x n matrix to
-- its entries (vec2smat per column is slow for many columns)
vecs2smats = U -> (
    kk := ring U;
    n := (-1 + round sqrt(1 + 8*numRows U))//2;
    E := expansionMatrix(kk,n) * U;
    toSequence for k to numColumns U - 1 list reshape(kk^n, kk^n, E_{k})
    )
expansionMatrix = (kk,n) -> (
    idx := (i,j) -> if i<=j then i*n - (i*(i-1))//2 + j - i else j*n - (j*(j-1))//2 + i - j;
    map(kk^(n*n), kk^(n*(n+1)//2),
        flatten for j to n-1 list for i to n-1 list (j*n+i, idx(i,j)) => 1)
    )

-- Is the rational symmetric matrix Q PSD?  A floating point eigenvalue below
-- -1e-10 max(1, n max|Q_ij|) proves that it is not (the error of the conversion and
-- of the eigenvalues is far below that), without the exact LDL
-- decomposition, which decides in all other cases.
isPSDexact = Q -> (
    n := numRows Q;
    if n==0 then return true;
    QR := sub(Q, RR);
    if min eigenvalues(QR, Hermitian=>true) < -1e-10 * max(1, n * norm QR) then return false;
    first PSDdecomposition Q =!= null
    )

parameterVector = method()
parameterVector(RingElement,RingElement) := (f,objFcn) -> (
    -- given a polynomial f = f_0 + \sum_i p_i * f_i
    -- the method returns the vector with the f_i's
    R := ring f;
    checkCoefficientRing R;
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
    -- the objective must be an affine function of the parameters alone
    -- (an element of the coefficient ring, such as x, is not one)
    objErr := "Objective should be a linear function of the parameters.";
    objFcn = try promote(objFcn,R) else error objErr;
    toConst := c -> (
        if ring c =!= kk then c = lift(c,kk);
        if #support c > 0 then error objErr;
        leadCoefficient c );
    (objMons,objCoefs) := coefficients objFcn;
    if any(flatten entries objMons, m -> first degree m > 1) then error objErr;
    scan(flatten entries objCoefs, toConst);
    objP := matrix for t in p list {toConst coefficient(t,objFcn)};
    (F,objP)
    )
parameterVector(RingElement) := (f) -> first parameterVector(f,0_(ring f))

-- Only QQ and RR (and polynomial rings over them, for parameters) are supported
checkCoefficientRing = R -> (
    kk := ultimate(coefficientRing, R);
    if kk === ZZ or char kk > 0 then
        error("expected the coefficient ring QQ or RR, not " | toString kk);
    )

-- sosInIdeal and sosdecTernary default to CSDP: a clear error if it is missing
-- (read from the dependency; if that fails, the dependency reports it)
checkCSDP = o -> (
    if o.Solver =!= "CSDP" then return;
    csdp := try value (SemidefiniteProgramming#"private dictionary")#"csdpProgram" else 1;
    if csdp===null then
        error "the solver CSDP was not found; install it, or use Solver => \"MOSEK\"";
    )

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
    -- 0 is the empty sum of squares; its Gram matrix on the monomial 1 is 0
    if F==0 then return matrix{{1_R}};
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
    -- A form of degree 2e that is SOS with monomials of degree <= D/2 is SOS
    -- with the monomials of degree e (the blocks of other degrees of a PSD
    -- Gram matrix vanish).  Not so in quotient rings, where a square of
    -- lower degree can vanish.
    nonzero := select(flatten entries F, g -> g != 0);
    degs := unique apply(nonzero, g -> first degree g);
    isForm := not isQuotientRing R and isHomogeneous R and #degs==1
        and all(nonzero, isHomogeneous);
    mon := if isForm and even first degs and first degs <= D then
            basis((first degs)//2,R,Variables=>var)
        else if isForm then basis(D//2,R,Variables=>var) -- not SOS in degree D
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

-- The solution is rounded in its coordinates on the exact image basis, each
-- basis vector scaled by a power of 2 to entries of size about 1: the rounded
-- point satisfies the linear constraints exactly, and only PSD is checked
-- ([PP08] 3.2; LOG.md 5.1 decision 3).  For an optimization problem (objP not
-- null) the candidates at a precision d also include the parameters as
-- continued fraction convergents with denominator <= 2^d (all of them, or the
-- objective one with the others rounded to 2^-d), with the Gram matrix
-- projected for them ([PP08] 3.1); after the first precision with a success
-- also every coarser one; the best certified objective of all is returned,
-- so the bound is monotone in RoundTol (LOG.md 5.1 decision 4).
-- RoundTol <= LoopRoundTol: the precisions RoundTol..LoopRoundTol; above:
-- the precision RoundTol only (at most MaxRoundTol).
roundSolution = {RoundTol=>3,Verbosity=>1} >> o -> (imageData,pVec0,Q,A,B,b,objP) -> (
    np := numColumns B;
    U := imageData#"U";
    V := imageData#"V";
    nb := numColumns U;
    UV := if np>0 then U || V else U;
    -- (compress keeps the nonzero entries only: the basis vectors are sparse)
    sc := for i to nb-1 list powerOfTwo max prepend(0, apply(flatten entries compress transpose UV_{i}, abs));
    z := apply(imageData#"coords", sc, (c,t) -> c*t);
    -- the Gram matrix of a vector by one product (see vecs2smats); P (U z)
    -- rather than (P U) z, so that P U (n^2 by the number of basis vectors) is
    -- not kept during the search (memory, LOG.md 6.8)
    n := numRows Q;
    P := expansionMatrix(QQ,n);
    -- the image rounding at precision d: the point first (cheap), then the
    -- Gram matrix and the PSD check
    imagePt := d -> (
        zt := map(QQ^nb, QQ^1, (i,j) -> roundQQ(d, z#i)/sc#i);
        (zt, if np>0 then imageData#"p0" + V * zt)
        );
    imageCheck := (zt,pt) -> (
        Qt := reshape(QQ^n, QQ^n, P * (imageData#"c" + U * zt));
        if imageData#"cone" and Qt==0 then return (false,Qt,pt);
        (isPSDexact Qt, Qt, pt)
        );
    image := d -> imageCheck imagePt d;
    -- the Gram matrix rounded and projected onto A q = b - B p ([PP08] 3.1);
    -- in a polynomial ring A A^T is diagonal and the projection explicit
    -- (a zero row of A A^T: the constraint must already hold, checked)
    AAt := A * transpose A;
    dAAt := apply(numRows AAt, i -> AAt_(i,i));
    diagAAt := AAt == diagonalMatrix(QQ, dAAt);
    Dinv := if diagAAt then diagonalMatrix(QQ, apply(dAAt, a -> if a==0 then 0 else 1/a));
    projected := (d,pt) -> (
        bPar := b - B*pt;
        if not diagAAt then (
            (ok,Qp) := roundPSDmatrix(Q,A,bPar,d);
            if ok then ok = A * smat2vec Qp == bPar;
            return (ok,Qp,pt);
            );
        x0 := smat2vec roundQQ(d,Q);
        xp := x0 - transpose A * (Dinv * (A*x0 - bPar));
        if A*xp != bPar then return (false,null,pt);
        Qx := reshape(QQ^n, QQ^n, P*xp);
        (isPSDexact Qx, Qx, pt)
        );
    -- the candidates at precision d, the image rounding first: pairs of the
    -- parameters and a function that checks the candidate
    candidates := d -> (
        (zt,pt) := imagePt d;
        L := {(pt, () -> imageCheck(zt,pt))};
        if objP=!=null then (
            cf := apply(flatten entries pVec0, x -> lastConvergent(x, 2^d));
            dy := flatten entries roundQQ(d,pVec0);
            k := position(flatten entries objP, c -> c != 0);
            p1 := matrix apply(cf, c -> {c});
            p2 := matrix apply(np, i -> {if i==k then cf#i else dy#i});
            L = L | {(p1, () -> projected(d,p1)), (p2, () -> projected(d,p2))};
            );
        L
        );
    attempts := d -> select(apply(candidates d, c -> c#1()), r -> r#0);
    objective := pt -> (transpose objP * pt)_(0,0);
    d := o.RoundTol;
    dlast := max(d, LoopRoundTol);
    verbose1("Start rational rounding", o);
    if objP===null then (
        while d <= dlast do (
            verbose2("rounding step #" | d, o);
            r := image d;
            if r#0 then return r;
            d = d + 1;
            );
        return (false,null,null);
        );
    -- an optimization problem: the depth is the first precision at which
    -- the image rounding succeeds (or, failing that, any candidate); the
    -- best of all candidates at that and every coarser precision.  So the
    -- result is never worse than the image rounding alone, and monotone in
    -- RoundTol.
    dstar := null;
    while d <= dlast and dstar===null do (
        verbose2("rounding step #" | d, o);
        if (image d)#0 then dstar = d else d = d + 1;
        );
    if dstar===null then (
        -- the image rounding never succeeded: the first precision with any
        -- candidate
        d = o.RoundTol;
        while d <= dlast and #(attempts d)==0 do d = d + 1;
        if d > dlast then return (false,null,null);
        dstar = d;
        );
    -- (for RoundTol <= 0 the coarser precisions start at dstar itself)
    -- The objective depends on the parameters only: a candidate that cannot
    -- be strictly better than the best so far is not checked (the same
    -- result as checking all of them in this order, LOG.md 6.8).
    best := null;
    for d' from min(1,dstar) to dstar do for c in candidates d' do (
        v := objective c#0;
        if best=!=null and v >= objective best#2 then continue;
        r := c#1();
        if r#0 and (best===null or v < objective best#2) then best = r;
        );
    best
    )

-- the power of 2 in [m, 2m] (1 for m = 0)
powerOfTwo = m -> (
    if m==0 then return 1;
    e := 1_QQ;
    while e < m do e = 2*e;
    while e > 2*m do e = e/2;
    e
    )

-- the last continued fraction convergent of x with denominator <= N
lastConvergent = (x,N) -> (
    (h0,h1,k0,k1) := (0,1,1,0);
    y := x;
    c := round x;
    for i to 80 do (
        a := floor y;
        (h0,h1) = (h1, a*h1 + h0);
        (k0,k1) = (k1, a*k1 + k0);
        if k1 > N then break;
        c = h1/k1;
        r := y - a;
        if r == 0 or abs(x - c) < 1e-15 * max(1, abs x) then break;
        y = 1/r;
        );
    promote(c,QQ)
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
    checkCoefficientRing R;
    checkCSDP o;
    if odd D then error "D must be even";
    mon := chooseMons(matrix{{0_R}}, D);
    sol := rawSolveSOS (matrix{{0_R}}, zeros(QQ,0,1), mon, o ++ {NormalizeTrace=>true});
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
    checkCoefficientRing ring h;
    checkCSDP o;
    if odd D then error "D must be even";
    homog := isHomogeneous h;
    (H,m) := makeMultiples(first entries h, D, homog);
    -- only a basis of the multiples is used, so that no nonzero multipliers
    -- give the zero polynomial
    keep := independentPositions H;
    F := matrix transpose {{0_(ring h)}|H_keep};
    sol := rawSolveSOS (F, o ++ {NormalizeTrace=>true});
    (mon,Q,X,tval,sdpstatus) := readSdpResult sol;
    if Q===null or isZero(MedPrecision,Q) then (
        verbose1("no sos polynomial in degree "|D, o);
        return (sdpResult(mon,,X,,sdpstatus),null);
        );
    -- the multiples left out get the coefficient 0
    tval = if tval===null then {} else flatten entries tval;
    pos := hashTable apply(#keep, i -> keep#i => i);
    z := if #tval>0 then 0_(ring tval#0) else 0;
    tval = for k to #H-1 list if pos#?k then tval#(pos#k) else z;
    sol = sdpResult(mon,Q,X,if #tval>0 then transpose matrix{tval},sdpstatus);
    mult := getMultipliers(m,tval,ring mon);
    (sol,mult)
    )

-- positions of a maximal linearly independent subset of a list of polynomials
independentPositions = H -> (
    if #H==0 then return {};
    C := last coefficients matrix{H};
    kk := ultimate(coefficientRing, ring C);
    columnRankProfile mutableMatrix lift(C,kk)
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
    -- Returns two lists of SOSPolys, the numerator and the denominator polys
    checkCoefficientRing ring f;
    checkCSDP o;
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
    (mon,Q,X,tval,sdpstatus) := readSdpResult rawSolveSOS(matrix{{fi}},o);
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
    if not instance(ring X, RealField) then X = sub(X,RR);
    -- The moment matrix of a point x* is c * m(x*) m(x*)^T with c > 0
    -- (c = 1 for polynomials, c = 1/q(x*) for a rational function p/q).
    e := eigenvalues(X,Hermitian=>true);
    emax := max e;
    if emax<=0 or min e < -MedPrecision*emax then
        error "Moment matrix is not positive semidefinite";
    i0 := position(flatten entries mon, y -> y==1);
    -- the rank defect is compared with the moment of 1 as well: when it is
    -- small (a large denominator at x*), the point is not reliable
    sc := if i0=!=null then min(emax, X_(i0,i0)) else emax;
    if #select(e, ei -> ei > LowPrecision*sc) > 1 then
        print "Moment matrix is not rank one, solution might not be correct.";
    -- with the monomial 1: x* = (moments of degree one) / c
    if i0=!=null and X_(i0,i0) > HighPrecision*emax then
        return for j to #I-1 list var_(j,0) => X_(i0,I#j)/X_(i0,i0);
    -- otherwise: the leading eigenvector of the block of the variables
    Y := submatrix(X,I,I);
    (eY,V) := eigenvectors(Y,Hermitian=>true);
    if eY#(-1)<=0 then return for j to #I-1 list var_(j,0) => 0_RR;
    vv := sqrt(eY#(-1)) * V_{#I-1};
    sol := for j to #I-1 list ( var_(j,0) => vv_(j,0) );
    if #I < numRows X then(
        x0 := sub(mon,sol);  X0 := x0 * transpose x0;
        sol1 := for j to #I-1 list ( var_(j,0) => -vv_(j,0) );
        x1 := sub(mon,sol1);  X1 := x1 * transpose x1;
        if norm(X-X1) < norm(X-X0) then sol = sol1;
        );
    sol
    )
recoverSolution(SDPResult) := sol -> (
    if sol#Monomials===null or sol#MomentMatrix===null then return {};
    recoverSolution(sol#Monomials,sol#MomentMatrix)
    )

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
    checkCoefficientRing R;
    checkInputs(D,num,den,h,R);
    o' := new OptionTable from
        {RoundTol=>o.RoundTol, Solver=>o.Solver, Verbosity=>o.Verbosity};

    -- f >= t follows from num - t*den SOS only where den > 0 ([BPT13]
    -- 3.2.2).  So den >= 0 (on {h = 0}) is certified first by an SOS
    -- decomposition of den, or of w*den with w = 1 + x_1^2 + ... + x_n^2 > 0
    -- (a positive den need not be SOS), or of (w-1)*den; failing that, the bound comes from
    -- num*den - t*den^2 SOS, which is valid for every denominator.
    denExact := true;
    if #support den==0 then (
        if leadCoefficient den < 0 then (num,den) = (-num,-den);
        )
    else (
        denCert := sosCertificate(den,h,D,o');
        sgn := 1;
        if denCert===null and numColumns h==0 and not isQuotientRing R then (
            -- In lowest terms, a denominator that changes sign vanishes on a
            -- hypersurface on which the numerator does not, so f is
            -- unbounded below; no SDP is needed (the solver may not stop on
            -- the infeasible relaxation)
            (pos,neg) := signsAtPoints den;
            if pos and neg then (
                verbose1("The denominator changes sign: f is unbounded below.", o);
                return (,sdpResult(,,,,),);
                );
            if neg then (
                sgn = -1;
                denCert = sosCertificate(-den,h,D,o');
                );
            );
        if denCert===null then (
            w := 1 + sum(gens R, x -> x^2);
            denCert = sosCertificate(sgn*w*den,h,D,o');
            );
        -- |x|^2 den SOS gives den >= 0 away from the origin, and at the
        -- origin by continuity (LOG.md 5.1 decision 6)
        if denCert===null then (
            w' := sum(gens R, x -> x^2);
            denCert = sosCertificate(sgn*w'*den,h,D,o');
            );
        if denCert=!=null and sgn<0 then (num,den) = (-num,-den);
        if denCert===null then (
            verbose1("The denominator is not certified nonnegative; using num*den - t*den^2.", o);
            (num,den) = (num*den,den^2);
            if D>=0 then D = max(D, 2*ceiling(max(first degree num, first degree den)/2));
            )
        else denExact = denCert;
        );

    -- prepare input
    (H,m) := makeMultiples(flatten entries h, D, false);
    F := matrix transpose {{num,-den}|H};
    objP := matrix{{-1}} || zeros(ZZ,#H,1);

    -- call solveSOS
    mon := if D>=0 then chooseMons(F,D,Verbosity=>o.Verbosity)
        else chooseMons (F,Verbosity=>o.Verbosity);
    if mon===null then return (,sdpResult(mon,,,,),);
    sol := rawSolveSOS(F,objP,mon,o' ++ {Multiples => absorbed(h,D)});
    (mon',Q,X,tval,sdpstatus) := readSdpResult sol;
    (bound,mult) := (,);
    if tval=!=null then(
        tval = flatten entries tval;
        bound = tval#0;
        mult = getMultipliers(m,drop(tval,1),ring mon');
        -- an exact bound needs an exact certificate of den >= 0 as well
        if not denExact and ring bound === QQ then bound = toRR bound;
        );
    (bound,sol,mult)
    )

-- Does g take positive, negative values at a fixed set of rational points?
signsAtPoints = g -> (
    R := ring g;
    n := numgens R;
    kk := coefficientRing R;
    grid := {{}};
    if n<=3 then for i to n-1 do grid = flatten for p in grid list for c from -3 to 3 list append(p,c);
    vals := {1, -1, 1/2, -1/2, 10, -10, 100, -100, 10000, -10000};
    pts := grid | flatten for c in vals list (
        {toList(n:c)} | for i to n-1 list for j to n-1 list if i==j then c else 0);
    tol := if isExactField kk then 0 else
        HighPrecision * max(1, max apply(terms g, t -> abs leadCoefficient t));
    v := for p in pts list sub(g, matrix(kk,{p}));
    (any(v, a -> a > tol), any(v, a -> a < -tol))
    )

-- Is g a sum of squares modulo the ideal of h (multipliers up to degree D,
-- raised to the degree of g if necessary)?
-- Returns null if not found, else whether the certificate is exact.
sosCertificate = (g,h,D,o) -> (
    if D>=0 then D = max(D, 2*ceiling(first degree g/2));
    (H,m) := makeMultiples(flatten entries h, D, false);
    F := matrix transpose {{g}|H};
    mon := if D>=0 then chooseMons(F,D,Verbosity=>o.Verbosity)
        else chooseMons (F,Verbosity=>o.Verbosity);
    if mon===null then return;
    sol := rawSolveSOS(F,zeros(QQ,#H,1),mon,o ++ {Multiples => absorbed(h,D)});
    if sol#GramMatrix===null then return;
    ring sol#GramMatrix === QQ
    )

-- Is the monomial order of the ring a degree order (the first block GRevLex
-- or GLex over all variables, the standard degree)?
degreeCompatible = R -> (
    mo := (options monoid ambient R).MonomialOrder;
    blocks := select(toList mo, b -> instance(b, Option) and member(first b, {GRevLex, GLex, Lex, Weights, GroupLex, GroupRevLex, RevLex}));
    #blocks > 0 and member(first first blocks, {GRevLex, GLex}) and
        (last first blocks === numgens ambient R or (instance(last first blocks, List) and #(last first blocks) == numgens ambient R)) and
        all(degrees ambient R, d -> d === {1})
    )

-- the multiples h_i x^a of degree at most D/2 (see absorbMultiples)
absorbed = (h,D) -> (
    if D<0 then return {};
    hl := select(flatten entries h, g -> first degree g <= D//2);
    first makeMultiples(hl, D//2, false)
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
        return checkMethod#fun(solver,o);
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
        f = (x-exp(1)*y)^2 + x^2 + (y-2)^2;
        (bound,sol) = lowerBound(f, Solver=>solver,Verbosity=>o.Verbosity);
        equal(bound,4*exp(2)/(2+exp(2)))
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

