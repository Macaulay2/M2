newPackage(
    "SemidefiniteProgramming",
    Version => "0.2",
    Date => "November 2018",
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
    Headline => "semidefinite programming",
    Configuration => {"CSDPexec"=>"","MOSEKexec"=>"mosek","SDPAexec"=>"sdpa","DefaultSolver"=>null},
    AuxiliaryFiles => true,
    PackageExports => {"NumericalAlgebraicGeometry"}
)

export {
--Types
    "SDP",
--Methods/Functions
    "sdp",
    "PSDdecomposition",
    "optimize",
    "roundPSDmatrix",
    "smat2vec",
    "vec2smat",
    "checkOptimize",
    "changeSolver",
    "criticalIdeal",
--Method options
    "Verbosity",
    "Solver",
    "Scaling"
}

--##########################################################################--
-- GLOBAL VARIABLES 
--##########################################################################--

-- Choose default solver
chooseDefaultSolver = execs -> (
    solvers := {"CSDP", "MOSEK", "SDPA"}; --sorted by preference
    found := for i to #solvers-1 list
        if execs#i=!=null then solvers#i else continue;
    if notify then
      print if #found>0 then "Solvers configured: "|demark(", ",found)
        else "Warning: No external solver was found.";
    found = append(found,"M2");
    defaultSolver = ((options SemidefiniteProgramming).Configuration)#"DefaultSolver";
    if not member(defaultSolver,found) then
        defaultSolver = first found;
    if notify then
      print("Default solver: " | defaultSolver);
    defaultSolver)

-- Change a solver path
changeSolver = (solver, execpath) -> (
    execpath = replace(baseFilename execpath | "$", "", execpath);
    if solver == "CSDP" then (
	programPaths#"csdp" = execpath;
	csdpProgram = findCSDP false;
    );
    if solver == "SDPA" then (
	programPaths#"sdpa" = execpath;
	sdpaProgram = findSDPA false;
    );
    if solver == "mosek" then (
	programPaths#"mosek" = execpath;
	mosekProgram = findMOSEK false;
    );
    SemidefiniteProgramming.defaultSolver = chooseDefaultSolver(
	csdpProgram,
	mosekProgram,
	sdpaProgram)
    )

-- for backward compatibility
scan({"CSDP", "MOSEK", "SDPA"}, solver ->
    if not programPaths#?(toLower solver) and not member(
        SemidefiniteProgramming#Options#Configuration#(solver | "exec"),
        {"", toLower solver}) then programPaths#(toLower solver) =
            replace(toLower solver | "$", "",
                SemidefiniteProgramming#Options#Configuration#(solver | "exec"))
)

findCSDP = raiseError -> (
    fin := temporaryFileName() | ".dat-s";
    fin <<  ///3 =mdim
1 =nblocks
3
0 0 0
1 1 1 1 -1
1 1 1 2 -1.5
1 1 1 3 -1.5
1 1 2 3 -.5
2 1 1 2 -.5
2 1 1 3 -1.5
2 1 2 3 -1.5
2 1 3 3 -1
3 1 1 3 -.5
3 1 2 2 1
/// << close;
    findProgram("csdp", "csdp " | fin, RaiseError => raiseError))

findMOSEK = raiseError -> findProgram("mosek", "mosek",
    RaiseError => raiseError)

findSDPA = raiseError -> findProgram("sdpa", "sdpa --version",
    RaiseError => raiseError)

csdpProgram = findCSDP false
mosekProgram = findMOSEK false
sdpaProgram = findSDPA false

defaultSolver = chooseDefaultSolver(csdpProgram, mosekProgram, sdpaProgram)

-- SDP status
StatusFeas = "SDP solved, primal-dual feasible"
StatusPFeas = "SDP solved, primal feasible"
StatusDFeas = "SDP solved, dual feasible"
StatusPInfeas = "SDP solved, primal infeasible"
StatusDInfeas = "SDP solved, dual infeasible"
StatusFailed = "SDP failed"
StatusUnknown = "Solver returns unknown message\n"

-- Constants
HighPrecision = 1e-10 --e.g. for numerical linear algebra
MedPrecision = 1e-6 --e.g. for SDP solutions

--##########################################################################--
-- TYPES
--##########################################################################--

SDP = new Type of HashTable

sdp = method()
sdp (Matrix, Sequence, Matrix) := (C, A, b) -> (
    kk := if all(A|(C,b), isExactField @@ ring) then QQ else RR;
    checkMat := (dims,M) -> (
        if dims!={numrows M,numcols M} then error "Bad matrix dimensions.";
        return sub(M,kk);
        );
    n := numrows C;
    C = checkMat_{n,n} C;
    A = checkMat_{n,n} \ A;
    b = checkMat_{#A,1} b;
    sdp0(C,A,b))
sdp (Matrix, Matrix, Matrix) := (C, A, b) -> sdp(C,sequence A,b)
-- internal constructor that runs no checks:
sdp0 = (C,A,b) -> new SDP from { "C" => C, "A" => A, "b" => b }

-- constructor given symbolic matrix
sdp(List,Matrix,RingElement) := (X,M,objFun) -> (
    n := numrows M;
    if n!=numcols M then error "Matrix must be square";
    coeff := (x,f) -> ( 
        if f==0 then return f;
        if x!=1 and degree(x,f)>1 then 
            error "Entries must be affine functions";
        coefficient(x,f) );
    e := 1_(ring M);
    A := for x in {e}|X list 
        matrix for i to n-1 list for j to n-1 list -coeff(x,M_(i,j));
    b := matrix for x in X list {-coeff(x,objFun)};
    sdp(-A#0,toSequence drop(A,1),b))

ring SDP := P -> ring P#"C";

--##########################################################################--
-- METHODS
--##########################################################################--

verbose1 = method()
verbose1(Net,Number) := (s,Verbosity) -> if Verbosity>=1 then print s
verbose1(Net,OptionTable) := (s,o) -> if o.Verbosity>=1 then print s

verbose2 = method()
verbose2(Net,Number) := (s,Verbosity) -> if Verbosity>=2 then print s
verbose2(Net,OptionTable) := (s,o) -> if o.Verbosity>=2 then print s

--###################################
-- Transition QQ <=> RR
--###################################

isExactField = kk -> (
    try (kk = ring kk);
    kk = ultimate(coefficientRing,kk);
    precision 1_kk == infinity)

-- rounds real number to rational
roundQQ = method()
roundQQ(ZZ,RR) := (d,x) -> round(x*2^d)/2^d
roundQQ(ZZ,Matrix) := (d,X) -> 
     matrix(QQ, applyTable (entries X, roundQQ_d ));

--###################################
-- basicLinearAlgebra
--###################################

zeros = (kk,m,n) -> map(kk^m,kk^n,{})

-- Store a symmetric matrix in a vector, avoiding duplication
smat2vec = method( Options => {Scaling => 1} )
smat2vec(List) := o -> A -> (
    n := #A;
    v := for i to n-1 list
        for j from i to n-1 list 
            if i==j then A#i#j else o.Scaling*A#i#j;
    flatten v)
smat2vec(Matrix) := o -> A -> matrix(ring A, apply(smat2vec(entries A,o), a->{a}))

-- Reverse of the above
vec2smat = method( Options => {Scaling => 1} )
vec2smat(List) := o -> v -> (
    N := #v;
    n := (-1 + round sqrt(1+8*N))//2;
    ct := -1;
    L := for i to n-1 list (toList(i:0) |
        for j from i to n-1 list (ct = ct+1; ct));
    A := table(toList(0..n-1), toList(0..n-1), (i,j) -> 
        if i==j then v_(L#i#j) 
        else if i<j then v_(L#i#j)/(o.Scaling)
        else v_(L#j#i)/(o.Scaling) );
    A)
vec2smat(Matrix) := o -> v -> matrix(ring v, vec2smat(flatten entries v,o))

PSDdecomposition = A -> (
    -- Factors a PSD matrix A = L D L^T
    -- with D diagonal
    kk := ring A;
    if isExactField kk then
        return LDLdecomposition(A);
    if kk=!=RR and not instance(kk,RealField) then
        error "field must be QQ or RR";
    tol := HighPrecision;
    (e,V) := eigenvectors(A,Hermitian=>true);
    if any(e, i -> i < -tol) then return (,,);
    e = max_0 \ e;
    D := diagonalMatrix e;
    P := id_(kk^(numRows A));
    (V,D,P))

LDLdecomposition = (A) -> (
    -- This implements Algorithm 4.2.2 from [Golub-VanLoan-2012]
    kk := ring A;
    if kk=!=QQ then error "field must be QQ";

    n := numRows A;
    Ah := new MutableHashTable;
    for i to n-1 do for j to n-1 do Ah#(i,j) = A_(i,j);
    v := new MutableList from for i to n-1 list 0_kk;
    piv := new MutableList from toList(0..n-1);

    permuteMat := (k,q) -> (  -- k<=q
        if k==q then return;
        tmp := piv#q; piv#q = piv#k; piv#k = tmp;
        for i to n-1 do (tmp := Ah#(i,q); Ah#(i,q) = Ah#(i,k); Ah#(i,k) = tmp;);
        for i to n-1 do (tmp := Ah#(q,i); Ah#(q,i) = Ah#(k,i); Ah#(k,i) = tmp;);
        );

    for k to n-1 do (
        q := k + maxPosition apply(k..n-1, i->Ah#(i,i));
        permuteMat(k,q);

        --  positive semidefinite?
        a := Ah#(k,k);
        if a < 0 then return (,,);
        if a <= 0 then(
            if any(k+1..n-1, i->abs(Ah#(i,k))>0) then return (,,);
            continue;
            );

        -- Schur complement
        for i from k+1 to n-1 do 
            v#i = Ah#(i,k);
        for i from k+1 to n-1 do(
            Ah#(i,k) = v#i/a;
            for j from k+1 to n-1 do
                Ah#(i,j) = Ah#(i,j) - (v#i*v#j)/a;
            );
    );

    L := map(kk^n,kk^n,(i,j)-> if i>j then Ah#(i,j) else if i==j then 1_kk else 0_kk);
    D := map(kk^n,kk^n,(i,j)->if i==j then Ah#(i,j) else 0_kk);
    P := submatrix(id_(kk^n),toList piv);

    (L,D,P))

--###################################
-- Rational Rounding
--###################################

project2linspace = (A,b,x0) -> (
     -- cast into QQ (necessary class to compute inverse)
     A2 := promote (A,QQ);
     -- convert b into a matrix if it is a scalar in QQ/ZZ:
     b2 := promote (matrix{{b}},QQ);
     x02 := promote (x0,QQ);

     -- compute projection:
     xp := x02 - transpose(A2)*((A2*x02-b2)//(A2*transpose(A2)))
     )

roundPSDmatrix = (Q,A,b,d) -> (
     Q0 := roundQQ(d,Q);
     x0 := smat2vec(Q0);
     xp := project2linspace(A,b,x0);
     Q = vec2smat(xp);

     (L,D,P) := LDLdecomposition(Q);
     if L =!= null then (true, Q) else (false,Q)
     )

--###################################
-- EXACT SDP
--###################################

criticalIdeal = method()
criticalIdeal(SDP) := (P) -> rawCriticalIdeal(P,,false)
criticalIdeal(SDP,ZZ) := (P,Rank) -> rawCriticalIdeal(P,Rank,false)

rawCriticalIdeal = (P,Rank,Square) -> (
    (C,A,b) := (P#"C",P#"A",P#"b");
    kk := ring P;
    local x; x = symbol x;
    local y; y = symbol y;
    n := numRows C;
    n2 := binomial(n+1,2);
    m := numRows b;
    R := kk(monoid [x_0..x_(n2-1),y_0..y_(m-1)]);
    (C,A,b) = mat2ring_R (C,A,b);
    -- primal/dual matrices
    X := genericSymmetricMatrix(R,R_0,n);
    y = drop(gens R, n2);
    Z := C - sum(for i to m-1 list y_i * A_i);
    -- primal feasibility
    I1 := ideal(for i to m-1 list trace(A_i*X)-b_(i,0));
    -- complementary slackness
    I2 := if Square then ideal smat2vec entries(Z*X + X*Z)
        else ideal flatten entries(X*Z);
    I := I1 + I2;
    r := Rank;
    if r=!=null then
        I = I + minors(r+1,Z) + minors(n-r+1,X);
    y = matrix transpose {y};
    (I,X,y,Z))

mat2ring = (R,C,A,b) -> (
    C = promote(C,R);
    A = apply(A, Ai -> promote(Ai,R));
    b = promote(b,R);
    (C,A,b))

refine(SDP,Sequence) := o -> (P,X0y0) -> (
    (X0,y0) := X0y0;
    (J,X,y,Z) := rawCriticalIdeal(P,,true);
    pt := smat2vec entries X0 | flatten entries y0;
    pt' := first refine (polySystem J, {pt}, o);
    if pt'#SolutionStatus==RefinementFailure then(
        print "refinement failed";
        return (X0,y0) );
    L := coordinates pt';
    m := numColumns y0;
    X1 := matrix vec2smat drop(L,-m);
    y1 := matrix transpose {take(L,-m)};
    (X1,y1))

--###################################
-- SOLVE SDP
--###################################

optimize = method(
     Options => {Solver=>null, Verbosity => 0} )

optimize(SDP) := o -> P -> (
    solver := chooseSolver o;
    (C,A,b) := (P#"C",P#"A",P#"b");
    (C,A,b) = mat2ring(RR,C,A,b);
    (ok,X,y,Z,sdpstatus) := sdpNoConstraints(C,A,o.Verbosity);
    if ok then null
    else if solver == "M2" then
        (X,y,Z,sdpstatus) = simpleSDP(C,A,b,Verbosity=>o.Verbosity)
    else if solver == "CSDP" then
        (X,y,Z,sdpstatus) = solveCSDP(C,A,b,Verbosity=>o.Verbosity)
    else if solver == "SDPA" then
        (X,y,Z,sdpstatus) = solveSDPA(C,A,b,Verbosity=>o.Verbosity)
    else if solver == "MOSEK" then
        (X,y,Z,sdpstatus) = solveMOSEK(C,A,b,Verbosity=>o.Verbosity)
    else
        error "unknown SDP solver";
    verbose1("Status: "|sdpstatus, o);
    ntries := 6;
    (y,Z) = findNonZeroSolution(C,A,b,o,y,Z,ntries,o.Verbosity);
    (X,y,Z,sdpstatus))

findNonZeroSolution = (C,A,b,o,y,Z,ntries,Verbosity) -> (
    -- Heuristic to trick the solver into returning a nonzero solution of an SDP
    -- by changing the objective.
    if y===null then return (y,Z);
    if not(C==0 and b==0 and y==0) then return (y,Z);
    verbose1("Zero solution obtained. Trying again.", Verbosity);
    m := numRows b;
    badCoords := set();
    iszero := a -> norm a < MedPrecision;
    for i to ntries-1 do(
        if #badCoords==m then break;
        b' := map(RR^m,RR^1, (j,l) -> 
            if member(j,badCoords) then 0 else random(RR)-.5 );
        (X',y',Z',sdpstatus') := optimize(sdp0(C,A,b'),o);
        if Z'=!=null and not iszero Z' then return (y',Z');
        if X'===null and y'=!=null and (transpose(-b') * y')_(0,0) < -.1 then
            badCoords = badCoords + set select(0..m-1, j -> not iszero y'_(j,0));
        );
    (y,Z))

optimize(SDP, Matrix) := o -> (P,y0) -> (
    solver := chooseSolver o;
    (C,A,b) := (P#"C",P#"A",P#"b");
    (C,A,b) = mat2ring(RR,C,A,b);
    y0 = promote(y0,RR);
    (ok,X,y,Z,sdpstatus) := sdpNoConstraints(C,A,o.Verbosity);
    if ok then null
    else if solver != "M2" then (
        verbose1("Warning: Initial point will not be used",o);
        return optimize(P,o) )
    else
        (X,y,Z,sdpstatus) = simpleSDP2(C,A,b,y0,true,false,Verbosity=>o.Verbosity);
    verbose1("Status: "|sdpstatus, o);
    (X,y,Z,sdpstatus) )

chooseSolver = o -> if o.Solver=!=null then o.Solver else defaultSolver

-- Solve very simple SDP with no constraints
sdpNoConstraints = (C,A,Verbosity) -> (
    tol := HighPrecision;
    if #A==0 then(
        lambda := min eigenvalues(C, Hermitian=>true);
        if lambda>=-tol then(
            verbose1("SDP solved in preprocessing",Verbosity);
            y0 := zeros(RR,#A,1);
            return (true, 0*C, y0, C, StatusFeas);
        )else(
            return (true,,,,StatusDInfeas);
            );
        );
    (false,,,,))

-- check trivial cases and solve them directly
trivialSDP = (C,A,b,Verbosity) -> (
    if #A==0 or b==0 then(
        lambda := min eigenvalues(C, Hermitian=>true);
        if lambda>=0 then(
            verbose1("SDP solved in preprocessing",Verbosity);
            y0 := zeros(RR,#A,1);
            return (true, 0*C, y0, C, StatusFeas);
        )else if #A==0 then(
            return (true,,,,StatusDInfeas);
            );
        );
    (false,,,,))


-- Implementation of SDP in Macaulay2
-- Algorithm: Dual interior point method
-- see Boyd, Vandenberghe "Convex Optimization" pp. 618-619, pp. 463-466
simpleSDP = {Verbosity => 0} >> o -> (C,A,b) -> (
    (ok,X,y,Z,sdpstatus) := trivialSDP(C,A,b,o.Verbosity);
    if ok then return (X,y,Z,sdpstatus);

    verbose1("Running M2 Solver", o);
    R := RR;
    n := numRows C;

    -- try to find strictly feasible starting point --
    local Xnull; local sdpstatus;
    local y; local Z;
    lambda := min eigenvalues (C, Hermitian=>true);
    if lambda > 0 then
        y = zeros(R,#A,1)
    else(
        verbose2("Computing strictly feasible solution...", o);
        y =  zeros(R,#A,1) || matrix{{lambda*1.1}};
        obj :=  zeros(R,#A,1) || matrix{{1_R}};
        (Xnull,y,Z,sdpstatus) = simpleSDP2(C,append(A,id_(R^n)), obj, y, false, true, Verbosity=>o.Verbosity);
        if y===null then 
            return (Xnull,,,StatusFailed);
        y = transpose matrix {take (flatten entries y,numRows y - 1)};
        );
    verbose2("Computing an optimal solution...", o);
    simpleSDP2(C, A, b, y, false, false, o) )


-- This second part solves given an interior starting point.
simpleSDP2 = {Verbosity => 0} >> o -> (C,A,mb,y,checktrivial,UntilObjNegative) -> (
    if checktrivial then (
        (ok,X',y',Z',sdpstatus') := trivialSDP(C,A,mb,o.Verbosity);
        if ok then return (X',y',Z',sdpstatus');
        );

    Xnull := null;
    R := RR;
    n := numgens target C;
    b := -mb;

    m := numgens target y;
    mu := 1_R;
    theta := 10_R;
    iter := 1;
    NewtonIterMAX := 40;

    verbose2("#It:       b'y      dy'Hdy   mu   alpha", o);

    while mu > 0.000001 do (
        mu = mu/theta;
        while true do (
            S := C - sum toList apply(0..m-1, i-> y_(i,0) * A_i);
            try Sinv := solve(S, id_(target S)) else (
                verbose1("Slack matrix is singular", o);
                return (Xnull,,,StatusFailed) );
            -- compute Hessian:
            H := map(R^m,R^m,(i,j) -> trace(Sinv*A_i*Sinv*A_j));
            if H==0 then (
                verbose1("Hessian is zero", o);
                return (Xnull,,,StatusFailed) );
            -- compute gradient:
            g := map(R^m,R^1,(i,j) -> b_(i,0)/mu + trace(Sinv*A_i));
            
            -- compute damped Newton step:
            dy := -solve(H,g,ClosestFit=>true);
            alpha := backtrack(S, -sum for i to m-1 list matrix(dy_(i,0) * entries A_i), o.Verbosity);
            if alpha===null then return (Xnull,,,StatusFailed);
            y = y + transpose matrix {alpha* (flatten entries dy)};
            lambda := (transpose dy*H*dy)_(0,0);
            obj := transpose b * y;
            
            -- print some information:
            verbose2(iter | ":  " | net obj | "    " | net lambda | "    " | net mu | "    " | net alpha, o);

            iter = iter + 1;
            if iter > NewtonIterMAX then (
                verbose2("Warning: exceeded maximum number of iterations", o);
                break);
            if UntilObjNegative and (obj_(0,0) < 0) then break;
            if lambda < 0.4 then break;
            ); 
        );
    Z := C - sum(for i to #A-1 list y_(i,0) * A_i);
    (Xnull,y,Z,StatusDFeas))

backtrack = (S0, dS, Verbosity) -> (
     R := ring S0;
     alpha := 1_R;
     BacktrackIterMAX := 100;
     S :=  matrix( alpha * entries dS) + S0;
     
     cnt := 1;     
     while min eigenvalues(S,Hermitian=>true) <= 0 do (
      cnt = cnt + 1;
      alpha = alpha / sqrt(2_R);
      S = S0 + matrix( alpha * entries dS);
      if cnt > BacktrackIterMAX then (
          verbose1("line search did not converge.", Verbosity);
          return null );
      );
     alpha)

--###################################
-- Interface to CSDP
--###################################

solveCSDP = method( Options => {Verbosity => 0} )
solveCSDP(Matrix,Sequence,Matrix) := o -> (C,A,b) -> (
    -- CSDP expects the file fparam to be in the working directory.
    -- That's why we need to change directory before executing csdp.
    if csdpProgram === null then csdpProgram = findCSDP true;
    n := numColumns C;
    fin := temporaryFileName() | ".dat-s";
    (dir,fin1) := splitFileName(fin);
    fparam := dir | "param.csdp";
    fout := temporaryFileName();
    writeSDPA(fin,C,A,b);
    writeCSDPparam(fparam);
    verbose1("Executing CSDP", o);
    verbose1("Input file: " | fin, o);
    csdpRun := runProgram(csdpProgram, fin1 | " " | fout,
	RunDirectory => dir, KeepFiles => true, RaiseError => false);
    handleErrors(csdpRun#"return value", csdpRun#"error file", o.Verbosity);
    verbose1("Output file: " | fout, o);
    fout2 := csdpRun#"output file";
    (X,y,Z,sdpstatus) := readCSDP(fout,fout2,n,o.Verbosity);
    y = checkDualSol(C,A,y,Z,o.Verbosity);
    (X,y,Z,sdpstatus))

handleErrors = (r, tmp, Verbosity) -> (
    if r == 32512 then error "Executable not found.";
    if r == 11 then error "Segmentation fault.";
    if r>0 then (
        txt := get tmp;
        if #txt>0 then(
            verbose1(txt, Verbosity);
            error "Command could not be executed." );
        );
    )

splitFileName = (fname) -> (
    s := separate("/",fname);
    dir := demark("/",drop(s,-1))|"/";
    file := last s;
    (dir,file))

-- SDPA file format is a shared input format of SDPA and CSDP
writeSDPA = (fin,C,A,b) -> (
    digits := 16;
    formatD := format_digits;
    m := length A;
    n := numColumns C;
    A = prepend(C,A);
    f := openOut fin;
    smat2str := (a,pref) -> (
        s := "";
        for i to n-1 do
            for j from i to n-1 do
                if a_(i,j)!=0 then
                    s = s | pref | i+1 | " " | j+1 | " " | formatD a_(i,j) | "\n";
        return s;
        );
    f << "*SDPA file generated by SemidefiniteProgramming.m2" << endl;    
    f << m << " =mdim" << endl;
    f << "1 =nblocks" << endl;
    f << n << endl;
    f << demark(" ", formatD \ flatten entries(-b)) << endl;
    for i to m do
        f << smat2str(-A#i, i|" 1 ");
    f << close;
    )

-- Writes parameter file for CSDP.
-- All but one are also defaults
writeCSDPparam = (fparam) -> (
    f := openOut fparam;
    f << "axtol=1.0e-8" << endl;
    f << "atytol=1.0e-8" << endl;
    f << "objtol=1.0e-8" << endl;
    f << "pinftol=1.0e8" << endl;
    f << "dinftol=1.0e8" << endl;
    f << "maxiter=100" << endl;
    f << "minstepfrac=0.90" << endl;
    f << "maxstepfrac=0.97" << endl;
    f << "minstepp=1.0e-8" << endl;
    f << "minstepd=1.0e-8" << endl;
    f << "usexzgap=1" << endl;
    f << "tweakgap=0" << endl;
    f << "affine=0" << endl;
    f << "printlevel=1" << endl;
    f << "perturbobj=0" << endl; -- This one is changed from the default
    f << "fastmode=0" << endl;
    f << close;
    )

-- read CSDP output
readCSDP = (fout,fout2,n,Verbosity) -> (
    sdpa2matrix := s -> (
        e := for i in s list (i_2-1,i_3-1) => i_4;
        e' := for i in s list (i_3-1,i_2-1) => i_4;
        map(RR^n, RR^n, e|e'));
    readLine := l -> for s in separate(" ",l) list if s=="" then continue else value s;
    --READ SOLUTIONS
    local sdpstatus;
    text := get fout;
    text = replace("\\+","",text);
    L := lines text;
    y := matrix(RR,transpose{readLine L_0});
    S := readLine \ drop(L,1);
    S1 := select(S, l -> l_0==1);
    S2 := select(S, l -> l_0==2);
    Z := sdpa2matrix(S1); -- slack matrix
    X := sdpa2matrix(S2); -- dual solution
    -- READ STATUS
    text = get fout2;
    s := select(lines text, l -> match("Success",l));
    if #s==0 then( return (,,,StatusFailed) );
    s = first s;
    verbose2(text,Verbosity);
    if match("SDP solved",s) then 
        sdpstatus = StatusFeas
    else if match("primal infeasible",s) then(
        sdpstatus = StatusPInfeas;
        X=null; )
    else if match("dual infeasible",s) then (
        sdpstatus = StatusDInfeas;
        y=null;Z=null; )
    else 
        sdpstatus = StatusUnknown | s;
    (X,y,Z,sdpstatus))

-- A heuristic to postprocess output of CSDP
checkDualSol = (C,A,y,Z,Verbosity) -> (
    if y===null then return;
    yA := sum for i to #A-1 list y_(i,0)*A_i;
    if norm(Z-C+yA)<MedPrecision then return y;
    verbose2("updating dual solution",Verbosity);
    AA := transpose matrix(RR, smat2vec \ entries \ toList A);
    bb := transpose matrix(RR, {smat2vec entries(C-Z)});
    y = solve(AA,bb,ClosestFit=>true);
    y)

--###################################
-- Interface to SDPA
--###################################

solveSDPA = method( Options => {Verbosity => 0} )
solveSDPA(Matrix,Sequence,Matrix) := o -> (C,A,b) -> (
    if sdpaProgram === null then sdpaProgram = findSDPA true;
    n := numColumns C;
    fin := temporaryFileName() | ".dat-s";
    fout := temporaryFileName() ;
    writeSDPA(fin,C,A,b);
    verbose1("Executing SDPA", o);
    verbose1("Input file: " | fin, o);
    sdpaRun := runProgram(sdpaProgram, fin | " " | fout,
	KeepFiles => true, RaiseError => false);
    handleErrors(sdpaRun#"return value", sdpaRun#"error file", o.Verbosity);
    verbose1("Output file: " | fout, o);
    (X,y,Z,sdpstatus) := readSDPA(fout,n,o.Verbosity);
    (X,y,Z,sdpstatus))

readSDPA = (fout,n,Verbosity) -> (
    readVec := l -> (
        l = replace("([{} +])","",l);
        for s in separate(",",l) list if s=="" then continue else value s
    );
    readMatrix := ll -> 
        matrix(RR, for l in ll list readVec l);
    text := get fout;
    L := lines text;
    --READ SOLUTIONS
    local sdpstatus;
    y := null; X := null; Z := null;
    i := position(L, l -> match("xVec =",l));
    if i=!=null then 
        y = transpose matrix(RR, {readVec L#(i+1)});
    i = position(L, l -> match("xMat =",l));
    if i=!=null then 
        Z = matrix(RR, for j to n-1 list readVec L#(i+j+2));
    i = position(L, l -> match("yMat =",l));
    if i=!=null then 
        X = matrix(RR, for j to n-1 list readVec L#(i+j+2));
    --READ STATUS
    verbose2(text,Verbosity);
    s := first select(L, l -> match("phase.value",l));
    if match("pdOPT|pdFEAS",s) then 
        sdpstatus = StatusFeas
    else if match("dFEAS",s) then 
        sdpstatus = StatusPFeas
    else if match("pFEAS",s) then 
        sdpstatus = StatusDFeas
    else if match("dUNBD|pINF_dFEAS",s)  then(
        sdpstatus = StatusDInfeas;
        y=null;Z=null; )
    else if match("pUNBD|pFEAS_dINF",s) then(
        sdpstatus = StatusPInfeas;
        X=null; )
    else if match("noINFO|pdINF",s) then(
        sdpstatus = StatusFailed;
        X=null;y=null;Z=null; )
    else
        sdpstatus = StatusUnknown | s;
    (X,y,Z,sdpstatus))

--###################################
-- Interface to MOSEK
--###################################

solveMOSEK = method( Options => {Verbosity => 0} )
solveMOSEK(Matrix,Sequence,Matrix) := o -> (C,A,b) -> (
    if mosekProgram === null then mosekProgram = findMOSEK true;
    n := numColumns C;
    fin := temporaryFileName() | ".cbf";
    fout := replace(".cbf",".sol",fin);
    writeMOSEK(fin,C,A,b);
    verbose1("Executing MOSEK", o);
    verbose1("Input file: " | fin, o);
    mosekRun := runProgram(mosekProgram, fin, KeepFiles => true,
	RaiseError => false);
    fout2 := mosekRun#"output file";
    handleErrors(mosekRun#"return value", mosekRun#"error file", o.Verbosity);
    verbose1("Output file: " | fout, o);
    (X,y,Z,sdpstatus) := readMOSEK(fout,fout2,n,o.Verbosity);
    (X,y,Z,sdpstatus))

-- write mosek input file (CBF format)
writeMOSEK = (fin,C,A,b) -> (
    version := 1;
    digits := 16;
    formatD := format_digits;
    m := length A;
    n := numColumns C;
    f := openOut fin;
    smat2str := (a,pref) -> (
        s := "";
        for i to n-1 do
            for j from 0 to i do
                if a_(i,j)!=0 then
                    s = s | pref | i | " " | j | " " | formatD a_(i,j) | "\n";
        return s;
        );
    nlines := str -> #select("^",str)-1;
    -- header
    f << "# CBF file generated by SemidefiniteProgramming.m2" << endl;    
    f << "VER" << endl << version << endl;
    f << "OBJSENSE" << endl << "MIN" << endl;
    f << "PSDVAR" << endl << 1 << endl << n << endl;
    f << "CON" << endl;
    f << m << " " << 1 << endl << "L= " << m << endl;
    -- objective function
    f << "OBJFCOORD" << endl;
    Cstr := smat2str(C, "0 ");
    f << nlines Cstr << endl << Cstr;
    -- constraints
    f << "FCOORD" << endl;
    Astr := concatenate for i to m-1 list 
        smat2str(A#i, i|" 0 ");
    f << nlines Astr << endl << Astr;
    -- constants
    f << "BCOORD" << endl << m << endl;
    for i to m-1 do
        f << i << " " << formatD(-b_(i,0)) << endl;
    f << close;
    )

readMOSEK = (fout,fout2,n,Verbosity) -> (
    splitline := l -> separate(" ", replace(" +"," ",l));
    verbose2(get fout2, Verbosity);
    text := get fout;
    L := lines replace("\\+","",text);
    -- READ SOL y
    Ly := select(L, match_" EQ ");
    y := matrix(RR, for l in Ly list(
        l = splitline(l);
        {value l#6 - value l#7} ));
    -- READ SOL X,Z
    Lpsd := select(L, match_"BARX1");
    k := #Lpsd;
    Xh := new MutableList from 2*k:null;
    Zh := new MutableList from 2*k:null;
    for s to k-1 do(
        l := splitline(Lpsd#s);
        i := value l#2;
        j := value l#3;
        Xij := value l#4;
        Zij := value l#5;
        Xh#(2*s) = (i,j)=>Xij;
        Xh#(2*s+1) = (j,i)=>Xij;
        Zh#(2*s) = (i,j)=>Zij;
        Zh#(2*s+1) = (j,i)=>Zij
	);
    X := map(RR^n,RR^n,toList Xh);
    Z := map(RR^n,RR^n,toList Zh);
    -- READ STATUS
    local sdpstatus;
    s := select(L, l -> match("PROBLEM STATUS",l));
    if #s==0 then return (,,,StatusFailed);
    s = first s;
    if match("PRIMAL_AND_DUAL_FEASIBLE",s) then 
        sdpstatus = StatusFeas
    else if match("PRIMAL_FEASIBLE",s) then
        sdpstatus = StatusPFeas
    else if match("DUAL_FEASIBLE",s) then
        sdpstatus = StatusDFeas
    else if match("UNKNOWN|ILL_POSED",s) then(
        sdpstatus = StatusFailed;
        X=null; y=null;Z=null; )
    else if match("PRIMAL_INFEASIBLE",s) then(
        sdpstatus = StatusPInfeas;
        X=null; )
    else if match("DUAL_INFEASIBLE",s) then (
        sdpstatus = StatusDInfeas;
        y=null;Z=null; )
    else 
        sdpstatus = StatusUnknown | s;
    (X,y,Z,sdpstatus))


--###################################
-- Methods for testing
--###################################

--checkOptimize
checkOptimize = method( Options => {Verbosity => 0} )
checkOptimize(String) := o -> (solver) -> (
    tol := .001;
    equal := (y0,y) -> y=!=null and norm(y0-y)<tol*(1+norm(y0));
    checkZ := (C,A,y,Z) -> if y===null then false
        else ( yA := sum for i to #A-1 list y_(i,0)*A_i; norm(Z-C+yA)<MedPrecision );
    local C; local b; local A; local A1; local A2; local A3; 
    local y0; local y; local X; local Z; local yopt; local sdpstatus;

    t0:= (
        C = matrix{{0,2,0,0,0,0},{2,0,0,0,0,0},
         {0,0,10,0,0,0},{0,0,0,10,0,0},{0,0,0,0,0,0},{0,0,0,0,0,0}};
        A1 = matrix{{-1,0,0,0,0,0},{0,0,0,0,0,0},
         {0,0,1,0,0,0},{0,0,0,0,0,0},{0,0,0,0,-1,0},{0,0,0,0,0,0}};
        A2 = matrix{{0,0,0,0,0,0},{0,-1,0,0,0,0},
         {0,0,0,0,0,0},{0,0,0,1,0,0},{0,0,0,0,0,0},{0,0,0,0,0,-1}};
        A = (A1,A2);
        y0 = matrix{{7},{9}};
        b = matrix{{-1},{-1}};
        (X,y,Z,sdpstatus) = optimize(sdp0(C,A,b),y0,Solver=>solver,Verbosity=>o.Verbosity);
        yopt = matrix{{2.},{2.}};
        equal(yopt,y)
        );

    t1:= (
        C = matrix {{2,1,-1},{1,0,0},{-1,0,5}};
        A1 = matrix {{0,0,1/2},{0,-1,0},{1/2,0,0}};
        A2 = matrix {{1,0,0},{0,1,0},{0,0,1}};
        A = (A1,A2);
        b = matrix {{0},{1}};
        y0 = matrix {{0},{-.486952}};
        (X,y,Z,sdpstatus) = optimize(sdp0(C,A,b),y0,Solver=>solver,Verbosity=>o.Verbosity);
        yopt = matrix{{1.97619},{.466049}};
        equal(yopt,y)
        );

    t2:= (
        C = matrix{{2,2,-1,3},{2,0,0,2},{-1,0,1,0},{3,2,0,1}};
        A1 = matrix{{-1,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0}};
        A2 = matrix{{0,0,0,1/2},{0,-1,0,0},{0,0,0,0},{1/2,0,0,0}};
        A = (A1,A2);
        b = matrix{{-1},{0}};
        (X,y,Z,sdpstatus) = optimize(sdp0(C,A,b),Solver=>solver,Verbosity=>o.Verbosity);
        yopt = matrix{{0.},{4.}};
        equal(yopt,y)
        );

    t3:= ( -- not strictly feasible
        C = matrix {{2,2,-1,3},{2,0,0,2},{-1,0,1,0},{3,2,0,1}};
        A1 = matrix {{0,0,0,1/2},{0,-1,0,0},{0,0,0,0},{1/2,0,0,0}};
        A = sequence A1;
        b = matrix {{-1}};
        (X,y,Z,sdpstatus) = optimize(sdp0(C,A,b),Solver=>solver,Verbosity=>o.Verbosity);
        yopt = 4.;
        equal(yopt,y)
        );

    t4:= ( -- zero objective
        C = matrix(RR, {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}});
        A1 = matrix(RR, {{1, 3/2, 3/2}, {3/2, 0, 1/2}, {3/2, 1/2, 0}});
        A2 = matrix(RR, {{0, 1/2, 3/2}, {1/2, 0, 3/2}, {3/2, 3/2, 1}});
        A3 = matrix(RR, {{0, 0, 1/2}, {0, -1, 0}, {1/2, 0, 0}});
        A = (A1,A2,A3);
        b = matrix(RR, {{0}, {0}, {0}});
        (X,y,Z,sdpstatus) = optimize(sdp0(C,A,b), Solver=>solver,Verbosity=>o.Verbosity);
        checkZ(C,A,y,Z)
        );

    {t0,t1,t2,t3,t4})

--##########################################################################--
-- Documentation and Tests
--##########################################################################--

beginDocumentation()

load "./SemidefiniteProgramming/SDPdoc.m2"

--0
TEST /// --sdp construction
    R = QQ[u,v,w];
    M = matrix {{1,u,3-v},{u,5,w},{3-v,w,9+u}};
    objFun = u+v+w;
    P = sdp({u,v,w}, M, objFun);
    A=P#"A"; b=P#"b"; C=P#"C"; 
    assert( #A == 3 )
    assert( A#0 == -matrix(QQ,{{0,1,0},{1,0,0},{0,0,1}}) )
    assert( b == -matrix(QQ,{{1},{1},{1}}) )
    assert( C == matrix(QQ,{{1,0,3},{0,5,0},{3,0,9}}) )
///

--1
TEST /// --smat2vec
    A = matrix(QQ, {{1,2,3,4},{2,5,6,7},{3,6,8,9},{4,7,9,10}})
    v = smat2vec A
    assert( v == matrix(QQ, {{1},{2},{3},{4},{5},{6},{7},{8},{9},{10}}) )
    assert( vec2smat v == A )
///

--2
TEST /// --PSDdecomposition
    debug needsPackage "SemidefiniteProgramming"
    equal = (f1,f2) -> norm(f1-f2) < HighPrecision;
    
    A = matrix(QQ, {{5,3,5},{3,2,4},{5,4,10}})
    (L,D,P) = PSDdecomposition A
    assert(L=!=null and L*D*transpose L == transpose P * A * P)
    (L,D,P) = PSDdecomposition promote(A,RR)
    assert(L=!=null and equal(L*D*transpose L, transpose P * A * P))
    
    V = random(QQ^12,QQ^8)
    A = V * transpose V 
    (L,D,P) = PSDdecomposition(A)
    assert(L=!=null and L*D*transpose L == transpose P * A * P)

    V = random(RR^12,RR^8)
    A = V * transpose V 
    (L,D,P) = PSDdecomposition(A)
    assert(L=!=null and equal(L*D*transpose L, transpose P * A * P))

    -- this matrix is not psd, but its principal minors are zero
    A = matrix(QQ,{{1,-1,1},{-1,1,1},{1,1,1}})
    (L,D,P) = PSDdecomposition A
    assert(L===null)
///

--3
TEST /// --roundPSDmatrix
    Q=matrix{{2.01,0,0},{0,1.1,0},{0,0,2}}
    A=matrix{{1,0,0,0,0,0},{0,0,0,1,0,0},{0,0,0,0,0,1}}
    b=matrix{{2},{1},{2}}
    (boolv,Qpsd)=roundPSDmatrix(Q,A,b,10)
    Qtrue = matrix(QQ,{{2,0,0},{0,1,0},{0,0,2}})
    assert(Qpsd==Qtrue and boolv)

    -- BUG: the next example fails with for d=1
    Q=matrix{{1,-0.75,-0.75},{-0.75,1,0.99},{-0.75,0.99,1}}
    A=matrix{{1,0,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,0,1}}
    b=matrix{{1},{1},{1},{1}}
    (boolv,Qpsd)=roundPSDmatrix(Q,A,b,10)
    Qtrue = matrix{{1, -3/4, 1}, {-3/4, 1, 507/512}, {1, 507/512, 1}};
    assert(Qpsd==Qtrue and not boolv)

    Q=matrix{{2,-1,-0.75},{-1,2,-1.1},{-0.75,-1.1,2}}
    A=matrix {{1, 3, 0, 0, 0, 0}, {2, 0, 5, 1, 0, 0}, {0, 0, 0, 4, -3, 1}, {6, 0, 1, 0, 0, -5}}
    b=matrix{{1},{4},{-3},{1}}
    (boolv,Qpsd)=roundPSDmatrix(Q,A,b,100)
    Qtrue = matrix{{27446399799074697971/15073547952809050112, -4124283948755215953/ 15073547952809050112, 8072814922052298793/45220643858427150336}, {-4124283948755215953/15073547952809050112, -24159897971001080447/ 45220643858427150336, 14488868131185674623/15073547952809050112}, {8072814922052298793/45220643858427150336, 14488868131185674623/ 15073547952809050112, 91377473489393942387/45220643858427150336}}
    assert(Qpsd==Qtrue and not boolv)
///

--4
TEST /// --optimize
    debug needsPackage "SemidefiniteProgramming"

    -- trivial cases (solved in preprocessing)
    P = sdp0(matrix{{1,0},{0,-1}},(),zeros(QQ,0,1));
    (X,y,Z,sdpstatus) = optimize(P,Solver=>"M2");
    assert(y===null and X===null);
    P = sdp0(matrix{{1,0},{0,1}},(),zeros(QQ,0,1));
    (X,y,Z,sdpstatus) = optimize(P,Solver=>"M2");
    assert(y==0);

    results := checkOptimize("CSDP")
    assert all(results,t->t=!=false);
///

--5
TEST /// --criticalIdeal
    A = (-matrix{{0,1,0},{1,0,0},{0,0,1}}, matrix{{0,0,1},{0,0,0},{1,0,0}}, -matrix{{0,0,0},{0,0,1},{0,1,0}});
    (C, b) = (matrix{{1/1,0,3},{0,5,0},{3,0,9}}, matrix{{-1},{-1},{-1}});
    P = sdp(C,A,b);
    (I,X,y,Z) = criticalIdeal P;
    assert(degree I==6);
    (I,X,y,Z) = criticalIdeal(P, 1);
    assert(degree I==4);
///

--6
TEST /// --refine
    debug needsPackage "SemidefiniteProgramming"
    tol = HighPrecision;
    P = sdp(matrix{{1,0},{0,2}}, matrix{{0,1},{1,0}}, matrix{{-1}});
    (X0,y0) = (matrix{{.71, -.5}, {-.5, .35}}, matrix{{-1.41}})
    (X1,y1) = refine(P,(X0,y0))
    assert(norm(y1+sqrt 2)<tol)
///

