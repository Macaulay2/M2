--------------------------------------------------------------------
----- Creation of main objects (OscRing, oscQuadrics)
--------------------------------------------------------------------

oscRing = method(Options => {
        Symbols => (getSymbol "x", getSymbol "y"),
        CoefficientRing => QQ,
        Reduced => false
        })

oscRing(Graph, List) := Ring => opts -> (G, params) -> (
    kk := opts.CoefficientRing;
    s := opts#Symbols#1;
    c := opts#Symbols#0;
    verticesG := sort vertices G;
    vertexList := if not opts.Reduced then verticesG else drop(verticesG, 1);
    clist := for v in vertexList list c_v;
    slist := for v in vertexList list s_v;
    R := if #params > 0
      then kk[clist,slist,params]
      else kk[clist, slist];
    R.numOscillators = # verticesG;
    R.Symbols = (c,s);
    R.Graph = G;
    R.Reduced = opts.Reduced;
    R
)
oscRing(Graph) := Ring => opts -> G -> oscRing(G, {}, opts)
oscRing(ZZ) := Ring => opts -> n -> oscRing(ringOscillatorGraph(n), {}, opts)


---------------------------------------------------
-- Jacobian Constructions
--------------------------------------------------

oscJacobian = method(Options => {Reduced=>false})
oscJacobian Ideal := opts -> P -> (
    -- P is a system created with oscSystem
    -- each polynomial should be <= linear in R_i, R_(n+i), all i=0..n-1
    R := ring P;
    G := R.Graph;
    verticesG := sort vertices G;
    vertexList := if R.Reduced or opts.Reduced then drop(verticesG, 1) else verticesG;
    -- P1 := ideal drop(P_*,1);
    sine := (i) -> if member(i, vertexList) then (select(R.generators, k -> toString k == toString (R.Symbols_1)_i))_0 else 0_R;
    cosine := (i) -> if member(i, vertexList) then (select(R.generators, k -> toString k == toString (R.Symbols_0)_i))_0 else 1_R;
    if not R.?numOscillators then error "expected ring generated by 'oscRing'";
    matrix for i in vertexList list (
        first entries (sine i * diff(cosine i, gens P) - cosine i * diff(sine i, gens P))
        )
    )
oscJacobian(Graph, Ring) := opts -> (G, R) -> (
    verticesG := sort vertices G; -- or should this be R.Graph?
    vertexList := if R.Reduced or opts.Reduced then drop(verticesG, 1) else verticesG;
    if not R.?numOscillators then error "expected ring generated by 'oscRing'";
    sine := (i) -> if member(i, vertexList) then (select(R.generators, k -> toString k == toString (R.Symbols_1)_i))_0 else 0_R;
    cosine := (i) -> if member(i, vertexList) then (select(R.generators, k -> toString k == toString (R.Symbols_0)_i))_0 else 1_R;
    matrix for i in verticesG list for j in verticesG list (
        if i =!= j then (
            if member(j, neighbors(G,i)) then (
                -- entry is cos(theta_j - theta_i)
                cosine j * cosine i + sine j * sine i
            ) else 0_R
        ) else 
          - sum for j in toList neighbors(G, i) list (cosine j * cosine i + sine j * sine i)
        )
)
oscJacobian(Graph) := opts -> G -> oscJacobian(G, oscRing(G, opts), opts)

oscQuadrics = method(Options => {Reduced=>false})
oscQuadrics(Graph, Ring) := opts -> (G, R) -> (
    verticesG := sort vertices G; -- or should this be R.Graph?
    vertexList := if R.Reduced or opts.Reduced then drop(verticesG, 1) else verticesG;
    sine := (i) -> if member(i, vertexList) then (select(R.generators, k -> toString k == toString (R.Symbols_1)_i))_0 else 0_R;
    cosine := (i) -> if member(i, vertexList) then (select(R.generators, k -> toString k == toString (R.Symbols_0)_i))_0 else 1_R;
    ideal for i in verticesG list (
        N := toList neighbors(G,i);
        sum for j in N list (
            (- sine j * cosine i + sine i * cosine j)
            )
        )
    )
oscQuadrics(Graph) := opts -> G -> oscQuadrics(G, oscRing(G), opts)

-- Helper function to return the ideal on cos and sin for n variables.
trig = method(Options => {Reduced=>false})
trig Ring := opts -> (R) -> (
    if not R.?numOscillators then error "expected ring generated by 'oscRing'";
    G := R.Graph;
    verticesG := sort vertices G;
    vertexList := if R.Reduced or opts.Reduced then drop(verticesG, 1) else verticesG;
    sine := (i) -> (select(R.generators, k -> toString k == toString (R.Symbols_1)_i))_0;
    cosine := (i) -> (select(R.generators, k -> toString k == toString (R.Symbols_0)_i))_0;
    ideal for i in vertexList list (sine i)^2+(cosine i)^2-1
    )

oscSystem = method(Options => {Reduced=>false})
oscSystem(Graph, Ring) := opts -> (G, R) -> oscQuadrics(G, R, opts) + trig(R, opts)
oscSystem(Graph) := opts -> G -> oscSystem(G, oscRing(G, opts), opts) 

----------------------------------------------------------------------------------
-- Handy Constructions
----------------------------------------------------------------------------------
ringOscillatorGraph = method()
ringOscillatorGraph(ZZ,ZZ) := (n,k) -> (
    graph unique flatten for i from 0 to n-1 list (
        for j from i-k to i+k list (
            if j == i then continue;
            j1 := if j < 0 then j + n else if j >= n then j-n else j;
            sort {i,j1}
            )
        )
    )
ringOscillatorGraph(ZZ) := (n) -> ringOscillatorGraph(n,1)

vertexSpanningPolynomial = method()
vertexSpanningPolynomial(Graph, Ring) := (G,R) -> (
    cosine := (i) -> (select(R.generators, k -> toString k == toString (R.Symbols_0)_i))_0;
    verticesG := sort vertices G;
    VG := matrix for i in verticesG list for j in verticesG list (
        if i === j then (
            sum for n in toList neighbors(G,i) list cosine n
        ) else if member(j, neighbors(G,i)) then - cosine i else 0
    );
    det submatrix(VG, 1..#vertices G - 1, 1..#verticesG - 1) // cosine (verticesG)_0
)
vertexSpanningPolynomial(Graph) := G -> vertexSpanningPolynomial(G, oscRing(G))

standardSols = method(Options => {Reduced => false})
standardSols(Graph, Ring) := opts -> (G,R) -> (
    verticesG := sort vertices G;
    vertexList := if opts.Reduced or R.Reduced then drop(verticesG, 1) else verticesG;
    sine := (i) -> if member(i, vertexList) then (select(R.generators, k -> toString k == toString (R.Symbols_1)_i))_0 else 0_R;
    cosine := (i) -> if member(i, vertexList) then (select(R.generators, k -> toString k == toString (R.Symbols_0)_i))_0 else 1_R;
    minors(2, matrix for i in verticesG list {sine i, cosine i})
)
standardSols(Graph) := opts -> G -> standardSols(G, oscRing(G, opts), opts)
----------------------------------------------------------------------------------
-- Finding solutions: beware, these might give duplicates and/or miss solutions --
----------------------------------------------------------------------------------
isReal List := (L) -> all(L, p1 -> imaginaryPart p1 == 0)

findRealSolutions = method()
findRealSolutions Ideal := (I) -> (
    -- I is an ideal over the complexes, which is zero-dimensional (might be higher dimensional though...!
    sols := solveSystem I_*;
    bads := positions(sols, p -> p.cache.SolutionStatus != Regular);
    if #bads > 0 then (<< "warning: some solutions are not regular: " << bads << endl);
    coords := matrix(sols/coordinates);
    coords = clean(.00000001, coords);
    select(entries coords, p -> all(p, p1 -> imaginaryPart p1 == 0))
    )
findRealSolutions(Graph) := (G) -> (
    n := # vertices G;
    RC := oscRing(n-1,{}, CoefficientRing => CC);
    IC := trim oscSystem(G,RC);
    JC := oscJacobian(G, RC);
    pts := findRealSolutions IC;
    for p in pts list {p, sub(JC, matrix{p}), identifyStability(JC, p, Tolerance=>1e-10)}
    )

allUniquePrincipalMinors = method(Options => {Modulo=>null})
allUniquePrincipalMinors Matrix := opts -> (M) -> (
    I := if opts.?Modulo and opts.Modulo =!= null then opts.Modulo else 0_(ring M);
    S := drop(subsets numcols M, 1); -- drop the empty subset
    -- I is an ideal in a polynomial ring over e.g. QQ
    -- M is a symmetric n by n matrix over the sam ering as I.
    if ring M =!= ring I then error "expected ideal and matrix over the same ring";
    if M - transpose M != 0 then error "expected a symmetric matrix";
    M1 := M % I;
    unique for s in S list (
        (det submatrix(M1, s, s)) % I
        )
    )

identifyStability = method(Options => {Tolerance => 1e-15})
-- returns one of the symbols Stable, Unstable, Semistable
identifyStability(Matrix, List) := Symbol => opts -> (Jac,pt) -> (
    J0 := substitute(Jac, matrix{pt});
    ev := eigenvalues J0;
    identifyStability(opts, ev)
    )
identifyStability BasicList := Symbol => opts -> eigenvals -> (
    signs := for e in eigenvals list
        if abs(realPart e) < opts.Tolerance then 0 
        else if realPart e < 0 then -1 
        else 1;
    nzero := # select(signs, s -> s === 0);
    npos := # select(signs, s -> s === 1);
    nneg := # select(signs, s -> s === -1);
    assert(nzero + npos + nneg == # eigenvals);
    if npos > 0 then Unstable
    else if nzero == 1 then Stable
    else Semistable
    )
    
isStableSolution = method()
isStableSolution(Matrix, List) := (J,pt) -> (
    J0 := substitute(J, matrix{pt});
    ev := eigenvalues J0;
    all(ev, e -> realPart e < 0.0)
    )

-----------------------------
-- twisted angle points -----
-----------------------------
twisted = method()
twisted(ZZ, ZZ, Ring) := (p, n, R) -> (
    a := cos(p*pi/n);
    c := for i from 1 to n-1 list cos(1_R * i*p*2*pi/n);
    s := for i from 1 to n-1 list sin(1_R * i*p*2*pi/n);
    c | s
    )
twisted(ZZ, ZZ, ZZ) := (p, n, prec) -> twisted(p, n, RR_prec)

----------------------------------------------------------------------------
-- Useful display functions and methods to see results in terms of angles --
----------------------------------------------------------------------------

getAngles = method(Options => {Radians => true})
getAngles(ZZ,List) := opts -> (nangles, sols) -> (
    -- assume 'sols' is a list of
    -- (cos theta_1, cos theta_1, ..., cos theta_nangles, sin theta_1, ..., sin theta_nangles, ...)
    for p in sols list (
        for i from 0 to nangles-1 list (
            --a := atan2(realPart p#(i), realPart p#(i+nangles));
            a := atan2(realPart p#(i+nangles), realPart p#(i));
            b := if a < 0 then a + 2*pi else a;
            if opts.Radians then b else b * 180/pi
            )
        )
    )

getLinearlyStableSolutions = method()
getLinearlyStableSolutions Graph := G -> (
    n := # vertices G;
    RC := oscRing(n, CoefficientRing => CC, Reduced => true);
    IC := trim oscSystem(G,RC);
    JC := oscJacobian(G, RC);
    elapsedTime realsols := findRealSolutions IC;
    stablesols := for pt in realsols list (
        result := identifyStability(JC, pt, Tolerance=>1e-10);
        if result == Stable then pt else continue
        );
    stablesols
    )

showExoticSolutions = method()
showExoticSolutions Graph := G -> (
    stablesols := getLinearlyStableSolutions G;
    n := #vertices G;
    if #stablesols > 1 then (
        << "-- found extra exotic solutions for graph " << G << " --" << endl;
        << netList stablesols << endl;
        << "-- angles (in degrees), first angle is zero and omitted --" << endl;
        << netList (getAngles(n-1, stablesols, Radians=>false)) << endl;
        );
    stablesols
    )

-- for compatibility with the paper
getExoticSolutions = method()
getExoticSolutions Graph := G -> showExoticSolutions G
