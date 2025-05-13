-- Test 0
TEST /// 
  -- A simple example to make sure this is working
-*
  restart
  needsPackage "Oscillators"
*-
  G = graph ({2, 0, 1, 3}, {{0, 2}, {1, 2}, {2, 3}, {0, 3}, {1, 3}})

  S = oscRing(G, Reduced => true)
  assert(numgens S == 6) -- basically: we set theta_0 to 0.

  IG = oscSystem(G,S)

  -- TODO: fix oscRing,oscSystem to handle reduction of any specific vertex.
  -- Ians = ideal(-y_0-y_1-y_3,x_3*y_0-x_0*y_3+y_0,x_3*y_1-x_1*y_3+y_1,-x_3*y_0-x_3*y_1+x_0*y_3+x_1*y_3+y_3,x_0^2+y_0^2-1,x_1^2+y_1^2-1,x_3^2+y_3^2-1)
  -- assert(IG == Ians)
  -- assert(gens IG == gens Ians) -- checking that we did not remove any of the equations.
  -- end of todo section
  
  assert(dim IG == 1)
  
  -- now let's make sure the Jacobian(s) of this system are OK.
  Jac = oscJacobian(G,S)
  assert(det Jac == 0)
  assert(rank Jac == 3)
  assert((numrows Jac, numcols Jac) == (4,4))
///

-- Test 1
TEST ///
  -- The square
-*
  restart
  needsPackage "Oscillators"
*-
  G = graph( {0,1,2,3}, {{0,1}, {1,2}, {2,3}, {0,3}})

  S = oscRing(G, Reduced => true)
  assert(numgens S == 6) -- basically: we set theta_0 to 0.

  IG = oscSystem(G,S)
  Ians = ideal(-y_1-y_3,x_2*y_1-x_1*y_2+y_1,-x_2*y_1+x_1*y_2+x_3*y_2-x_2*y_3,-x_3*y_2+x_2*y_3+y_3,x_1^2+y_1^2-1,x_2^2+y_2^2-1,x_3^2+y_3^2-1)
  assert(IG == Ians)
  assert(gens IG == gens Ians) -- checking that we did not remove any of the equations.
  
  assert(dim IG == 1)
  
  -- now let's make sure the Jacobian(s) of this system are OK.
  Jac = oscJacobian(G,S)
  assert(det Jac == 0)
  assert(rank Jac == 3)
  assert((numrows Jac, numcols Jac) == (4,4))
  
  --numericalIrreducibleDecomposition IG
///

-- Test 2
TEST /// 
-*
  restart
  needsPackage "Oscillators"
*-
  -- The triangle
  G = graph({0,1,2},{{0,1},{1,2},{0,2}})
  R = oscRing(G, CoefficientRing => QQ, Reduced => true)
  I = oscSystem(G,R);
  I = ideal gens gb trim I
      C = decompose I
      standardI = ideal(y_1, y_2, x_1^2-1, x_2^2-1)
      I1 = I : standardI
      I2 = I : I1
      assert(I == intersect(I1, I2))
      assert(I2 == standardI)
      decompose I1
      assert(I == radical I)
      assert(dim I == 0)
      assert(#C == 5) -- 4 standard plus 1 more (which has 2 solutions)
    J = oscJacobian(G,R)
    Jacs = for i in C list J % i
    EVs = for j in Jacs list eigenvalues(lift(j, QQ)) -- self-synch solution is stable, all others are unstable.
    cleanedEVs = EVs/(evs -> evs/(x -> clean(1e-10, x)))
    stable = positions(cleanedEVs, x -> Stable == identifyStability x )
    semistable = positions(cleanedEVs, x -> Semistable == identifyStability x)    
    unstable = positions(cleanedEVs, x -> Unstable == identifyStability x)
    assert(#semistable == 0)
    assert(#unstable == 4)
    assert(#stable == 1)

    unstablesols = select(findRealSolutions I, p -> Unstable === identifyStability(J,p))
    tally ((findRealSolutions I)/(p -> identifyStability(J, p)))
    getAngles(2, unstablesols, Radians=>false)
    
    -- note: the 2 nonstandard solutiona are twisted solutions:
    -- theta_0 = 0, theta_1 = 2 pi/3, theta_2 = 4 pi/3
    -- theta_0 = 0, theta_1 = 4 pi/3, theta_2 = 8 pi/3
    -- upshot: ideal is radical zero-dim, 4 standard solutions, 2 twisted solutions (unstable), SS
///

-- Test 3
TEST ///
  -- analyze stability of all solutions given a graph G
  G = graph({0,1,2,3},{{0,1},{1,2},{2,3},{0,3}})
  S = oscRing(G, CoefficientRing => QQ, Reduced => true)
  I = oscSystem(G,S);
  netList I_*
  dim I
  I == radical I
  C = decompose I;
  assert(#C == 5)
  C = C/trim;
  netList C
  C/dim
  
  J = oscJacobian(G,S)
  netList for i in C list allUniquePrincipalMinors(-J, Modulo=>i)
  -- by looking at each one, all points but one are unstable.
///

--Test 4
TEST ///
  needsPackage "NautyGraphs"
  Gstrs = flatten for i from 4 to 9 list generateGraphs(5,i, OnlyConnected=>true)
  Gs = Gstrs/stringToGraph
  assert(#Gs == 20)
  Gs = select(Gs, G -> all(vertices G, v -> degree(G,v) > 1))
  assert(#Gs == 10)  
  
  -- now let's compute these numerically:
  -- note: this may completely miss the points on
  --  higher dimensional components.  I'm not sure,
  --  but for singular isolated points, I think it
  --  will find all of those, maybe with higher multiplicity.
  n = 5
  realsolsGs = for G in Gs list (
      RC = oscRing(G, CoefficientRing => CC, Reduced => true);
      IC = oscSystem(G,RC);
      JC = oscJacobian(G, RC, Reduced => true);
      elapsedTime realsols = findRealSolutions IC;
      realsols = realsols/(pt -> prepend(identifyStability(JC, pt), pt));
      stablesols = select(realsols, x -> x#0 === Stable);
      (stablesols, realsols)
      )
///

-- Test 5
TEST ///
  needsPackage "Oscillators"
  needsPackage "NautyGraphs"
  n = 5;
  R = oscRing (n, Reduced => true);

  Gstrs = generateGraphs(n, OnlyConnected => true, MinDegree => 2);  
  assert(#Gstrs == 11)
  G5s = Gstrs/stringToGraph;
  assert(#G5s == 11)

  I0 = oscQuadrics(G5s_0, R)
  -- TODO: the order of generators is different since we are sorting vertices.
  --  change the code to allow setting any oscillator to 0.
  -- ans0 = ideal(-x_2*y_4+x_4*y_2+y_2,-y_2-y_3,-x_3*y_1+x_1*y_3+y_3,-x_1*y_4+x_4*y_1+x_3*y_1-x_1*y_3,x_1*y_4+x_2*y_4-x_4*y_1-x_4*y_2)
  -- assert(gens I0 ==  gens ans0)
  
  I0 = oscQuadrics(G5s_4, R)
  Seg = standardSols(G5s_4, R)
  I1 = I0 : Seg
  trim(I1 + Seg)
  (codim oo, degree oo)
  
  for G in G5s list G => getLinearlyStableSolutions G
  pt = first getLinearlyStableSolutions G5s_0
  pt1 = {1.0} | pt_{0..3} | {0.0} | pt_{4..7}
  RC = oscRing(n);
  I = oscQuadrics(G5s_0, RC)
  sub(I, matrix {pt1})
  comps = decompose I
  netList comps
  comps/(i -> sub(i, matrix{pt1}))  
  dim comps_1
  oscSystem(G5s_0, R)     
///

-- Test 6
TEST ///
  --Let's do all connected graphs with 4 vertices, which do not have a vertex with degree 1
    needsPackage "Oscillators"
    needsPackage "NautyGraphs"
    needsPackage "Visualize"

    n = 4
    -- There are 6 connected graphs:
    Gstrs = flatten for i from 3 to 6 list generateGraphs(n,i, OnlyConnected=>true)
    Gs = Gstrs/stringToGraph
    assert(#Gs == 6)
    -- the first 2 are trees:
    assert(Gs/isTree == splice{2:true, 4:false})
    Gs = select(Gs, G -> all(vertices G, i -> degree(G,i) > 1))
    assert(#Gs == 3)
    -- there are (at least) two ways to view graphs in Macaulay2.  The first 
    -- just pops up a jpeg picture using graphviz (which you must have
    -- installed on your system):
    -- displayGraph Gs_0
    -- Gs/displayGraph

    -- The next lines are for viewing graphs in your browser.
    -- This generally gives a nicer picture than graphviz, and you can change the look,
    -- and output tikz code too.  However, you need to end the session in the browser
    -- in order to move on to the next graph.
    -- SS: self synchro (only one stable real solution)
    -- POS: has positive dimensional components
    -- NONRAD: is not reduced
    -- NONREG: some solutions are not regular (from homotopy continuation)
    --openPort "8080"
    --visualize Gs_0 -- square NONREG SS POS
    --visualize Gs_1 -- square with one diagonal NONREG SS NONRAD POS
    --visualize Gs_2 -- complete graph NONREG SS NONRAD POS
    --closePort()

    realsolsGs = for G in Gs list (
        RC = oscRing(G, CoefficientRing => CC, Reduced => true);
        IC = oscSystem(G,RC);
        JC = oscJacobian(G, RC);
        elapsedTime realsols = findRealSolutions(IC);
        realsols = realsols/(pt -> prepend(identifyStability(JC, pt), pt));
        stablesols = select(realsols, x -> x#0 === Stable);
        (stablesols, realsols)
        )

    netList realsolsGs_0_1 -- one stable, 4 semistable (Jacobian == 0 at these), all rest unstable
    netList realsolsGs_1_1 -- 6 standard solutions, 6 others.
    netList realsolsGs_2_1 -- only standard solutions, yet some of the unstable ones must be singular
                             -- since some solutions appear more than once.


    realsolsGsQQ = for G in Gs list (
        R = oscRing(G, CoefficientRing => QQ, Reduced => true);
        I = oscSystem(G,R);
        J = oscJacobian(G,R);
        C = decompose I;
        C = select(C, i -> dim i == 0);
        C = C/(i -> sub(i, R));
        realsols = C/findRealSolutions//flatten;
        stablesols = select(realsols, p -> isStableSolution(J, p));
        {stablesols, realsols}
        )

    netList realsolsGsQQ_0_1 -- this one has 2 isolated solutions, both with sin theta_i = 0. (all i)
    netList realsolsGsQQ_1_1 -- this one has 4 isolated solutions, all with sin theta_i = 0
    netList realsolsGsQQ_2_1 -- this one has 5 isolated solutions, all with sin theta_i = 0

    Is = for G in Gs list (
        I = oscSystem(G,R);
        J = oscJacobian(G,R);
        C = decompose I;
        (I, C, J)
        )

    Is/first/dim
    Cs = Is/first/primaryDecomposition
    select(Cs_0, i -> dim i == 0 and i == radical i)
    select(Cs_0, i -> dim i == 0 and i != radical i)

    select(Cs_1, i -> dim i == 0 and i == radical i)
    select(Cs_1, i -> dim i == 0 and i != radical i)

    select(Cs_2, i -> dim i == 0 and i == radical i)
    select(Cs_2, i -> dim i == 0 and i != radical i)

    Is/(x -> x#1)/length

    -- let's consider the higher dimensional components, and radical-ness of ideals
    for i in Is/first list  radical i == i
    Is/first/dim -- {0,0,0,1,1,1}
    -- Notes:
    -- (1) the first 3 graphs have edges sticking out.
    -- (2) the first 3 graphs have radical ideals, of dim 0.
    -- We now describe the features of each example.
    --
    ---------------------
    -- Graph Gs_0
    -- square
    G = graph ({0, 1, 2, 3}, {{0, 1}, {1,2}, {2, 3}, {0,3}})
    R = oscRing(G, CoefficientRing => QQ, Reduced => true)
    I = oscSystem(G,R)
    C = decompose I
    trim first Is_0 -- 8 solutions: all sin=0, cos=+-1.
    assert(I == radical I) -- true
    C = decompose I
    J = oscJacobian(G,R)
    Jacs = for i in C list J % i
    --for j in Jacs list eigenvalues(lift(j, QQ))
      -- upshot: this is a tree, ideal is a reduced set of the 8 "standard" points.
      --   self-synch pt: yes
      --   other 7 are unstable.
    ---------------------
    -- Graph Gs_1
    -- square with one diagonal
    -- sort vertices Gs_1, (edges Gs_1)/toList/sort//sort
    G = graph ({0, 1, 2, 3}, {{0, 2}, {0, 3}, {1, 2}, {1, 3}, {2, 3}})
    I = oscSystem(G,R);
    C = decompose I
    PD = primaryDecomposition I
    gbI = ideal gens gb trim I
    J = oscJacobian(G,R)
    Jacs = for i in C list J % i
    --for j in Jacs list eigenvalues(lift(j, QQ))
      -- upshot: this is a tree, ideal is a reduced set of the 8 "standard" points.
      --   self-synch pt: yes
      --   other 7 are unstable.
    ---------------------
    -- Graph Gs_2
    -- complete graph on 4 vertices
    -- sort vertices Gs_2, (edges Gs_2)/toList/sort//sort
    G = graph ({0, 1, 2, 3}, {{0, 1}, {0, 2}, {0, 3}, {1, 2}, {1, 3}, {2, 3}})
    I = oscSystem(G,R);
    C = decompose I
    C/dim
    #C
    PD = primaryDecomposition I
    #PD
    PD/dim
    PD/(i -> i == radical i)
    J = oscJacobian(G,R)
    assert(#C == 8) -- 3 of dim 1, 5 standard points
    for i in C list allUniquePrincipalMinors(-J, Modulo => i)
    C0 = select(C, i -> dim i == 0)
    C1 = select(C, i -> dim i >  0)
    Jacs0 = for i in C0 list J % i
    for j in Jacs0 list eigenvalues(lift(j, QQ)) -- one stable, the other 4 are unstable.
    trim(C1_0 + C1_1) -- a standard point
    trim(C1_0 + C1_2) -- a different standard point
    trim(C1_1 + C1_2) -- a different standard point
    assert(1 == trim(C1_0 + C1_1 + C1_2)) -- don't intersect
    -- geometry: there are 8 components, all reduced.
    --  have 3 curve solutions C0, C1, C2: each consisting solely of unstable points
    --  have 5 standard isolated solutions, one of which is singular.
    --  4 are unstable, 1 is stable.
    -- the pairwise intersection of 2 of the curves is a single (unstable standard point) (all different).
    -- the three curves do not have a common intersection.
    -- there are 3 embedded points (the intersection points of the curves), all standard.
    -- all 8 standard points occur as associated primes.
///

-- Test 7
TEST ///
-- Test: oscJacobian and reduced version return the same eigenvalues, except for
--  one extra eigenvalue of 0.0 for the non-reduced form.
-- XXX
  restart
    needsPackage "Oscillators"
    needsPackage "NautyGraphs"

    n = 4
    -- There are 6 connected graphs:
    Gstrs = flatten for i from 3 to 6 list generateGraphs(n,i, OnlyConnected=>true)
    Gs = Gstrs/stringToGraph

    G = Gs_4
    RC = oscRing(3, {}, CoefficientRing => CC)
    J1 = oscJacobian(G,RC,Reduced=>false)
    J2 = oscJacobian(G,RC)

    PC = oscSystem(G,RC);
    IC = PC + trig RC;
    elapsedTime realsols = findRealSolutions IC;
    netList realsols    
    evs = for p in realsols list map(CC, RC, p)
    evsJ1 = for phi in evs list eigenvalues phi J1 -- I think J1 is not correct..
    evsJ2 = for phi in evs list eigenvalues phi J2
///

--Test 8
TEST ///
  --Let's do all connected graphs with 5 vertices -- YYY
  restart
    needsPackage "Oscillators"
    needsPackage "NautyGraphs"
    needsPackage "Visualize"

    n = 5
    Gstrs = flatten for i from 4 to 10 list generateGraphs(n,i, OnlyConnected=>true)
    Gs = Gstrs/stringToGraph
    assert(#Gs == 21)
    -- openPort "8080"
    -- visualize Gs_3
    -- closePort()

    RC = oscRing(n-1,{}, CoefficientRing => CC)
    R = oscRing(n-1,{}, CoefficientRing => QQ)
    realsolsGs = for G in Gs list (
        PC = oscSystem(G,RC);
        JC = oscJacobian(G, RC);
        IC = PC + trig RC;
        elapsedTime realsols = findRealSolutions IC;
        stablesols = select(realsols, p -> isStableSolution(JC,p));
        (stablesols, realsols)
        )
    netList realsolsGs
    realsolsGs/(x -> (#x#0, #x#1))//netList 

    realsolsGsQQ = for G in drop(Gs,-1) list (
        << "doing " << G << endl;
        P = oscSystem(G,R);
        JC = oscJacobian(G, RC);
        J = oscJacobian(G,R);
        I = P + trig R;
        C = elapsedTime decompose I;
        C = select(C, i -> dim i == 0);
        C = C/(i -> sub(i, RC));
        elapsedTime realsols = C/findRealSolutions//flatten;
        stablesols = select(realsols, p -> isStableSolution(JC, p));
        << "stablesols = " << stablesols << endl << " #real sols = " << #realsols << endl;
        {stablesols, realsols}
        )
    netList realsolsGsQQ
    realsolsGsQQ/(x -> (#x#0, #x#1))//netList 

    -- the only one with more than one stable (isolated) solution is the pentagon ring.
    G = Gs_7
    -- openPort "8081"
    -- visualize G
    -- closePort()
    
    dims = for G in drop(Gs,-1) list (
        << "doing " << G << endl;
        P = oscSystem(G,R);
        I = P + trig R;
        C = elapsedTime decompose I;
        G => tally (C/dim)
        )
    positions(dims, x -> x#1#?1 or x#1#?2)
    -- openPort "8081"
    -- visualize dims#4#0
    -- visualize dims#8#0
    -- visualize dims#9#0
    -- visualize dims#12#0
    -- visualize dims#13#0
    -- visualize dims#15#0
    -- visualize dims#16#0
    -- visualize dims#17#0
    -- visualize dims#19#0
    
    -- visualize dims#5#0
    -- visualize dims#6#0    
    -- visualize dims#7#0    
    -- visualize dims#10#0    
    -- visualize dims#11#0    
    -- visualize dims#14#0    
    -- closePort()
    
///

-- Test 9
TEST ///
    gbTrace=0
    needsPackage "NautyGraphs"
    needsPackage "Visualize"

    n = 6
    Gstrs = flatten for i from n-1 to binomial(n,2)-1 list generateGraphs(n,i, OnlyConnected=>true)
    Gcomplete = stringToGraph first generateGraphs(n,binomial(n,2), OnlyConnected=>true)
    assert(#Gstrs == 111)
    Gs = Gstrs/stringToGraph;

    getSols = (G) -> (
        << "---- doing graph " << G << endl;
        n := # vertices G;
        RC = oscRing(n, CoefficientRing => CC, Reduced => true);
        PC = oscSystem(G,RC);
        JC = oscJacobian(G, RC);
        IC = PC + trig RC;
        elapsedTime realsols = findRealSolutions IC;
        stablesols = select(realsols, p -> isStableSolution(JC,p));
        << netList stablesols << endl;
        (stablesols, realsols)
        )
    getSols Gs_0 -- one sol

    G = Gs_21
    R = oscRing(n, CoefficientRing => QQ, Reduced => true)
    P = oscSystem(G,R);
    J = oscJacobian(G,R);
    I = P + trig R;
    dim I
    degree I
    C0 = select(decompose I, i -> dim i == 0)
    C = primaryDecomposition I
    for i in C0 list lift(J % i, QQ)
    for i in C0 list lift((vars ring i)%i, QQ)
    Isat = saturate(I, product(5, i -> y_(i+1)))
    I0 = I : Isat
    I == intersect(I0, Isat)
///

--Test 10
TEST ///
-- MES XXX
    needsPackage "NautyGraphs"
    needsPackage "Visualize"
    n = 7
    Gstrs = flatten for i from n-1 to binomial(n,2)-1 list generateGraphs(n,i, OnlyConnected=>true)
    Gs = Gstrs/stringToGraph;
    #Gs
    assert(#Gs == 852)
    positions(Gs, isTree) -- the first 11 are trees, that is it...

    getSols = (G) -> (
        << "---- doing graph " << G << endl;
        n := # vertices G;
        RC = oscRing(n, CoefficientRing => CC, Reduced => true);
        PC = oscSystem(G,RC);
        JC = oscJacobian(G, RC);
        IC = PC + trig RC;
        elapsedTime realsols = findRealSolutions IC;
        stablesols = select(realsols, p -> isStableSolution(JC,p));
        << netList stablesols << endl;
        (stablesols, realsols)
      )

    -- example: Gs_11 (one triangle, with edges sticking out)
    G = Gs_11
    R = oscRing(n, CoefficientRing => QQ, Reduced => true)
    P = oscSystem(G,R);
    J = oscJacobian(G,R);
    I = P + trig R;
    dim I
    C = decompose I
    for i in C list ((vars R) % i)
    assert(I == intersect C) -- true, so reduced, zero dimensional
    realroots = flatten for i in C list findRealSolutions i -- 96 real points.  Interesting, since degree is 96...
    RC = oscRing(n, CoefficientRing => CC, Reduced => true)
    JC = oscJacobian(G,RC);
    realroots/(p -> isStableSolution(JC,p))
    -- upshot: 96 roots, all real, exactly one is stable.
    realroots/(p -> (map(CC,RC,p)) JC)
    oo/eigenvalues

    -- example: Gs_500 (planar symmetric triangulation of a triangle)
    G = Gs_500
    R = oscRing(n, CoefficientRing => ZZ/32003, Reduced => true)
    P = oscSystem(G,R);
    J = oscJacobian(G,R);
    I = P + trig R; -- appears to have dim 0, degree 232 (these are OK in char 32003).
///
