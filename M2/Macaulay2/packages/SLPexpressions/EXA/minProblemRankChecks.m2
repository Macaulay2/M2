FF=ZZ/nextPrime 2019

debug needsPackage "SLPexpressions"

size GateMatrix := M -> (numrows M, numcols M)

-- evaluate a gateMatrix G at a matrix x (use sparingly)
evaluate (GateMatrix, Matrix) := (G, x) -> (
    M := mutableMatrix(FF,numrows G,numcols G);
    E' := makeEvaluator(G,matrix{varGates|paramGates});
    evaluate(E',mutableMatrix(x),M);
    matrix M
    )

-- 2x2 determinant
det2 = M -> M_(0,0)*M_(1,1)-M_(1,0)*M_(0,1)

-- Plücker vector for a 3*2 matrix
pl3 = M -> matrix{{det2 M^{1,2},det2 M^{2,0},det2 M^{0,1}}}

installedGates = {}

resetGates = () -> (
    undeclareVariable \ installedGates;
    installedGates = declareVariable \ {x_2,y_2,z_2,x_3,y_3,z_3,t_(2,1),t_(2,2),t_(3,1),t_(3,2),t_(3,3)};
    )
	
installGates = G -> (
    newG := declareVariable \ G;
    installedGates = installedGates | newG;
    newG
    )

cay2R = method(Options=>{Normalized=>true})
cay2R (Thing,Thing,Thing) := o -> (x,y,z) -> (
    M := matrix{
    {1+x*x-(y*y+z*z), 2*(x*y-z), 2*(x*z+y)},
    {2*(x*y+z), 1+y^2-(x*x+z*z), 2*(y*z-x)},
    {2*(x*z-y), 2*(y*z+x), 1 +z*z -(x*x+y*y)}
	};
    if o.Normalized then (oneGate/(1+x^2+y^2+z^2)) * M else M
    )
cay2R List := o -> L -> cay2R(L#0, L#1, L#2, o)


P2chart0 = M -> (oneGate/M_(0,0))*M^{1,2}
P2chart1 = M -> (oneGate/M_(1,0))*M^{0,2}
P2chart2 = M -> (oneGate/M_(2,0))*M^{0,1}

getLines = pl1p -> first pl1p
getViews = pl1p -> last pl1p
getVisLines = view -> first view
getVisPoints = view -> last view

getNumPoints = pl1p -> # getVisPoints first getViews pl1p
getNumLines = pl1p -> # getVisLines first getViews pl1p
checkPL1P = pl1p -> (
    ls := getLines pl1p;
    vs := getViews pl1p;
    n := getNumPoints pl1p;
    assert all(vs, v-># getVisPoints v == n);
    assert all(ls, l->instance(l,ZZ) and l>=-1 and l<n);
    -* UNCHECKED AT THE MOMENT:  
    the lines are SORTED (free come before pins)
    BALANCEDness
    ADMISSIBILITY of visibility pattern  
    *-
    )

resetGates()

cameras = {
    gateMatrix(id_(FF^3)|matrix{{0},{0},{0}}),
    cay2R(x_2,y_2,z_2)|matrix{{t_(2,1)},{t_(2,2)},{1}},
    cay2R(x_3,y_3,z_3)|matrix{{t_(3,1)},{t_(3,2)},{t_(3,3)}}
    }
    

parametrizeWorld = pl1p -> (
    resetGates();
    n := getNumPoints pl1p;
    worldPoints := apply(n, i->transpose matrix{installGates{X_{i,0},X_{i,1},X_{i,2}} | {oneGate}});         
    ls := getLines pl1p;
    worldLines := apply(#ls, i->
	(if (ls#i<0) then -- point A on the line 
	transpose matrix{installGates{A_{i,0},A_{i,1}} | {zeroGate, oneGate}}
	else worldPoints#(ls#i)) | transpose matrix{installGates{B_{i,0},B_{i,1}} | {oneGate, zeroGate}} 
	);      	  
    (worldPoints,worldLines)
    )


makePhi = method()
-* IN : description of PL1P, a list of GateMatrices for cameras
   OUT: GateMatrix for the forward map Phi

parameters of the domain (transpose each matrix below):    
  [*,*,*,1] for each world point
  [[*,*,1,0],
   [*,*,0,1]] for each free line (perceived as two points on the line)
  [*,*,1,0] for each pin (perceived as a point on the pin)
*-      
makePhi (List, List) := (pl1p,cameras) -> (
    -- we can make the next few lines marginally more efficient
    (worldPoints,worldLines) := parametrizeWorld pl1p;
    allLines := getLines pl1p;
    v := getViews pl1p;
    (n, l) := (getNumPoints pl1p, getNumLines pl1p);
    firstPin := position(getLines pl1p|{0}, i -> i > -1);
    phi := new MutableList from {}; -- list of (lists of) Gates (coordinates in some chart on the codomain) 
    lastPhi := 0;
    scan(#cameras, j->(
	c := cameras#j;
	(ls, ps) := (v#j#0, v#j#1); -- visibility pattern for camera j
	for i from 0 to firstPin-1 do if (ls#i==1) then (	    
	    projLine := c*worldLines#i;
	    --<< "size of free line is " << size projLine << endl;
	    phi#lastPhi = flatten entries P2chart0 transpose pl3 projLine;
	    lastPhi = lastPhi + 1;
	    );
	for i from firstPin to l-1 do if (ls#i==1) then (	    
	    projLine := c*worldLines#i;
	    --<< "size of pinned line is " << size projLine << endl;
	    phi#lastPhi = if ps#(allLines#i)==1 
	    then (P2chart0 transpose pl3 projLine)_(0,0)-- grab one of the coordinates
	    else flatten entries P2chart0 transpose pl3 projLine; -- grab two: this pin is a free line
	    lastPhi = lastPhi + 1;
	    );
	for i from 0 to n-1 do if (ps#i==1) then (
	    projPoint := c*worldPoints#i;
	    --<< "size of image point is " << size projPoint << endl;
	    phi#lastPhi = flatten entries P2chart2 projPoint;
	    lastPhi = lastPhi + 1;
	    );
	));
    matrix{flatten toList phi}
    )
rankCheck = method()    
rankCheck List := pl1p -> (
    Phi := makePhi(pl1p,cameras);
    inGates := matrix {toList installedGates};
    J := diff(transpose inGates, Phi);
    -*
    E := makeSLProgram(inGates,J); -- this is potential trouble
    Mout := mutableMatrix(FF,numrows J,numcols J);
    evaluate(E,mutableMatrix random(FF^(#installedGates),FF^1),Mout);
    r := rank matrix Mout;
    << "minimal is " << (#installedGates == r) << endl;
    << "inputs, outputs, rank of Jacobian" << endl;
    append(size J,r)
    *-
    )    
end--

restart
load "minProblemRankChecks.m2"
load "99problems.m2"

--i = 98
--pl1p := PROBLEMS#i#"pl1p"; --prob#"pl1p";
elapsedTime scan(
    #PROBLEMS, 
    --100,
    i->(
    pl1p := PROBLEMS#i#"pl1p"; --prob#"pl1p";
    -- resetGates(); -- does not leak anymore (after declareVariable introduction)  
    if i % 10 == 0 then << "on problem number " << i << endl;
    Phi := makePhi(pl1p,cameras);
    inGates := transpose matrix {installedGates};
    J := diff(inGates, Phi); -- leaks (RES=171m, memoized diff helps to reduce memory consumption) 
    J = compress J; -- leaks more (RES=183m, also slow)  
    E := makeSLProgram(inGates,J); --leaks more (RES=171m with no compress; RES=199m with)
    --print E;
    collectGarbage()
    ))








