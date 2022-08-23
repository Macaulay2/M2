needsPackage "MonodromySolver"

-- several convenience functions before running test

--fold objects with a "||" method in a list (ie matrices along rows)
foldVert = method()
foldVert List := L -> if (#L == 0) then L else fold(L,(a,b)->a||b)

-- determinant gates
det2 = M -> M_(0,0)*M_(1,1)-M_(1,0)*M_(0,1)
det3 = M -> M_(0,0)*det2(M^{1,2}_{1,2})-M_(0,1)*det2(M^{1,2}_{0,2})+M_(0,2)*det2(M^{1,2}_{0,1})
laplaceDet = M -> (
    (m, n) := (numrows M, numcols M);
    assert(m==n);
    if (m==2) then det2 M else if (m==3) then det3 M else error "not implemented"
    )
-- convenience functions for minors
minors (GateMatrix, ZZ, Sequence, Boolean) := o ->  (M, k, S, laplace) -> (
    (Sm, Sn) := (first S, last S);
    (m, n) := (numrows M, numcols M);
    assert(k<=min(m,n));
    assert(all(Sm,s->#s==k));
    assert(all(Sn,s->#s==k));
    gateMatrix{flatten apply(Sm,sm->apply(Sn, sn -> 
	    if (laplace) then laplaceDet submatrix(M,sm,sn)
	    else det submatrix(M,sm,sn)
	    ))}
    )
allMinors = method(Options=>{Laplace=>false})
allMinors (GateMatrix, ZZ) := o -> (M, k) -> (
    (m, n ) := (numrows M, numcols M);
    s := (subsets(0..m-1,k),subsets(0..n-1,k));
    minors(M, k, s, o.Laplace)
    )

maxMinors = method(Options=>{})
maxMinors GateMatrix := o -> M -> (
    r := min(numrows M, numcols M);
    useLaplace := (r < 4);
    allMinors(M,r, Laplace=> useLaplace)
    )
-- "join" of two GateSystems (take all functions from both)
GateSystem || GateSystem := (P, Q) -> (
    allVars := unique( (flatten entries vars P) | (flatten entries vars Q) );
    allParams := unique( (flatten entries parameters P) | (flatten entries parameters Q) );
    gateSystem(
	gateMatrix{allParams},
	gateMatrix{allVars},
    	(gateMatrix P)||(gateMatrix Q)
	)
    )

evaluate (GateMatrix, GateMatrix, AbstractPoint) := (GM, varMat, xVals) -> (
    Peval := gateSystem(varMat, transpose gateMatrix{flatten entries GM});
    result := evaluate(Peval, xVals);
    matrix(result, numrows GM, numcols GM)
    )
evaluate (GateMatrix, VisibleList, AbstractPoint) := (GM, xVars, xVals) -> evaluate(GM, gateMatrix{xVars}, xVals)
evaluate (GateMatrix, VisibleList, Matrix) := (GM, xVars, xVals) -> (
    I := if instance(xVars, GateMatrix) then xVars else gateMatrix{toList xVars};
    evaluate(GM, I, point xVals)
    )
evaluate (Matrix, Thing, Thing) := (M, thing1, thing2) -> evaluate(gateMatrix M, thing1, thing2)

-- take some functions from the GateMatrix \ GateSystem
GateMatrix ^ BasicList := (M, inds) -> M^(toList inds)
GateSystem ^ BasicList := (P, inds) -> gateSystem(parameters P, vars P, (gateMatrix P)^inds)

gateSystem (BasicList, BasicList, BasicList) := (P, X, Fs) -> foldVert apply(toList Fs, f -> gateSystem(P, X, gateMatrix f))

-- example starts here

xs=vars(x_1..x_14)

-- camera matrices for the tree views
P1=id_(QQ^3)|matrix{{0},{0},{0}}
P2=matrix{
    {x_1,1,0,-1},
    {0,x_2,x_3,-x_3},
    {x_4,x_5,x_6,x_7}
    }
P3 = matrix{
    {x_8,x_9,0,-x_9},
    {0,x_10,1,-1},
    {x_11,x_12,x_13,x_14}
    }
Ps = {gateMatrix P1,P2,P3}

-- parameters encoding the 9 lines in 3 views (2 are fixed)
lineParameters = vars(l_(2,0,0)..l_(8,2,2))


-- each of the 9 triples of lines gives determinantal constraints on cameras
rankDropMats = apply(2..8, i -> foldVert apply(3, j -> matrix{{l_(i,j,0),l_(i,j,1),l_(i,j,2)}} * Ps#j))
rankDropConstraints = rankDropMats/maxMinors;

fabricatePair = () -> (
    x0 := point random(CC^1,CC^14);
    cams0 := Ps/(m->evaluate(m, vars G, x0));
    worldLines := for i from 2 to 8 list random(CC^4,CC^2);
    p0 := point foldVert apply(worldLines, wL -> foldVert apply(cams0, c0 -> numericalKernel(transpose(c0*wL), 1e-5)));
    (p0, x0)
    )
errorDepth = 0
G = gateSystem(lineParameters, xs, rankDropConstraints)
(p0, x0) = fabricatePair()

pivotIndices = flatten for i from 0 to 6 list {4*i+2,4*i+3}
GSq=G^pivotIndices

V = first monodromySolve(GSq, p0, {x0}, Verbose => true)
assert(length V.PartialSols == 36)

end--

restart
setRandomSeed 0
needs "larrson.m2"
