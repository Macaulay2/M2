-- produces Faddeev-Leverriere circuits for computing the characteristic polynomial of a matrix
-- warning: this algorithm is probably not sensible over InexactFields due to numerical instability

recursionLimit = 5000

needsPackage "SLPexpressions"

-- List -> divide-conquer circuit for summing its contents
dcSum = L -> if (#L == 0) then 0 else if (#L == 1) then L#0 else (
    mid := floor(#L/2);
    dcSum(take(L,mid)) + dcSum(drop(L,mid))
    )

tr = method(Options=>{SumStrategy=>"Iterative"})
tr GateMatrix := o -> M -> (
    n := numcols M;
    assert(numrows M == n);
    diag := for i from 0 to n-1 list M_(i,i);
    if (o.SumStrategy == "Iterative") then sum diag else dcSum diag
    )

cpoly = method(Options=>{SumStrategy=>"Iterative"})
cpoly GateMatrix := o -> M -> (
    n := numcols M;
    assert(numrows M == n);
    coefs := new MutableList from {};
    auxMats := new MutableList from {};
    coefs#0 = 1_FF;
    auxMats#0 = gateMatrix matrix mutableMatrix(FF, n, n); -- cleaner syntax?
    I := gateMatrix id_(FF^n);
    tmp := M*auxMats#0;
    for k from 1 to n do (
	auxMats#k = tmp + coefs#(k-1)*I;
	tmp = M*auxMats#k;
	coefs#k = (-1/k) * tr(tmp, o);
	);
    gateMatrix{reverse toList coefs}
    )

end--
restart
FF = ZZ/101
needs "charPoly.m2"

n=20
A = matrix (for i from 1 to n list for j from 1 to n list declareVariable M_(i,j)) -- generic matrix

-- build circuit for characteristic polynomial
--elapsedTime p = cpoly M;
-- 12.3016 seconds elapsed
-- M2-binary now consumes about ~2.8 g

elapsedTime p = cpoly(A,SumStrategy=>"DivideConquer");
--depth p
elapsedTime q = compress p;
elapsedTime Ep= makeSLProgram(A, p);
elapsedTime Eq= makeSLProgram(A, q);

--M0 = matrix(FF,{{3,1,5},{3,3,1},{4,6,4}}); -- 3x3 test
A0 = random(FF^n,FF^n)
elapsedTime evaluate(Ep,A0);
elapsedTime evaluate(Eq,A0);









