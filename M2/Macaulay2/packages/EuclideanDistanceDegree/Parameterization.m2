rand := () -> random QQ;

parameterOptions = { UseMonodromy => false }
parameterizedWeightEDDegree = method(Options => parameterOptions)
parameterizedWeightEDDegree(List, List, List) := o -> (F, U, W) -> (
    -- print("U: " | toString U | " W: " | toString W);
    R := ring first F;
    coef := coefficientRing R;
    numX := #gens R;
    n := #F;

    S := R ** coef[apply(n - numX, i->"L"|i)];
    xList := flatten entries basis({1,0}, S);
    lamList := flatten entries basis({0,1}, S);
    M := sub(matrix{F}, S);

    -- Find a spanning set for ker(jacM)
    jacM := jacobian M;
    if rank jacM != numX then error "Jacobian is not full rank";
    columnVectors := gens kernel jacM;
    evalColumnVectors := sub(columnVectors, apply(xList, x -> x => random(1, 100)));

    A := matrix for i to n-1 list {};
    count := 0;
    numSelectedCols := 0;
    selectColumns := while numSelectedCols < n-numX and count < #columnVectors list (
        B := A | matrix(evalColumnVectors_count);
	count = count + 1;
	if not rank B > rank A then continue;
	A = B;
	numSelectedCols = numSelectedCols + 1;
	count - 1
    );
    if #selectColumns != n-numX then error "Failed to find spanning set of kernel";
    unscaledMatrix := columnVectors_selectColumns;
    outMatrix := unscaledMatrix * transpose matrix{lamList};

    gradObjective := 2 * diagonalMatrix(W) * transpose(M - matrix{U});  -- 2w(f - u)
    criteqs := outMatrix - gradObjective;

    if o.UseMonodromy then (
        sys := polySystem flatten entries criteqs;
        sols := sparseMonodromySolve sys;
        number(points sols, p -> status p === Regular)
    )
    else (
        I := ideal(criteqs);
        degree I
    )
)

parameterizedUnitEDDegree = method(Options => parameterOptions)
parameterizedUnitEDDegree(List) := o -> (F) -> parameterizedWeightEDDegree(
    F,
    apply(#F, i -> rand()),
    apply(#F, i -> 1_(ring first F)),
    o
)

parameterizedGenericEDDegree = method(Options => parameterOptions)
parameterizedGenericEDDegree(List) := o -> (F) -> parameterizedWeightEDDegree(
    F,
    apply(#F, i -> rand()),
    apply(#F, i -> rand()),
    o
)

end

loadPackage "MonodromySolver"
loadPackage "EuclideanDistanceDegree"
setRandomSeed(123456);
R = QQ[x1,x2,x3,x4,x5];
F = {x1^2+x4^2, x2^2+x5^2, x3^2+1, x1*x2+x4*x5, x1*x3+x4, x2*x3+x5};
U = {1,2,3,4,5,6};
W = {1,1,1,1,1,1};
parameterizedWeightEDDegree(F,U,W)
parameterizedWeightEDDegree(F,U,W, UseMonodromy => true)

restart
loadPackage "EuclideanDistanceDegree"
R = QQ[x];
F = {x^2 - 1, x^3 - x, x^6};
parameterizedGenericEDDegree F

n = #F;
numX = #gens R;
S = QQ[gens R] ** QQ[y_1..y_(n-numX), u_1..u_n];
M = sub(matrix{F}, S);
imageModel = eliminate(support M, ideal(M - matrix{for i from 1 to n list u_i}));
determinantalUnitEDDegree((sub(imageModel, QQ[support imageModel]))_*)