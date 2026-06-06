leftKernelOptions = { TempDirectory => null }
leftKernelWeightEDDegree = method(Options => leftKernelOptions)
leftKernelWeightEDDegree(List, List, List) := o-> (F, data, weight) -> (
    dir := o.TempDirectory ?? temporaryFileName();
    if not fileExists dir then mkdir dir;

    R := ring first F;
    numX := #gens R;
    coef := coefficientRing R;
    S := R ** coef[apply(#F+1, i->"L"|i)] ** coef[apply(numX, i->"u"|i)] ** coef[apply(numX, i->"w"|i)];
    
    xList := flatten entries basis({1,0,0,0}, S);
    lamList := flatten entries basis({0,1,0,0}, S);
    uList := flatten entries basis({0,0,1,0}, S);
    wList := flatten entries basis({0,0,0,1}, S);

    c := #lamList - 1;
    jac := sub(transpose jacobian matrix{F}, S);
    topRow := apply(#weight, i->2*wList_i*(xList_i-uList_i));
    M := matrix{topRow} || jac;  -- augmented Jacobian
    critEq := flatten entries((matrix{lamList} * sub(M, S)));
    restrictLam := apply(#lamList - 1 - #F, i->makeB'Section(drop(lamList, 1)));

    win := restrictLam | F | critEq;
    consts := (transpose{uList, data}) | (transpose{wList, weight});
    --sl = ideal singularLocus I;
    --win = saturate(win, sl);

    makeB'InputFile(
        dir,
        AffVariableGroup => {xList},
        HomVariableGroup => {lamList},
        BertiniInputConfiguration => {"UseRegeneration" => 1, "TrackType" => 0, "PrintPathProgress" => 1000},
        B'Polynomials => win,
        B'Constants => consts
	);

    dir = addSlash(dir);
    runBertini(dir);
    --readFile(dir);
    numSols := null;
    scanLines(ell -> (numSols = value ell; break), dir | "nonsingular_solutions");
    numSols
)

leftKernelUnitEDDegree = method(Options => leftKernelOptions)
leftKernelUnitEDDegree(List) := o -> (F) -> leftKernelWeightEDDegree(
    F,
    apply(#gens ring first F, i->randCC()),
    apply(#gens ring first F, i->1_(ring first F)),
    o
)

leftKernelGenericEDDegree = method(Options => leftKernelOptions)
leftKernelGenericEDDegree(List) := o-> (F) -> leftKernelWeightEDDegree(
    F,
    apply(#gens ring first F, i->randCC()),
    apply(#gens ring first F, i->randCC()),
    o
)

end

-*
R = QQ[x,y];
F = {x^2+y^2-1};
(U,W) = ({.12, .23}, {.15, .331})
GED = leftKernelWeightEDDegree(F, U, W);
GED = leftKernelGenericEDDegree F;
UED = leftKernelUnitEDDegree F;
*-