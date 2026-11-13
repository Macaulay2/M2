rand := randomZZ  --This is the function used to compute random numbers. 
--random(QQ) is not a good choice in practice.

--This function computes weighted ED degrees
symbolicOptions = { ReturnCriticalIdeal => false }
symbolicWeightEDDegree = method(Options => symbolicOptions)
symbolicWeightEDDegree(List, List, List) := o-> (F, data, weight) -> (
    I := ideal F;
    xList := gens ring I;
    jac := transpose jacobian matrix{F};
    topRow := apply(#weight, i -> 2 * weight_i * (xList_i-data_i));
    M := matrix{topRow} || jac;  -- augmented Jacobian
    c := codim I;
    win := I + minors(c+1, M); -- critical ideal
    sl := ideal singularLocus I;
    win = saturate(win, sl);
    if o.ReturnCriticalIdeal then win else degree win
)

determinantalUnitEDDegree = method(Options => symbolicOptions)
determinantalUnitEDDegree(List) := o-> (F) -> symbolicWeightEDDegree(
    F,
    apply(#gens ring first F, i->rand()),
    apply(#gens ring first F, i->1_(ring first F)),
    o
)

determinantalGenericEDDegree = method(Options => symbolicOptions)
determinantalGenericEDDegree(List) := o-> (F) -> symbolicWeightEDDegree(
    F,
    apply(#gens ring first F, i->rand()),
    apply(#gens ring first F, i->rand()),
    o
)

end

-*
restart
load "./EDD_Determinantal.m2"
makeJac = (system, unknowns) -> (for i in system list for j in unknowns list diff(j, i))

R = QQ[x, y];
F = {x^2 + y^2 - 1}
(U,W) = ({12, 23}, {15, 331})
UED = determinantalUnitEDDegree F 
GED = determinantalGenericEDDegree F 
ICP = symbolicWeightEDDegree(F, U, W, ReturnCriticalIdeal => true)
*-