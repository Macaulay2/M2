toCoefficients = (F) -> (
    R := ring F;
    deg := degree(R_0, F);
    monoms := for i from 0 to deg list (R_0)^(deg-i);
    (notused, coeffs) := coefficients(F, Monomials=>monoms);
    flatten entries lift(coeffs, coefficientRing R)
    )
eval = method()
eval(List, CC) := 
eval(List, RingElement) := (L, x0) -> (
    t := L#0;
    for i from 1 to #L-1 do (
        t = t*x0 + L#i;
        );
    t
    )
eval(RingElement, CC) := 
eval(RingElement, RingElement) := (F, x0) ->  eval(toCoefficients F, x0)
companionMatrix = (F) -> (
    L := toCoefficients F;
    R := coefficientRing ring F;
    deg := #L-1;
    M := mutableMatrix(R, deg, deg);
    for i from 0 to deg-2 do M_(i+1,i) = 1.0_R;
    for i from 0 to deg-1 do M_(i,deg-1) = -L#(deg-i);
    M
    )
roots = (F) -> (
    -- F is a polynomial in CC_prec[x]
    M := companionMatrix F;
    eigenvalues M
    )
polishRoots = (prec, F, roots) -> (
    L := toCoefficients F;
    L' := toCoefficients diff((ring F)_0, F);
    for rt in roots list (
        x := rt;
        num := eval(L,x);
        while abs(num) > prec do (
            denom := eval(L',x);
            x = x - num/denom;
            num = eval(L,x);
        );
        x
    ))

end

restart
load "~/src/M2-git-linalg/bugs/mike/roots.m2"
printingPrecision = 50
R = CC_200[x]
F = x^3-6*x^2+5*x-1
roots F
(roots F)/(r -> eval(F,r))
polishRoots(2^-100, F, roots F)
oo/(r -> eval(F,r))
roots (F^2)
(mns, coeffs) = coefficients(F, Monomials=>{x^3, x^2, x, 1_R})
L = flatten entries lift(coeffs,CC_53)

L
eval(L, 2.0)
eval(L, 1.0)
eval(L, .3)
eval(L, .4)
eval(L, .30797)

companionMatrix(L)
eigenvalues companionMatrix L

printingPrecision = 50
R = CC[x]
F = x^3-6*x^2+5*x-1
(roots F)/(r -> promote(r, CC_150))

polishRoots(2^-15, F, roots F)
G = F^2
L = toCoefficients G
rts = roots G
rts/(r -> eval(L, r))
