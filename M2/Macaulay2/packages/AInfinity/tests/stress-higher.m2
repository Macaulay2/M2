-- This four-variable example has degree-three operations already over the
-- ambient polynomial ring. Remove one to make sure the identities detect it.
for kk in {QQ,ZZ/2,ZZ/3,ZZ/101} do (
    S := kk[x,y,z,u];
    I := ideal(u^3,x*y^2,(x+y)*z^2,x^2*u+z*u^2,y^2*u+x*z*u,y^2*z+y*z^2);
    R := S/I;
    B := sAInfAlgModel R;
    G := sAInfModModel(B,coker vars R);
    assert(checkStasheff B and checkStasheff(aInfAlg B));
    assert(checkStasheff G and checkStasheff(aInfMod G));
    assert(B == sAInfAlg(aInfAlg B) and G == sAInfMod(aInfMod G));
    Aops := B#"mm";
    ternary := select(keys Aops,k -> #k == 3 and Aops#k != 0);
    assert(#ternary > 0);
    damaged := hashTable select(pairs Aops,(k,v) -> k != first ternary);
    bad := sAInfAlg(S,B#"natural",damaged,B#"d");
    assert(not checkStasheff(bad,3));
    assert(not checkStasheff(aInfAlg bad,3));
    Mops := G#"mm";
    broken := hashTable select(pairs Mops, (k,v) -> #k == 1);
    H := sAInfMod(B,G#"natural",broken);
    assert(not checkStasheff(H,2) and not checkStasheff(aInfMod H,2));
    );
