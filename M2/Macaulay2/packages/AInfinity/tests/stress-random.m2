-- A fixed seed makes failures in this family reproducible.
setRandomSeed 20261126;
for kk in {ZZ/2,ZZ/5,ZZ/101} do (
    for trial from 0 to 7 do (
        S := kk[a,b,c];
        quadrics := apply(2+trial%4,i -> random(2,S));
        R := S/(ideal quadrics);
        M := if even trial then coker vars R else coker random(R^2,R^{3:-1});
        B := sAInfAlgModel R;
        G := sAInfModModel(B,M);
        assert(checkStasheff B and checkStasheff G);
        assert(checkStasheff(aInfAlg B) and checkStasheff(aInfMod G));
        F := burkeResolution(M,5,Check=>true);
        assert(ann coker F.dd_1 == ann M);
        assert(betti F == golodBetti(M,5));
        reference := res(M,LengthLimit=>5);
        red := map(kk,R,toList(numgens R:0));
        for j from 0 to 4 do (
            outgoing := if j == 0 then 0 else rank(red F.dd_j);
            refOutgoing := if j == 0 then 0 else rank(red reference.dd_j);
            expected := rank reference_j - refOutgoing - rank(red reference.dd_(j+1));
            assert(expected == rank F_j - outgoing - rank(red F.dd_(j+1))));
        ));
