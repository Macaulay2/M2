-- Deterministic families across characteristics. Change coordinates in each
-- presentation and compare the minimal Betti numbers with M2's own resolution.
runFamily = (kk,twist,family) -> (
    S := kk[a,b,c];
    (x,y,z) := (S_0 + twist*S_1, S_1 + twist*S_2, S_2);
    I := ({ideal(x^2), ideal(x^2,y^2), (ideal(x,y,z))^2,
           ideal(x*y,x*z,y*z), ideal(x^2,x*y,y^2,z^3)})#family;
    R := S/I;
    B := sAInfAlgModel R;
    assert(checkStasheff B and checkStasheff(aInfAlg B));
    for M in {coker vars R, R^1/ideal(sub(x,R),sub(z,R))} do (
        G := sAInfModModel(B,M);
        assert(checkStasheff G and checkStasheff(aInfMod G));
        F := burkeResolution(M,5,Check=>true);
        assert(F.dd^2 == 0);
        assert(ann coker F.dd_1 == ann M);
        reduce := map(kk,R,toList(numgens R:0));
        minimal := res(M,LengthLimit=>4);
        for j from 0 to 4 do (
            incoming := rank(reduce F.dd_(j+1));
            outgoing := if j == 0 then 0 else rank(reduce F.dd_j);
            assert(rank F_j - incoming - outgoing == rank minimal_j));
        assert(golodBetti(M,5) == betti F));
    );
for kk in {QQ,ZZ/2,ZZ/3,ZZ/101} do
    for twist in {0,1} do
        for family from 0 to 4 do runFamily(kk,twist,family);

-- A known infinite resolution gives a long independent check of actual maps.
S = QQ[t];
R = S/(t^5);
B = sAInfAlgModel R;
G = sAInfModModel(B,coker vars R);
F = burkeResolution(coker vars R,32,Check=>true);
for i from 1 to 32 do assert(F.dd_i == burkeDifferential(R,B,G,i));
assert(F.dd^2 == 0 and all(31,i -> HH_(i+1) F == 0));
assert(all(33,i -> rank F_i == 1));
assert(all(toList(1..32),i -> ideal F.dd_i == ideal(R_0^(if odd i then 1 else 4))));
