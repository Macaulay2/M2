checks = 0
invalid = 0
for k in {QQ,ZZ/2,ZZ/3} do (
    Q := k[p,q,r];
    R := Q/((ideal vars Q)^2);
    B := sAInfAlgModel R;
    G := sAInfModModel(B,coker vars R);
    for w in keys B#"mm" do (
        if B#"mm"#w == 0 then continue;
        mm := hashTable select(pairs B#"mm",(key,v)->key != w);
        C := sAInfAlg(Q,B#"natural",mm,B#"d");
        for n from 1 to 3 do (
            a := checkStasheff(C,n);
            assert(a === checkStasheff(aInfAlg C,n));
            checks = checks+1;
            if not a then invalid = invalid+1;));
    for w in keys G#"mm" do (
        if G#"mm"#w == 0 then continue;
        mm := hashTable select(pairs G#"mm",(key,v)->key != w);
        H := sAInfMod(B,G#"natural",mm);
        for n from 1 to 3 do (
            a := checkStasheff(H,n);
            assert(a === checkStasheff(aInfMod H,n));
            assert(a === checkStasheff(H[1],n));
            assert(a === checkStasheff((aInfMod H)[-2],n));
            checks = checks+1;
            if not a then invalid = invalid+1;));)
assert(checks == 135 and invalid == 75)
