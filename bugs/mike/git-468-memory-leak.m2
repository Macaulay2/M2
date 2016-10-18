--23.05.2016
-- we want to explore possible memory leaks in the "res(, FastNonminimal)" command

loadPackage"RandomCanonicalCurves"

leak1 = () -> (
    setRandomSeed "37862873";
    g := 9;
    p := 12347;
    numEx := 100;
    T := ZZ/p[t_0..t_(g-1)];
    i := 0;
    L := {};
    while  (i < numEx) do (
        IC := (random canonicalCurve)(g,T);
        --L=L|{betti(res(IC,FastNonminimal=>true),Minimize=>true)};
        betti(res(IC,FastNonminimal=>true),Minimize=>true);
        IC = symbol IC;
        print(i);
        i=i+1;
        )
    )

leak2 = () -> (
    setRandomSeed "37862873";
    g := 9;
    p := 12347;
    numEx := 400;
    T := ZZ/p[t_0..t_(g-1)];
    i := 0;
    I1 := null;
    for i from 1 to 50 do elapsedTime I1 = (random canonicalCurve)(g,T);
    while  (i < numEx) do (
        IC := ideal(I1_*);
        betti(res(IC,FastNonminimal=>true),Minimize=>true);
        IC=symbol IC;
        print(i);
        i=i+1;
        )
    )

doOne = (g,T) -> (
    IC := (random canonicalCurve)(g,T);
    oldtrace := gbTrace;
    gbTrace=3;
    res(IC,FastNonminimal=>true);
    gbTrace=oldtrace;
    )
leak3 = () -> (
    setRandomSeed "37862873";
    g := 9;
    p := 12347;
    numEx := 100;
    T := ZZ/p[t_0..t_(g-1)];
    i := 0;
    L := {};
    while  (i < numEx) do (
        doOne(g,T);
        print(i);
        i=i+1;
        )
    )

end
restart
load "~/src/M2-new/bugs/mike/git-468-memory-leak.m2"
-- 79.3 MB memory used at this point
leak1() -- seems to leak about 9.4 MB per iteration
leak2() -- leaking perhaps 1.275 MB per iteration
  -- But note: the computation of canonical curves is rock solid at 79.3 MB.

leak3() -- same behavior as leak1


--initialization:
g=9;
p=12347;
numEx=100;
T=ZZ/p[t_0..t_(g-1)]
IC=(random canonicalCurve)(g,T);
gbTrace=3
res(IC,FastNonminimal=>true)
    betti(res(IC,FastNonminimal=>true),Minimize=>true);

--
i=0;
L={};
while  (i < numEx) do (
    IC=(random canonicalCurve)(g,T);
    --L=L|{betti(res(IC,FastNonminimal=>true),Minimize=>true)};
    betti(res(IC,FastNonminimal=>true),Minimize=>true);
    IC=symbol IC;
    print(i);
    i=i+1;
    )

while  (i < numEx) do (
    IC=(random canonicalCurve)(g,T);
    --L=L|{betti(res(IC,FastNonminimal=>true),Minimize=>true)};
    << betti(res(IC,FastNonminimal=>true),Minimize=>true) << endl;
    IC=symbol IC;
    print(i);
    i=i+1;
    )

while  (i < numEx) do (
    IC=(random canonicalCurve)(g,T);
    --L=L|{betti(res(IC,FastNonminimal=>true),Minimize=>true)};
    << betti(res(IC)) << endl;
    IC=symbol IC;
    print(i);
    i=i+1;
    )

I1 = (random canonicalCurve)(g,T);
gbTrace=0
while  (i < numEx) do (
    IC=ideal(I1_*);
    --L=L|{betti(res(IC,FastNonminimal=>true),Minimize=>true)};
    << betti(res(IC,FastNonminimal=>true),Minimize=>true) << endl;
    IC=symbol IC;
    print(i);
    i=i+1;
    )

while  (i < numEx) do (
    IC=(random canonicalCurve)(g,T);
    IC=symbol IC;
    print(i);
    i=i+1;
    )

while  (i < numEx) do (
    IC=(random canonicalCurve)(g,T);
    --L=L|{betti(res(IC,FastNonminimal=>true),Minimize=>true)};
    res(IC,FastNonminimal=>true);
    IC=symbol IC;
    collectGarbage();
    print(i);
    i=i+1;
    )
    

