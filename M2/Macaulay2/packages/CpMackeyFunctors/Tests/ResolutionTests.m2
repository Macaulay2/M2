TEST ///

-- if this passes then free resolutions should be exact.
for i to 50 do (
    p = 2;
    M = makeRandomCpMackeyFunctor(p);
    d = res(M, 0);
    assert isTrivialMackeyFunctor coker d_0;
);

///