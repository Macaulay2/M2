testpower = (f,N) -> (
    assert(f^0 == 1);
    g := f;
    elapsedTime for i from 1 to N-1 do g = f*g;
    elapsedTime h := f^N;
    assert(g == h);
    )

S=ZZ/5[x,y] -- or ZZ/5[x,y,z]/z or (GF 125)[x,y]
testpower(x+y, 5001)
testpower(x+y, 5006)
testpower(random(2,S), 5006)
--testpower(random(2,S)+random(1,S), 5006)

S=ZZ/5[x,y,z]/z -- or ZZ/5[x,y,z]/z or (GF 125)[x,y]
testpower(x+y, 5001)
testpower(x+y, 5006)
