testpower = (f,N) -> (
    assert(f^0 == 1);
    g := f;
    elapsedTime for i from 1 to N-1 do g = f*g;
    elapsedTime h := f^N;
    assert(g == h);
    )

S = (GF 125)[x,y]
testpower(x+y, 5001)
testpower(x+y, 5006)

S = (GF 125)[x,y]
testpower(x+2*y, 5^5 + 5^3+5^2+3*5+2)
testpower(x+y, 5006)

S = (GF 125)[symbol x, symbol y, symbol z]
--testpower(x^2-3*y^2+z, 5^5 + 5^3+5^2+3*5+2) -- 20 seconds for iterative code...
