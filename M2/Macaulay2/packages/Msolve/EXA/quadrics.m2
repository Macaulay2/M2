-- longer tests, benchmarks, etc.
-- (not loaded by the package)
needsPackage "Msolve"
quadricsExample = method()
quadricsExample Ring := FF -> (
    R := FF[a..o,x,y];
    F := x^2 + a*x*y + b*y^2 + c*x + d*y + e;
    G := x^2 + f*x*y + g*y^2 + h*x + i*y + j;
    H := x^2 + k*x*y + l*y^2 + m*x + n*y + o;
    eMsolve := msolveEliminate({x,y}, ideal{F,G,H});
    assert(numgens eMsolve == if char FF == 0 then 1 else 66);
    assert(size eMsolve_0 == 21894);
    )

-- measurements on Intel Core i7-8650U CPU @ 8x1.90GHz
FF = ZZ/nextPrime 2^30 -- 33s via msolve vs 26s via eliminate
FF = ZZ/nextPrime 2^20 -- 32s via msolve vs 25s via eliminate
FF = QQ                -- 43s via msolve vs 27s via eliminate
elapsedTime quadricsExample FF

end
restart
load "quadrics.m2"
