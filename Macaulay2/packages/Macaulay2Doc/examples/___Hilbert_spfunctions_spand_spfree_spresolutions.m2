R = ZZ/32003[vars(0..17)];
M = coker genericMatrix(R,a,3,6)
isHomogeneous M
codim M
degree M
genera M
poincare M
hf = hilbertSeries M
reduceHilbert hf
poincare' = (M) -> (
        H := poincare M;
        t := (ring H)_0;  -- The variable t above
        while H % (1-t) == 0 do H = H // (1-t);
        H)
poincare' M
C = resolution M
C.dd_3
betti C
