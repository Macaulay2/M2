-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

-- TESTS TO WRITE (exported symbols);
--    AnnFs List
--    kDiffs
--    kOrderAnnFa
--    kOrderAnnFs
--    kappaAnnF1PlanarCurve

-- TESTS TO WRITE (unexported symbols);
--    diffRatFun (List, RingElement)
--    diffRatFun (List, RingElement, ZZ)
--    kCoeffVectorWRTs

needsPackage "Dmodules"
Dtrace 1

----------
-- AnnFs
----------
pInfo(1, "testing AnnFs...")

----------------- TEST AnnFs -------------------

-- TEST: AnnFs RingElement
x = symbol x; z = symbol z; d = symbol d; Dz = symbol Dz;
R = QQ[x_1..x_4, z, d_1..d_4, Dz, WeylAlgebra => ( toList(1..4) / (i -> (x_i => d_i)) | {z => Dz} ) ]
f = x_1 + x_2 * z + x_3 * z^2 + x_4 * z^3

Ann = AnnFs f
R = ring Ann
s = R_(numgens R - 1)
assert ( Ann == ideal {
	  z * d_1 - d_2,
	  z * d_2 - d_3,
	  z * d_3 - d_4,
	  d_2^2 - d_1 * d_3,
	  d_3^2 - d_2 * d_4,
	  d_2 * d_3 - d_1 * d_4,
	  x_2 * d_1 + 2 * x_3 * d_2 + 3 * x_4 * d_3 - Dz,
	  x_2 * d_2 + 2 * x_3 * d_3 + 3 * x_4 * d_4 - z * Dz,
	  x_1 * d_1 - x_3 * d_3 - 2 * x_4 * d_4 + z * Dz - s,
	  3 * x_4 * z * d_4 - z^2 * Dz + x_2 * d_3 + 2 * x_3 * d_4
	  })
  
  
-- TEST: AnnFs List



----------------- TEST AnnIFs -------------------
x = symbol x; dx = symbol dx;
W = QQ[x,dx, WeylAlgebra=>{x=>dx}]
Ann = AnnIFs(ideal dx, x^2)
WS = ring Ann
assert( Ann == ideal (x*dx - 2*WS_2) )  



----------------- TEST diffRatFun -------------------
diffRatFun := value(Dmodules#"private dictionary"#"diffRatFun")

-- Test 1
x = symbol x, y = symbol y;
R = QQ[x,y];
f = 1/(x^4 + y);
ans = -24*x^3/(x^4+y)^4;
assert(diffRatFun({1,2}, f) == ans);







