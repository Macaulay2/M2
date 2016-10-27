restart
needsPackage "NumericalImplicitization" 
R = CC[s,t]; F = flatten entries basis(60,R);
allowableThreads = 4
W = numericalImageDegree(F, ideal 0_R, maxThreads => 4)

end--------------------------------
load "NumericalAlgebraicGeometry/SYSTEMS/monodromy/parallelNumericalImplicitization.m2"

Sampling point in source ...
Tracking monodromy loops ...
Points found: 2
Points found: 3
Points found: 4
Points found: 4
Points found: 5
Points found: 6
Points found: 7
Points found: 7
Points found: 10
Points found: 11
Finished tracking 11 paths in parallel
Finished tracking 11 paths in parallel
Points found: 13
Finished tracking 13 paths in parallel
Finished tracking 13 paths in parallel
Points found: 14
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/ofcm.m2:306:25:(1):[6]: error: degree list should be of length 6
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/methods.m2:98:83:(1):[5]: --back trace--
/nethome/aleykin3/H/M2-summer/M2/Macaulay2/packages/NumericalAlgebraicGeometry/track.m2:142:14:(3):[3]: --back trace--
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/methods.m2:112:80:(1):[2]: --back trace--
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/option.m2:8:8:(1):[1]: --back trace--
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/option.m2:8:8:(1): --back trace--
Finished tracking 8 paths in parallel
Points found: 16
Finished tracking 16 paths in parallel
Finished tracking 16 paths in parallel
Points found: 17
Finished tracking 17 paths in parallel
Finished tracking 17 paths in parallel
Points found: 19
Finished tracking 19 paths in parallel
Finished tracking 19 paths in parallel
Points found: 21
Finished tracking 21 paths in parallel
Finished tracking 21 paths in parallel
Points found: 25
Finished tracking 25 paths in parallel
Finished tracking 25 paths in parallel
Points found: 30
Finished tracking 30 paths in parallel
Finished tracking 30 paths in parallel
Points found: 34
Finished tracking 34 paths in parallel
Finished tracking 34 paths in parallel
Points found: 39
Finished tracking 39 paths in parallel
Finished tracking 39 paths in parallel
Points found: 43
Finished tracking 43 paths in parallel
Finished tracking 43 paths in parallel
Points found: 43
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/ofcm.m2:306:25:(1):[6]: error: degree list should be of length 0
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/methods.m2:98:83:(1):[5]: --back trace--
/nethome/aleykin3/H/M2-summer/M2/Macaulay2/packages/NumericalAlgebraicGeometry/track.m2:142:14:(3):[3]: --back trace--
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/methods.m2:112:80:(1):[2]: --back trace--
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/option.m2:8:8:(1):[1]: --back trace--
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/option.m2:8:8:(1): --back trace--
Finished tracking 22 paths in parallel
Finished tracking 21 paths in parallel
Points found: 46
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/ofcm.m2:306:25:(1):[6]: error: degree list should be of length 6
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/methods.m2:98:83:(1):[5]: --back trace--
/nethome/aleykin3/H/M2-summer/M2/Macaulay2/packages/NumericalAlgebraicGeometry/track.m2:142:14:(3):[3]: --back trace--
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/methods.m2:112:80:(1):[2]: --back trace--
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/option.m2:8:8:(1):[1]: --back trace--
/scratch/aleykin3/M2-anton/M2/Macaulay2/m2/option.m2:8:8:(1): --back trace--
Finished tracking 24 paths in parallel
Finished tracking 23 paths in parallel
Points found: 48
