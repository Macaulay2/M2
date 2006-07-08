for i to 5 do (
     N = prune (M = coker ( random (ZZ^11, ZZ^3) * matrix "2,,;,3,;,,7" ) ) ;
     assert (numgens source presentation N < 6);
     assert isIsomorphism N.cache.pruningMap ;
     )

