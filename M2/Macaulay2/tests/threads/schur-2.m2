needsPackage "SchurRings"
S = schurRing(s,3);
rep = s_{5};
M = {1_S,s_{5},s_{10},s_{15},s_{20},s_{25},s_{30}};
for i to 15 do ( stderr << i << endl; schurResolution(rep,M,SyzygyLimit => 3) );
