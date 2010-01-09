restart
loadPackage "Polyhedra"
P=convexHull matrix{{0,0,2,2},{0,2,0,2}}
Q=convexHull matrix{{0,1,2,1},{1,0,1,2}}
P1=convexHull matrix{{0,0,1},{0,1,0}}
mixedVolume {P,Q}
mixedVolume {Q,P}
mixedVolume {P,P}
mixedVolume {Q,Q}
mixedVolume {P1,P1}
mixedVolume {P,P1}
mixedVolume {P1,P}
mixedVolume {P1,Q}
mixedVolume {Q,P1}
volume P
volume Q
volume P1
L={Q,Q}
