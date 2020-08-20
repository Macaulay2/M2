needsPackage "FourierMotzkin"
M =  matrix{{1,1,1,1,1,1,1,1},{-1,1,-2,2,1,-2,2,-1},{2,2,1,-1,-2,-1,1,-2}}
assert( fourierMotzkin M == fourierMotzkin M_{4..7,0..3} )
