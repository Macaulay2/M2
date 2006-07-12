f = random(ZZ^6, ZZ^4)
M = subquotient ( f * diagonalMatrix{2,3,8,21}, f * diagonalMatrix{2*11,3*5*13,0,21*5} )
factor M
