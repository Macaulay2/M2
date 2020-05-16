-- had been a bug in promote...
R = QQ[]
m = matrix (R,{{2/1},{1/2}})
assert (2 * m == matrix(R,{{4},{1}}))
