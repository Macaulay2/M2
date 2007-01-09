R = ZZ/101[a..d]
M = kernel vars R ++ cokernel vars R
generators M
relations M
M === subquotient(generators M, relations M)
prune M,
