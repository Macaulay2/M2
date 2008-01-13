R = ZZ/101[a..c];
f = vars R
ker f
coker f
image f
image f ++ coker f
M = subquotient(f, matrix {{a}})
prune M
