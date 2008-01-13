A = QQ[x,y,z]
M = matrix(A, {{1,2,3},{4,5,6},{7,8,9}})
Hom(M,A^2)
Hom(A^2,M)
contraHom = (M, s) -> (
    (n,m) := (numgens target M, numgens source M);
    R := mutableZero(ring M, s*n, s*m);
    for b from 0 to m-1 do
      for a from 0 to s-1 do
        for c from 0 to n-1 do
          R_(a*n+c,a*m+b) = M_(b,c);
    matrix R
    )
contraHom(M,2)
