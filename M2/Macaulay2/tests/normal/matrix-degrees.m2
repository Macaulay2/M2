  c = 2
  S = ZZ/101[x_1..x_c, a_(1,1)..a_(c,c)];
  X = matrix{{x_1..x_c}}
  ff = X*genericMatrix(S,a_(1,1),c,c)
  R = S/ideal ff;
  betti res coker vars R
  --The following is (very) wrong
  E =prune Ext(coker vars R, coker vars R);
  b=betti res E

  S = ZZ/101[p,q,r,s,t,u]
  ff = matrix"rp+sq, tp+uq"
  R = S/ideal ff
  betti res coker vars R
  --this is right!
  E =prune Ext(coker vars R, coker vars R);
  b'=betti res E

assert ( b == b' )
