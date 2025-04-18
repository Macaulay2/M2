TEST /// -- c.f. https://github.com/Macaulay2/M2/issues/1358
  R = QQ[x,y,z]
  I = ideal(z^2-y,y*z-x,y^2-x*z)
  M = coker (res I).dd_2
  F = sheaf(Spec R, M)
  assert(M == HH^0(F))
  assert(0 == HH^1(F))
///
