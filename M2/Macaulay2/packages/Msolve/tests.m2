TEST ///
  K = ZZ/1073741827
  R = K[x_(0,0), x_(0,1)]
  I = ideal(2*x_(0,0)+3*x_(0,1))
  assert(I == ideal msolveSaturate(I, x_(0,0)))
  assert(I == saturate(I, x_(0,0), Strategy => Msolve))

  -- c.f. https://github.com/algebraic-solving/msolve/issues/165
  K = ZZ/11
  R = K[x_(0,0), x_(0,1)]
  I = ideal(2*x_(0,0)+3*x_(0,1))
  assert(try (ideal msolveSaturate(I, x_(0,0));         false) else true)
  assert(try (saturate(I, x_(0,0), Strategy => Msolve); false) else true)
  assert(I == saturate(I, x_(0,0)))
///
