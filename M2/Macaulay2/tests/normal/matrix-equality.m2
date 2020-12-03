-- matrix equality, fixed after M2 1.13.
debug Core
  R = ZZ/101[a..d]
  M = image vars R
  M1 = image matrix{{b,a,c,d}}
  M == M1
  N = R^{4:-1}
  f0 = map(R^1,, matrix{{a,b,c,d}})
  -- example 1: sources are identical, targets are same image module.
  f = inducedMap(M,source f0,f0)
  g = inducedMap(M1,source f0,f0)
  assert(source f === source g)
  assert(source f == source g)
  assert(raw super f === raw super g)
  assert(f == g) -- fails in version 1.13
  -- example 2: sources are identical, targets are same subquotient
  --  (but with different image parts).
  M = (image vars R)/(image matrix{{c,d}})
  M1 = (image matrix{{b,a,c,d}})/(image matrix{{d,c}})
  assert(M == M1)
  assert(M =!= M1)
  f = inducedMap(M,source f0,f0)
  g = inducedMap(M1,source f0,f0)
  assert(source f === source g)
  assert(source f == source g)
  assert(raw super f === raw super g)
  assert(f == g) -- fails in version 1.13
  -- next case: sources are the same image, but with different gen order
  M = image vars R  
  N = image matrix {{a,b,c,d}}
  N1 = image matrix {{c,a,b,d}}
  f = map(R^1, N, {{a,b,c,d}})
  g = map(R^1, N1, {{c,a,b,d}})
  assert isWellDefined f
  assert isWellDefined g
  assert(f == g)  -- fails in version 1.13
end--

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test matrix-equality.out"
-- End:
