setRandomSeed() -- just to fix the numbers so the tests will be deterministic
R = RR[x,y,z]
M = matrix {apply(3, i -> random(R^3, R^3) * transpose vars R)}
assert (det M != 0) -- this line would sometimes return 0, and silently
  -- cause an error that would be noticed:
assert(x+y != 0) -- would give an error, or possibly crash M2

R = CC_300[x,y,z]
M = matrix {apply(3, i -> random(R^3, R^3) * transpose vars R)}
assert (det M != 0) -- this line would sometimes return 0, and silently
  -- cause an error that would be noticed:
assert(x+y != 0) -- would give an error, or possibly crash M2

R = RR
M = random(R^3, R^3)
assert(det M != 0)

R = CC_400
M = random(R^3, R^3)
assert(det M != 0)
