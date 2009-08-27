S = ZZ/32003[x,y]
J = ideal jacobian ideal"55x8+66y2x9+837x2y6-75y4x2-70y6-97y7x2"
C = decompose J
-- each prime component P of J should contain J
D = C/(i -> (gens J) % i) -- but one doesn't!
assert all(D, m -> m == 0)
