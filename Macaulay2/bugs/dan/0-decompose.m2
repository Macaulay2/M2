S = ZZ/32003[x,y]
I = ideal jacobian ideal "55x8+66y2x9+837x2y6-75y4x2-70y6-97y7x2"
i = gens I

-- this is part of what decompose is doing
(w,phi) = irreducibleCharacteristicSeries I;
P = ideal w#4;
R = ring P;			   -- R and S have different monomial orderings
j = phi^-1 i
j % P == 0
i % phi P == 0
a = first select(first entries last topCoefficients gens P, i -> degree i =!= {0});
g = gens saturate(P, a, Strategy=>Eliminate)   -- this step changes it
j % g == 0			  -- this one is okay
i % phi g == 0			  -- oops! is the ring map spoiling it?

j == g * (j // g) + (j % g)				    -- oops!
use ring g; g' = value toString g
j == g' * (j // g') + (j % g')				    -- now it works!
g == g'
gens gb g == gens gb g'					    -- oops!

h = gens saturate(P, a, Strategy=>Eliminate)
peek h.cache
j == h * (j // h) + (j % h)				    -- fails
peek h.cache						    -- one of the two gb's here is suspicious, since it doesn't reach degree 7!
-- is it possible that it was continued after being stopped with StopWithMinimalGenerators in degree 3, and then it
-- didn't go all the way?
scan(keys h.cache, k -> if instance(k,GroebnerBasisOptions) then remove(h.cache,k)) -- try clearing the cache
j == h * (j // h) + (j % h)				    -- now it works!
peek h.cache


C = decompose I
-- each prime component P of I should contain I
D = C/(i -> (gens I) % i) -- but one doesn't!
assert all(D, m -> m == 0)
