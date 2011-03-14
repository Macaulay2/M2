restart
gbTrace=3
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



-- Let's minimalize and isolate this bug:
restart
gbTrace=3
S = ZZ/32003[x,y]
I = ideal jacobian ideal "55x8+66y2x9+837x2y6-75y4x2-70y6-97y7x2"
(w,phi) = irreducibleCharacteristicSeries I;
P = ideal w#4;
a = first select(first entries last topCoefficients gens P, i -> degree i =!= {0});

g = gens saturate(P, a, Strategy=>Eliminate)   -- this step changes it

g0 = gens saturate(P, a, Strategy=>Eliminate, MinimalGenerators=>false)   -- this step changes it

use ring P
g0 = ideal(y^2+13992*y*x-5435*y-10903*x^2-3917*x-15812,-8493*y*x-1644*y+x^3-8548*x^2+9473*x-918,y*x^2+11875*y*x-11064*y-7229*x^2-15219*x+1509)
g = gens g0
g = gens trim g0
--g = gens trim ideal g0
use ring g; g' = value toString g
g == g'
gens gb g == gens gb g'					    -- oops!
gens gb g
gens gb g'

restart
gbTrace=15
R = (ZZ/32003)[y, x, MonomialOrder=>Lex]
g0 = ideal(y^2+13992*y*x-5435*y-10903*x^2-3917*x-15812,-8493*y*x-1644*y+x^3-8548*x^2+9473*x-918,y*x^2+11875*y*x-11064*y-7229*x^2-15219*x+1509)
--g = gens g0
g = gens trim g0
--g = gens trim ideal g0

use ring g; g' = value toString g0
g == g'
gens gb g == gens gb g'					    -- oops!
gens gb g
gens gb g'

trim g0
oo == ideal flatten entries gens g0

-- The following isolates the bug
restart
gbTrace=15
R = (ZZ/32003)[y, x, MonomialOrder=>Lex]
g0 = ideal(y^2+13992*y*x-5435*y-10903*x^2-3917*x-15812,-8493*y*x-1644*y+x^3-8548*x^2+9473*x-918,y*x^2+11875*y*x-11064*y-7229*x^2-15219*x+1509)

-- The problem?  Are the trimmed gens actually generators of g0?
g0 -- the original ideal
g1 = trim g0
g2 = ideal flatten entries gens g1

gens gb g1
gens gb g2
gens gb g0

g0 == g2 -- false
g0 == g1 -- true
g1 == g2 -- true...

-- Now try this:
gens gb g0
-- I think: the trim'ed gens do NOT generate here...

-- hypothesis: while computing the gb of g0, the 'trimmed' generators do not generate the ideal.

restart
gbTrace=15
R = (ZZ/32003)[y, x, z, MonomialOrder=>Lex]
I = ideal(y*x-x^3, y^2*x-z)
gens gb I
trim I
