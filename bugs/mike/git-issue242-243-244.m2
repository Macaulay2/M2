-- 193 
R=QQ[x,Inverses=>true,MonomialOrder=>RevLex]
R/ideal(x-1) -- hangs

rng = ZZ[x,y,z]
I = ideal(-5*z+4, 6*y-1, 9*z+8)
gI = ideal gens gb I
ggI = ideal gens gb gI
assert(leadTerm ggI_2 % leadTerm gI_2 == 0)
assert(leadTerm gI_2 % leadTerm ggI_2 == 0)
assert(gI_* == {19, z+3, y+3})
assert(ggI_* == {19, z+3, y+3})

-- 242
restart
--gbTrace=15
rng = ZZ[x,y,z]
I = ideal(-5*z+4, 6*y-1, 9*z+8)
gens gb I
gI = ideal gens gb I
ggI = ideal gens gb gI
leadTerm ggI_2 % leadTerm gI_2
leadTerm gI_2 % leadTerm ggI_2

-- DEGREE 1
g0 = -I_0
g0 = -2*I_0-I_2  -- z-16
g1 = 9*g0-5*I_0 -- 76




[gb]
   -- DEGREE 1, number of spairs = 3
   -- considering generator -5z<1>+4<1> : 
   --     new GB elem: g0 = 5z<1>-4<1> [gap 0 size 2 deg 1 mingen]
   -- considering generator 9z<1>+8<1> : 
   --   swapping with GB element 0
   --   swap yielded
   --       76<1>
   --       z<1>-16<1>
   --     new GB elem: g1 = 76<1> [gap 0 size 1 deg 1 mingen]
   --   auto reduce 0 by 1
   -- considering generator 6y<1>-<1> : 
   --     tail rem by g1, yielding -<1>
   --     tail rem by g1, yielding 0
   --     new GB elem: g2 = 6y<1>-<1> [gap 0 size 2 deg 1 mingen]
   --   auto reduce 1 by 2
   --   auto reduce 0 by 2
   -- spair(g0,g1): deg 2 lcm exponents [0 0 1 0 0 ]
   -- spairgcd(g1,g2) deg(1) lcm[0 1 0 0 0 ]
   -- spair(g1,g2): deg 1 lcm exponents [0 1 0 0 0 ]
   -- spair(g0,g2): deg 2 lcm exponents [0 1 1 0 0 ]
   -- DEGREE 1, number of spairs = 2
   -- considering spair(g1,g2): deg 1 lcm exponents [0 1 0 0 0 ] : 
   --   swapping with GB element 1
   --   auto reduce 2 by 1
   --   auto reduce 0 by 1
   --   swap yielded
   --       0
   --       38<1>
   -- considering spairgcd(g1,g2) deg(1) lcm[0 1 0 0 0 ] : 
   --   swapping with GB element 2
   --     tail red by g1, yielding 0
   --   auto reduce 1 by 2
   --   auto reduce 0 by 2
   --   swap yielded
   --       19<1>
   --       2y<1>+6<1>
   --   swapping with GB element 1
   --   auto reduce 2 by 1
   --   auto reduce 0 by 1
   --   swap yielded
   --       0
   --       19<1>
   -- DEGREE 2, number of spairs = 2
   -- considering spair(g0,g1): deg 2 lcm exponents [0 0 1 0 0 ] : 
   --     reducing by g1, yielding 0
   -- considering spair(g0,g2): deg 2 lcm exponents [0 1 1 0 0 ] : 
   --     reducing by g2, yielding 6z<1>+18<1>
   --     reducing by g0, yielding 0
   -- number of (nonminimal) gb elements = 3
   -- number of monomials                = 5
   -- #reduction steps = 3
   -- #spairs done = 7
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- Groebner basis, 3 elements
   -- GB elem: g0 = z<1>+3<1> [gap 0 size 2 deg 1 mingen]
   -- GB elem: g1 = 19<1> [gap 0 size 1 deg 1 mingen]
   -- GB elem: g2 = 2y<1>+6<1> [gap 0 size 2 deg 1 mingen]
   -- computation of GB completed
   -- 
o4 = | 19 z+3 2y+6 |

-- 243

------------------------------------------------
x = symbol x
rng = ZZ[x_1]
I = ideal(-6*x_1^3+10*x_1-10,-2*x_1^2-x_1,-x_1^3,-4*x_1^3+8*x_1^2)
gI = ideal gens gb I;
assert( ( (gens I)%gI) == 0 ); --fails
gI

rng = ZZ[x_1]
I = ideal(-6*x_1^3-10,-2*x_1^2-x_1,-4*x_1^3)
gI = ideal gens gb I;
assert( ( (gens I)%gI) == 0 ); --fails
gI
-- 244

rng = ZZ[x_1, x_2, x_3 ]
I   = ideal(6*x_3,2*x_3-12,-3*x_1+1)
gI  = ideal gens gb  I;
ggI = ideal gens gb gI;
gI
ggI 
assert(numColumns (gens gI) == numColumns(gens ggI)); --fails

f1 = I_0 -- 6x3
f2 = I_1 -- 2x3 - 12
f3 = I_2 -- -3x1 + 1

g0 = f1 -- 6x3
f1 - 3*f2 -- 36
g1 = f1-3*f2
g0 = f2
g2 = -f3
netList {g0,g1,g2}
-- g1,g2:
x_1 * g1 - 12 * g2 -- gives 12
g1 = x_1 * g1 - 12 * g2 -- gives 12, but now there is an spair
-- that has been forgotten spair(12, 3x_1-1) !
g0
g1



-- matrix_lift?
-- insure that nrows > 0 ?
