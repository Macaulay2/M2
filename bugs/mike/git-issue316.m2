-- git issue 315
R = ZZ[x, y, MonomialOrder => Lex]
I = ideal(32,16*x^2*y-10*x*y+1*x)
  gI = ideal gens gb I;
  assert( ( (gens I)%gI) == 0 );
  
-- git issue 316
restart
R = ZZ[x, y, MonomialOrder => GRevLex]
I = ideal(-19*x*y+7*y^2-12*x,-5*y^3)
  gI = ideal gens gb I;
  assert( ( (gens I)%gI) == 0 );
 ggI = ideal gens gb gI;
 assert(numColumns (gens gI) == numColumns(gens ggI)); -- fails
gI
ggI

R = ZZ[x, y, MonomialOrder => GRevLex]
I = ideal(8*x^3+6,-22*x^3+8*x*y^2-17*y)
  gI = ideal gens gb I;
  assert( ( (gens I)%gI) == 0 );
 ggI = ideal gens gb gI;
 assert(numColumns (gens gI) == numColumns(gens ggI)); -- fails

R = ZZ[x, y, MonomialOrder => Lex]
I = ideal(22*y^3,-22*x^2*y-13*x-2*y,-26*x*y^2-x)
  gI = ideal gens gb I;
  assert( ( (gens I)%gI) == 0 );
(gens I)%gI
gI

end

-- first example above:

   -- DEGREE 0, number of spairs = 1
   -- considering generator 32<1> : 
   --     new GB elem: g0 = 32<1> [gap 0 size 1 deg 0 mingen]
   -- DEGREE 3, number of spairs = 1
   -- considering generator 16x2y<1>-10xy<1>+x<1> : 
   --     tail rem by g0, yielding -10xy<1>+x<1>
   --     tail rem by g0, yielding x<1>
   --     tail rem by g0, yielding 0
   --     new GB elem: g1 = 16x2y<1>-10xy<1>+x<1> [gap 0 size 3 deg 3 mingen]
   --   creating spair(g0,g1): deg 3 lcm exponents [2 1 0 0 ]
   -- DEGREE 3, number of spairs = 1
   -- considering spair(g0,g1): deg 3 lcm exponents [2 1 0 0 ] : 
   --     tail rem by g0, yielding 2x<1>
   --     tail rem by g0, yielding 0
   --     new GB elem: g2 = 12xy<1>+2x<1> [gap 1 size 2 deg 3]
   --   creating spairgcd(g0,g2) deg(3) lcm[1 1 0 0 ]
   --   creating spairgcd(g1,g2) deg(4) lcm[2 1 0 0 ]
   --   creating spair(g0,g2): deg 3 lcm exponents [1 1 0 0 ]
   --   creating spair(g1,g2): deg 4 lcm exponents [2 1 0 0 ]
   -- DEGREE 3, number of spairs = 2
   -- considering spair(g0,g2): deg 3 lcm exponents [1 1 0 0 ] : 
   --     tail rem by g0, yielding 0
   --     new GB elem: g3 = 16x<1> [gap 0 size 1 deg 3]
   --   auto reduce 2 by 3
   --   auto reduce 1 by 3
   -- considering spairgcd(g0,g2) deg(3) lcm[1 1 0 0 ] : 
   --   swapping with GB element 2
   --     tail red by g3, yielding 0
   --     retiring 2 new GB elem: g4 = 4xy<1>+6x<1> [gap 1 size 2 deg 3]
   --   swap yielded
   --       16x<1>
   --       4xy<1>+6x<1>
   --     reducing by g3, yielding 0
   --   creating spair(g0,g3): deg 3 lcm exponents [1 0 0 0 ]
   --   creating spair(g1,g3): deg 5 lcm exponents [2 1 0 0 ]
   --   creating spair(g3,g4): deg 3 lcm exponents [1 1 0 0 ]
   -- DEGREE 3, number of spairs = 2
   -- considering spair(g3,g4): deg 3 lcm exponents [1 1 0 0 ] : 
   --   swapping with GB element 3
   --     retiring 3 new GB elem: g5 = 8x<1> [gap 0 size 1 deg 3]
   --   auto reduce 4 by 5
   --   auto reduce 1 by 5
   --   swap yielded
   --       0
   --       8x<1>
   --   creating spair(g0,g5): deg 3 lcm exponents [1 0 0 0 ]
   --   creating spair(g4,g5): deg 5 lcm exponents [1 1 0 0 ]
   -- DEGREE 3, number of spairs = 1
   -- considering spair(g0,g5): deg 3 lcm exponents [1 0 0 0 ] : 
   -- DEGREE 4, number of spairs = 2
   -- DEGREE 5, number of spairs = 2
   -- considering spair(g4,g5): deg 5 lcm exponents [1 1 0 0 ] : 
   --     tail rem by g5, yielding 0
   --     new GB elem: g6 = 4x<1> [gap 0 size 1 deg 5]
   --   creating spair(g5,g6): deg 5 lcm exponents [1 0 0 0 ]
   --   creating spair(g4,g6): deg 7 lcm exponents [1 1 0 0 ]
   -- DEGREE 5, number of spairs = 1
   -- considering spair(g5,g6): deg 5 lcm exponents [1 0 0 0 ] : 
   -- DEGREE 7, number of spairs = 1
   -- considering spair(g4,g6): deg 7 lcm exponents [1 1 0 0 ] : 
   --     tail rem by g5, yielding 0
   --     new GB elem: g7 = 2x<1> [gap 0 size 1 deg 7]
   --   creating spair(g6,g7): deg 7 lcm exponents [1 0 0 0 ]
   -- DEGREE 7, number of spairs = 1
   -- considering spair(g6,g7): deg 7 lcm exponents [1 0 0 0 ] : 
   -- number of (nonminimal) gb elements = 8
   -- number of monomials                = 9
   -- #reduction steps = 1
   -- #spairs done = 15
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- Groebner basis, 8 elements
   -- GB elem: g0 = 32<1> [gap 0 size 1 deg 0 mingen]
   -- GB elem: g1 = 16x2y<1>-10xy<1>+x<1> [gap 0 size 3 deg 3 mingen]
   -- removed
   -- removed
   -- GB elem: g4 = 4xy<1>-2x<1> [gap 1 size 2 deg 3]
   -- GB elem: g5 = 8x<1> [gap 0 size 1 deg 3]
   -- GB elem: g6 = 4x<1> [gap 0 size 1 deg 5]
   -- GB elem: g7 = 2x<1> [gap 0 size 1 deg 7]
   -- computation of GB completed

g0 = I_0 -- 32
g1 = I_1 -- 16*x^2*y - 10*x*y + x
  -- gpair(g0,g1) gives g1, so nothing new DONE
  -- spair(g0,g1) DONE
g2 = 2*g1-x^2*y*g0 + g0*x*y -- 12*x*y + 2*x -- spair(g0,g1) REMOVED
  -- gpair(g0,g2):
  -- spair(g0,g2)
  -- gpair(g1,g2):
  -- spair(g1,g2)
-- spair(g0,g2):
g3 = 8*g2-3*x*y*g0 -- 16x REMOVED
-- gpair(g0,g2):
g4 = 3*g2-x*y*g0 -- 4xy+6x -- replaces g2, this is ok.
   g2-3*g4+g3 == 0
-- spair(g3,g4)
g5 = (4*g4 - y*g3) - g3 -- 8x
x*g0 -4*g5 == 0
-- spair(g4,g5)
y*g5 - 4*g4 + y*g5 + 3 * g5 == 0
g6 = -(y*g5 - 4*g4 + 2*g4 + g5)


-- another example
restart
R = ZZ[x,a,b,c,d, MonomialOrder=>Eliminate 1]       
f = x^7+3*x^4+a*x+b
g = x^8+x^5+c*x+d
elapsedTime gens gb ideal(f,g);
elapsedTime gens gb(ideal(f,g), MaxReductionCount=>1000);
elapsedTime gens gb(ideal(f,g), MaxReductionCount=>1);
time eliminate(ideal(f,g),x)

R = QQ[x,a,b,c,d, MonomialOrder=>Eliminate 1]       
f = x^7+3*x^4+a*x+b
g = x^8+x^5+c*x+d
