-- Some basic tests and benchmarks of ring maps:

R1 = ZZ/101[x_0 .. x_10]
R2 = ZZ/101[x_0 .. x_10,MonomialOrder=>Lex]
R3 = ZZ/101[x_0 .. x_8][x_9,x_10,Join=>false]
R4 = ZZ[x_0 .. x_10]
R5 = QQ[x_0 .. x_10]

--time m = random(R1^1, R1^{-6});  -- 11/12/97 time: 94.87, or 83.57 seconds!
--                                 -- 11/14/97: 11.91 seconds
--				 -- 11/18/97: 11.26 seconds

time m = random(R1^1, R1^{-5});

--time m = random(R1^1, R1^{-2}); 
size(m_(0,0))
------------------------------------------------------
F1 = map(R1,R1,vars R1)
time (F1 m);   -- 11/12/97 time: 14.39 seconds 
               -- 11/13/97 time:   .77 seconds
               -- 11/14/97 time:   .56 seconds
time assert(m == F1 m)
               -- 11/13/97 time:   .76 seconds
               -- 11/14/97 time:   .56 seconds
------------------------------------------------------
F2 = map(R2,R1,vars R2)
F2inv = map(R1,R2,vars R1)
time (m2 = F2 m);   
                    -- 11/12/97 time: 14.01 seconds
                    -- 11/13/97 time:  1.8  seconds
		    -- 11/14/97 time:  1.61 seconds
time (m == F2inv m2)
                    -- 11/13/97 time: .93 seconds
		    -- 11/14/97 time: .72 seconds
------------------------------------------------------
F3 = map(R3,R1)
F3inv = map(R1,R3)
scan(gens R1, x -> assert( x == F3inv F3 x ))
time (m3 = F3 m);   
                    -- 11/12/97: 4.36 seconds
                    -- 11/13/97: 5.61 seconds hmmmm
		    -- 11/14/97: 4.15 seconds
time assert(m == F3inv m3)
                    -- 11/12/97: 7.49 seconds
                    -- 11/13/97: 3.27 seconds
		    -- 11/14/97: 3.26 seconds
------------------------------------------------------
F5 = map(R4,R1,vars R4)
F5inv = map(R1,R4,vars R1)
scan(gens R1, x -> assert( x == F5inv F5 x ))
time (m5 = F5 m);   
                -- 11/12/97: 13.94 seconds
                -- 11/13/97:  1.11 seconds
		-- 11/14/97:   .67 seconds
time assert(m == F5inv m5)
                -- 11/13/97:  1.57 seconds
		-- 11/14/97:   .58 seconds
------------------------------------------------------
use R1
time F6 = map(R1,R1,apply(toList(0..10), i -> x_i - x_0))
time (F6 m);	-- 11/12/97: 18.55 seconds
                -- 11/13/97:  3.87 seconds
		-- 11/14/97:  4.52 seconds
------------------------------------------------------
use R1
time F6 = map(R1,R1,apply(toList(0..10), i -> if i == 0 then x_0 else x_i - x_0))
time F6inv = map(R1,R1,apply(toList(0..10), i -> if i == 0 then x_0 else x_i + x_0))
scan(gens R1, x -> assert( x == F6inv F6 x ))
time (m6 = F6 m);	
                -- 11/13/97: 5.16 seconds
		-- 11/14/97: 5.66 seconds
time assert(m == F6inv m6)
		-- 11/13/97: 5.18 seconds
		-- 11/14/97: 5.71 seconds
------------------------------------------------------
F7 = map(R5,R1,vars R5)
F7inv = map(R1,R5,vars R1)
scan(gens R1, x -> assert( x == F7inv F7 x ))
time (m7 = F7 m);    
                -- 11/12/97: 14.82 seconds
                -- 11/13/97:  3.27 seconds
		-- 11/14/97:  1.02 seconds
time assert(m == F7inv m7)
                -- 11/13/97:  1.01 seconds
		-- 11/14/97:   .75 seconds
------------------------------------------------------
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ringmap2.out"
-- End:
