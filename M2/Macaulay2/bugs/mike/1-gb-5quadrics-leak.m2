randomSparseIdeal = (B,r,n) -> (
     -- B is a list of monomials
     -- r is the size of each poly
     -- n is the number of polys
     -- returns an ideal
     S := ring B#0;
     ideal apply(n, j -> 
	  sum apply(r, i -> random coefficientRing S * B#(random(#B))))
     )

R = ZZ/5[vars(0..11), MonomialSize=>8]
B = flatten entries basis(2,R)

seed = 1288715482432
setRandomSeed seed

H = () -> (
     J := trim randomSparseIdeal(B,2,5);
     g := gens gb J;
     c := codim J;
     b := res J;
     )

doit = () -> (
     for i from 1 to 30 do (
	  << "i = " << i << flush;
	  time for j from 1 to 100 do H();
	  )
     )

end
restart
time load "1-gb-5quadrics-leak.m2"
time doit()
collectGarbage()
time doit()

-- keep some of g,c,b: (before loading file: real mem usage is 60.73 MB)
-- (none): real: 106.18 MB
-- b: real: 137 MB
-- g: 106.21 MB
-- c: 106.23 MB
-- bg: 166.74 MB
-- cg: 106.23 MB
-- bc: 244.67 MB
-- bcg: 228.78 MB

restart
time load "1-gb-5quadrics-leak.m2"
time for i from 1 to 10 do time (doit(); collectGarbage())
-- bcg: 745.30 MB (154.29 sec)
-- c: 121.92 MB (51.16 sec)
-- g: 106.31 MB (42.74 sec)
-- b: 213.27 MB (87.86 sec)
-- bg: 343.11 MB (115.62 sec)
-- bc: 760.95 MB (150.13 sec)
-- gc: 121.36 MB (55.0 sec)
-- (none): 106.18 MB (28.12 sec)

-- export GC_free_space_divisor=10
--  i=550, real mem 657 MB
--  i=825, real mem 940 MB
-- export GC_free_space_divisor=2
--  i= 326, real mem 1.02 GB
-- export GC_free_space_divisor=6
--  i= 550, real mem 723 GB
--  i = 825, real mem 1.01 GB
