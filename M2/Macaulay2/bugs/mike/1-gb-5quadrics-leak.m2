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

Ha = () -> (
     J := ideal (l^2,a*k+f*l,g*i,b*c-2*e*f,a*b+g*k);
     --c := codim J;
     b := res J;
     )

doit = () -> (
     for i from 1 to 30 do (
	  --<< "i = " << i << flush;
	  for j from 1 to 100 do H();
	  )
     )

end


-- first comment out the lines "b", "c", or "g" in H() above
-- and then run the following
restart
load "1-gb-5quadrics-leak.m2"
time for i from 1 to 10 do time (doit(); collectGarbage())
run ("ps u "|processID())

-- with trim: 472.13 MB / 527.64 MB
-- w/o trim: 293.78 MB / 344.89 MB
-- only trim: 106.24 MB / 143.64 MB
-- only res, no trim, no codim: 167.05 / 210.39 MB

-- Here, e.g. bg means comment out only the "c" line:

-- bcg: 745.30 MB (154.29 sec)
-- c: 121.92 MB (51.16 sec)
-- g: 106.31 MB (42.74 sec)
-- b: 213.27 MB (87.86 sec)
-- bg: 343.11 MB (115.62 sec)
-- bc: 760.95 MB (150.13 sec)
-- gc: 121.36 MB (55.0 sec)
-- (none): 106.18 MB (28.12 sec)

end

on rhodium -- Linux rhodium 2.6.18.1 #1 SMP PREEMPT Sat Nov 11 02:11:17 CET 2006 i686 pentium4 i386 GNU/Linux

-- bcg: 651520K (390.976 seconds)
-- c:    60140K (128.652 seconds)
-- g:    37360K (109.199 seconds)
-- b:   103484K (214.065 seconds)
-- bg:  224316K (287.994 seconds)
-- bc:  669M    (393.985 seconds)
-- gc:   59884K (156.442 seconds)
-- :     37360K ( 74.1686 seconds)
