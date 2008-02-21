randomSparseIdeal = (B,r,n) -> (
     -- B is a list of monomials
     -- r is the size of each poly
     -- n is the number of polys
     -- returns an ideal
     S := ring B#0;
     ideal apply(n, j -> 
       sum apply(r, i -> random coefficientRing S * B#(random(#B))))
     )

looper3 = (rep,bound,B,r,n) -> (
     -- r is the number of monomials per poly
     -- n is the number of polys
     for i from 1 to rep do (
	  if i % 1000 == 0 then << "." << flush;
	  --J := randomSparseIdeal(B,r,n);
	  J := ideal flatten entries gens loopJ;
	  --regularity coker gens J;
	  --m := regularity coker gens J;
	  C := res(coker gens J, Strategy=>1);
	  --regularity C;
	  m := 4;
	  if m >= bound
	  then << "reg " << m << " " << toString J << endl;
	  )
     )

looper = (rep,loopJ) -> (
     for i from 1 to rep do (
	  if i % 1000 == 0 then << "." << flush;
	  J := ideal flatten entries gens loopJ;
	  C := res(coker gens J, Strategy=>1);
	  )
     )

end

restart
load "/Users/mike/src/M2/Macaulay2/bugs/mike/1-eisenbud-memleak.m2"
kk=ZZ/5
S=kk[a,b,c,d]
B = flatten entries gens(ideal basis(2,S) * ideal"a3,b3,c3,d3")
scan(10,i->loopJ = randomSparseIdeal(B,3,3))
collectGarbage()
time looper(10000,loopJ)
collectGarbage()

time looper(1000000,loopJ)

-- is this where the leak is? answer: NO
restart
load "/Users/mike/M2/Macaulay2/bugs/mike/1-eisenbud-memleak.m2"
kk=ZZ/5
S=kk[a,b,c,d]
B = flatten entries gens(ideal basis(2,S) * ideal"a3,b3,c3,d3")
time for i from 0 to 10000 do loopJ = randomSparseIdeal(B,3,3);
