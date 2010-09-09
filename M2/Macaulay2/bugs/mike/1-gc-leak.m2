-- 8 Sep 2010: this seems to no longer be a problem

-- This problem seems to be a GC problem
-- run this program with collect=true  to see memory kept under control by gc
-- run this program with collect=false to see memory balloon (watch it with ps or top)
-- the only difference is whether  to perform frequent garbage collection at opportune times
-- the problem is probably spurious internal pointers that prevent collection of many object
-- see also 1-gc-leak.c for a demo of that
if not instance(collect,Boolean) then error "expected \"collect\" to be set to true or false"
if not instance(reps,ZZ) then error "expected \"reps\" to be set to an integer giving the repetition count"
<< "--running the test " << (if collect then "with" else "without") << " frequent garbage collection" << endl
<< "--remember to run this just after starting M2" << endl
kk = ZZ/5
R = kk[vars(0..10-1)];
B = flatten entries basis(2,R);
J = ideal (h*i-2*d*j,g*i+2*i^2,d*g+2*h*j,f^2+2*a*i,b*f-2*g^2,c^2+h*j,b*c+2*c*f)
--betti res(J = randomSparseIdeal(B,2,7))
time for i from 1 to reps do (
     res ideal flatten entries gens J;
     if collect then collectGarbage();
     stderr << "." << flush;
     )
collectGarbage()
run ("ps u " | toString processID())
end
-- run it like this (experiment with adjusting the limits to suit your machine):
ulimit -v 60000
M2 -q -e reps=70,collect=false 1-gc-leak.m2 -e 'exit 0'
M2 -q -e reps=70,collect=true  1-gc-leak.m2 -e 'exit 0'

