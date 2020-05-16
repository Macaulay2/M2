-- From: jeroos@pavidus.matematik.su.se
-- Date: Wed, 8 Aug 2001 10:36:05 +0200
-- To: dan@math.uiuc.edu
-- Subject: in27

stderr << "this test is no longer relevant" << endl
exit 0

binaries = (n) -> (
     if n === 0 then {}
     else if n === 1 then {{0},{1}}
     else (
     r := binaries(n-1);
     join(apply(r, i->prepend(0,i)),
          apply(r, i->prepend(1,i)))))

doit = (i) -> (
     h := hypers_(0,i);
     J1 := J + ideal(h);
     << newline << flush;
     << "--- n = " << i << " ideal = " << hypers_(0,i) << " ---" << flush;
     << newline << flush;
     -- Now the hard part
     A := (ring J1)/J1;
     C := res(coker vars A, LengthLimit=>6);
     << newline << flush;
     << "  " << betti C << flush;
     << newline << flush;
     )

makeHyperplanes = (J) -> (
    I := matrix basis(2,coker gens J);
    c := numgens source I;
    m := transpose matrix binaries c;
    I * m)


kk = ZZ/31
R = kk[x,y,z,u,v,w,SkewCommutative => true]
J = ideal(x*y,y*v+x*w+u*w,u*v+y*w)
time hypers = makeHyperplanes J;
--time scan(numgens source hypers, i -> doit i);

n = 6
collectGarbage()
collectGarbage()
collectGarbage()
k = # netRows engineHeap()
k'= # netRows engineMemory()
time scan(n, doit);
collectGarbage()
collectGarbage()
collectGarbage()
K = # netRows engineHeap()
K'= # netRows engineMemory()
w = floor( 1/2 + (K-k)/n )
w' = floor( 1/2 + (K'-k')/n )
stderr << "average number of handles wasted per iteration : " << w << endl
stderr << "average number of memory types wasted per iteration : " << w' << endl
engineHeap
engineMemory
assert( w < 2 )
assert( w' < 2 )
