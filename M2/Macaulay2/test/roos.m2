gbTrace 3

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
R = kk[x,y,z,u,v]
J = ideal(x^2+y*z,y*v,z^2-x*v)
time hypers = makeHyperplanes J
