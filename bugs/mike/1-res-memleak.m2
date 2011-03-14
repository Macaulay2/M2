-- Do resolutions sitll have memory leaks? 1 May 2009

R = ZZ/101[a..d];

for i from 1 to 10000 do (
     C := res(ideal(a^3-b^3,a*b*c-b^3,a*c*d-b^2*d), Strategy=>2);
     )

collectGarbage()

for i from 1 to 10000 do (
     R := ZZ/101[a..d];
     C := gens gb(ideal(a^3-b^3,a*b*c-b^3,a*c*d-b^2*d));
     )

time for i from 1 to 10000 do R := ZZ/101[a..d];

time for i from 1 to 10000 do R := (ZZ/101)(monoid[a..d]);

time for i from 1 to 100000 do R := (ZZ/101)(monoid[a..d]);

gbTrace=3
kk = ZZ/101; M = monoid[a..d]; time for i from 1 to 1 do R := kk M;
collectGarbage()
 
 

time for i from 1 to 1000 do (
     << "." << flush;
     for j from 1 to 1000 do R := ZZ/101[a..d];
     )


res(ideal(a^3-b^3,a*b*c-b^3,a*c*d-b^2*d), Strategy=>2);
