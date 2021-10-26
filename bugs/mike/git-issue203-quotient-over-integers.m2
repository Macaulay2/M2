restart
R = ZZ[x,y,z]

I = ideal(17*x*z+10*x-43,41*x*y-16*x,47*z^2-2*z)
J = ideal( 34*x-16,-2*y*z,-20)

Q = quotient(J, I)
gQ = quotient(ideal gens gb J,ideal gens gb I)
assert (Q == gQ)

-- 
trim ideal (34*x - 16, -2*y*z, -20)
ideal gens gb ideal (34*x - 16, -2*y*z, -20)
trim ideal gens gb ideal (34*x - 16, -2*y*z, -20)
code methods trim

mingb = m -> gb (m, StopWithMinimalGenerators=>true, Syzygies=>false, ChangeMatrix=>false);

              zr := f -> if f === null or f == 0 then null else f;
                F := ambient M;
                epi := g -> -1 === rawGBContains(g, rawIdentity(raw F,0));


L = ideal(34*x - 16, -2*y*z, -20)
mingb = m -> gb (m, StopWithMinimalGenerators=>true, Syzygies=>false, ChangeMatrix=>false);
mingb L
gens oo
