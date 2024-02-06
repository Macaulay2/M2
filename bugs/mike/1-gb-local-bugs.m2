-- Examples of computing Groebner/standard
-- bases in local rings, or localized rings.
loadPackage "ExampleIdeals"

-- The simplest example, over ZZ/101, QQ, ZZ
kk = ZZ/101
R = kk{t,x,y,z}
I = ideal(x-x^2)
gens gb I
x % I
assert(x % I == 0)
assert(flatten entries gens I == {x-x^2})

R = QQ{t,x,y,z}
I = ideal(x-x^2)
gens gb I
x % I
assert(x % I == 0)
assert(flatten entries gens I == {x-x^2})

R = ZZ{t,x,y,z}
I = ideal(x-x^2)
gens gb I
x % I -- hangs!!  -- BUG
assert(x % I == 0)
assert(flatten entries gens I == {x-x^2})

-- Another simple example
-- BUG: this should stop much sooner...! But it goes to this degree in singular too.
-- This appears slower than singular.
kk = ZZ/101
I = value (examplesStdSingular kk)#1#1
gbTrace=1
time gens gb I

{* -- Singular code
ring R=101,(t,x,y,z),ds;
ideal i = 5t3x2z+2t2x5y3,7y+2tz+4x2y+xy2,3tz+3yz2+2yz4;
timer=1;
option(prot);
option(teach);
std(i);
*}
---------------------------------------------
--
kk = ZZ/101
R = kk{t,x,y,z}
I = ideal"2t2z+z3t+z3+tz,x2z4t3y+t7,x4+zy2+y2"
m1 = syz gens I
m2 = syz m1
m3 = syz m2

loadPackage "LocalRings"
S = kk[t,x,y,z]
setMaxIdeal ideal vars S
I = ideal"2t2z+z3t+z3+tz,x2z4t3y+t7,x4+zy2+y2"
M = comodule I
localResolution M
oo.dd

loadPackage "LocalRings"
S = kk[t,x,y,z]
setMaxIdeal ideal vars S
I = ideal"2t2z+z3t+z3+tz,x2z4t3y+t7,x7+zy2+y2"
M = comodule I
localResolution M
oo.dd

--------------------------------------------------------
--mike-syz
--
kk = ZZ/101
R = kk{t,x,y,z}
I = ideal"2t2z+z3t+z3+tz,x2z4t3y+t7,x7+zy2+y2"
gens gb I;
gbTrace=15
syz(gens I, DegreeLimit=>30)
syz(gens I, DegreeLimit=>100)
{* -- Singular
  ring R=101,(t,x,y,z),ds;
  ideal i = 2t2z+z3t+z3+tz,
            x2z4t3y+t7,
	    x7+zy2+y2;
  timer=1;
  option(prot);
  option(teach);
  std(i); // this is very fast 
  syz(i); // this doesn't finish in a reasonable amount of time...
*}

kk = ZZ/101
R = kk{t,x,y,z}
I = ideal"4t2z+6z3t+3z3+tz,
          5x2z4t3y + 3t7,
          2x5 + 6z2y2 + 2y4"
time gens gb I; -- this does not finish...
time mingens kernel gens I

{* -- Singular
  ring R=101,(t,x,y,z),ds;
  ideal i = 4t2z+6z3t+3z3+tz,
          5x2z4t3y + 3t7,
          2x5 + 6z2y2 + 2y4;
  timer=1;
  option(prot);
  option(teach);
  std(i); // this doesn't finish in a reasonable amount of time...
  syz(i); // this is very fast
*}
--------------------------------------------------------
--mike-syz
--
kk = ZZ/101
R = kk[t,x,y,z, MonomialOrder=> {Position=>Down, Weights=>{-1,-1,-1,-1}, GRevLex=>4}, Global=>false]
M1 = matrix"2t2z+z3t+z3+tz,x2z4t3y+t7,x7+zy2+y2"	  
M = M1 || -id_(R^3)
leadTerm M
gbTrace=3
gens gb M; -- does not finish... try in singular too...
s1 = gens gb(M, DegreeLimit=>50)

gens gb I;
gbTrace=15
syz(M1, DegreeLimit=>30)

matrix"tz+2t2z+z3+tz3, -1, 0, 0; t7+t3x2yz4, 0, -1, 0; y2+y2z+x7, 0, 0, -1"

{* -- singular -- not working yet
  ring R=101,(t,x,y,z),(c,ds);
matrix m[3][4] = tz+2t2z+z3+tz3, -1, 0, 0,
  t7+t3x2yz4, 0, -1, 0,
  y2+y2z+x7, 0, 0, -1;
matrix n = transpose(m);
module M=n;
M;
std(M);
  timer=1;
  option(prot);
  option(teach);
  std(m);
  syz(i);
*}
