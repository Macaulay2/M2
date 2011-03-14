-- This code is experimental, and probably old.  Still, it might be useful.
-- Trying to get fraction field GB's working well... at least 0-dimensional ones.

-- from primary decomp examples:

kk = ZZ/32003
R = kk[reverse{a,b,c,d,e,f,g,h}]
I = ideal(
    h+f+e-d-a,
    2*f*b+2*e*c+2*d*a-2*a^2-a-1,
    3*f*b^2+3*e*c^2-3*d*a^2-d+3*a^3+3*a^2+4*a,
    6*g*e*b-6*d*a^2-3*d*a-d+6*a^3+6*a^2+4*a,
    4*f*b^3+4*e*c^3+4*d*a^3+4*d*a-4*a^4-6*a^3-10*a^2-a-1,
    8*g*e*c*b+8*d*a^3+4*d*a^2+4*d*a-8*a^4-12*a^3-14*a^2-3*a-1,
    12*g*e*b^2+12*d*a^3+12*d*a^2+8*d*a-12*a^4-18*a^3-14*a^2-a-1,
    -24*d*a^3-24*d*a^2-8*d*a+24*a^4+36*a^3+26*a^2+7*a+1)
--independentSets I

-- uses R
spoly = (f,g) -> (
     -- assumed: f and g are in some localization of R
     a := sub(leadCoefficient f,R);
     b := sub(leadCoefficient g,R);
     m := sub(leadMonomial f,R);
     n := sub(leadMonomial g,R);
     z := flatten entries syz(matrix{{a,b}}, SyzygyLimit=>1);
     mn := gcd(m,n);
     z1 := sub(z_0 * (n//mn),ring f);
     z2 := sub(z_1 * (m//mn),ring f);
     << "z = " << z << endl;
     << "multipliers are: " << z1 << " and " << z2 << endl;
     z1 * f + z2 * g
     )

-- uses R
pcontent = (f) -> sub(gcd flatten entries sub((coefficients f)_1, R), ring f)
removeContent = (f) -> f // pcontent f

genericpt = (I, m) -> (
     R := ring I;
     K := frac((coefficientRing R)[support m]);
     newvars := rsort toList(set gens R - set support m);
     R1 := K[newvars, MonomialOrder=>Lex];
     sub(I,R1)
     )

genericpt2 = (I, m) -> (
     R := ring I;
     suppm := support m;
     newvars := rsort toList(set gens R - set suppm);
     R1 := (coefficientRing R)[newvars, suppm, MonomialOrder=>{#newvars, #suppm}];
     sub(I,R1)
     )

genericptZZ = (I, m) -> (
     R := ring I;
     K := frac(ZZ[support m]);
     newvars := rsort toList(set gens R - set support m);
     R1 := K[newvars];
     sub(I,R1)
     )

use ring I
J = genericpt(I,b*c*g)
J = genericpt2(I,b*c*g)
J = genericptZZ(I,b*c*g)

R = frac(QQ[a])[x]
F = leadCoefficient (a*x)
leadTerm F
G = a^2*x^2+a^3*x
G / x -- crash
G / a^2 -- crash
G / sub(a^2,R)
G // a^2 -- this one is OK

J_1
leadCoefficient J_1
1/o7 * o6
gbTrace=10
-- This one dies a slow death:
time gens gb J

K = frac(ZZ/32003[a])
R = K[b]
I = ideal(2*a*b-3)
gens gb I

K = frac(ZZ[a])
R = K[b]
I = ideal(2*a*b-1)
gens gb I -- this one is OK

K = frac(QQ[a])
R = K[b]
I = ideal(2*a*b-1)
gens gb I -- this one is NOT ok

K = frac(QQ[a,t]/(t-1))
R = K[b]
I = ideal(2*a*b-1)
gens gb I -- this one is ok

-- An attempt to do GB by hand --
J_0  -- lead term h, not needed elsewhere, I think
J_1 -- (2*b) * f         x
J_2 -- (3*b^2) * f       x
J_3 -- (6*g*b) * e       x
J_4 -- (4*b^3) * f
J_5 -- (8*g*c*b) * e
J_6 -- (12*g*b^2) * e
J_7 -- -24 * d*a^3

g1 = J_1
g2 = J_3
g3 = g*b*(2*J_2 - 3*b*g1) - (c^2-c*b)*g2 -- lead term d*a^2
2*(3*g*b*(J_4 - 2*b^2*g1) - (2*c^3-2*c*b^2)*g2) + g*b*J_7 -- lead term d*a^2 too

g1 = 1/(2*b)*J_1  -- lead term f
g2 = 1/(6*g*b)*J_3 -- lead term e
g1 = g1 - coefficient(e,g1) * g2
  tmp = J_2 - 3*b^2*g1
  tmp = tmp - (leadCoefficient tmp)*g2 -- lead term d*a^2
  tmp = (1/(leadCoefficient tmp)) * tmp  
g3 = tmp -- lead term d*a^2
  tmp = J_4 - 4*b^3*g1
  tmp = tmp - (leadCoefficient tmp) * g2 - 4*a*g3
  tmp = tmp - (leadCoefficient tmp)*g3
  tmp = (1/(leadCoefficient tmp)) * tmp    
g4 = tmp -- d*a

---------------------------------------
-- no denominators GB, done by hand ---
---------------------------------------
g1 = 1/2_kk*J_1  -- leadterm b.f obsolete
g2 = 1/6_kk * J_3 -- leadterm gb.e
g1 = g*b*g1 - c*g2 -- leadterm gb2.f

-- J_2
  t1 = spoly(1/3_kk * J_2,g1)
  t1 = spoly(t1,g2)
j2 = t1 -- leadterm: (-c2+gb+cb).da2
-- J_4
  tmp = J_4
  tmp = spoly(tmp,g1)
  tmp = spoly(tmp,g2)
j4 = tmp -- leadterm da3
-- J_5
  tmp = J_5
  tmp = spoly(tmp,g2)
  j5 = tmp
-- J_6
  tmp = J_6
  j6 = spoly(tmp,g2)
-- J_7
  j7 = J_7

j4 = spoly(j4,j7)
j5 = spoly(j5,j7)
j6 = spoly(j6,j7)

j2 = spoly(j2,j6)
j4 = spoly(j4,j6)
j5 = spoly(j5,j6)
j7 = spoly(spoly(j7,j6),j6)

netList{j2,j4,j5,j7,j6}
-- use j7 as the simple one:

j2 = spoly(j2,j7)
j4 = spoly(j4,j7)
j5 = spoly(j5,j7)
j6 = spoly(spoly(j6,j7),j7)
netList{j2,j4,j5,j6,j7}
j2 = removeContent j2
j4 = removeContent j4
j5 = removeContent j5
j6 = removeContent j6
j7 = removeContent j7

j2 = spoly(j2,j6)
j4 = spoly(j4,j6)
j5 = spoly(j5,j6)
j7 = spoly(spoly(j7,j6),j6)
netList{j2,j4,j5,j7,j6}
factor sub(j7,R)

j2 = removeContent j2
j4 = removeContent j4
j5 = removeContent j5
j6 = removeContent j6
j7 = removeContent j7

netList{j2,j4,j5,j7,j6}

-- now use j5
j2 = removeContent spoly(j2,j5)
j4 = removeContent spoly(j4,j5)
j7 = removeContent spoly(spoly(j7,j5),j5)
---------------------------------------

  
       leadCoefficient g3
factor oo
sub(g3,R)
factor oo

-- sub(I, {b=>13,c=>7,g=>22}) -- doesn't work
sub(I, {b=>13,c=>7,g=>22}) -- doesn't work
A = ring J

B = ZZ/32003[gens A]

getgb = (p1,p2,p3) -> (
  phi = map(B,A,vars B | matrix{{p1*1_B,p2,p3}});
  gens gb phi J)

getgb(1,2,3)
getgb(1,2,4)
getgb(1,2,5)
getgb(1,2,6)

phi = map(B,A,vars B | matrix{{17_B,13,25}})
phi J
gens gb oo
