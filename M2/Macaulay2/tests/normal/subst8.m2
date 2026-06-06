R = QQ[x,y,z];
f = x^3 - y^2 + 999*z;
assert( (f()) === f );
assert( (f(10,0,0)) === 1000/1 );
assert( (f(0,1,0)) === -1/1 );
assert( (f(0,0,1)) === 999/1 );
assert( (f(10,1,-1)) === 0/1 );
assert( (x^3 * f(0,0,1)) === 999*x^3 );
assert( (f(0,0,x+y+z) / 999) === x+y+z );
assert( (f(z,y,x)) === z^3-y^2+999*x );
assert( (f(x,x,x)) === x^3-x^2+999*x );
assert( (f(10)) === -y^2+999*z+1000 );
assert( (f(10,y,z)) === -y^2+999*z+1000 );
--
B = QQ[b]; A = B[a];
f = a + 10*b;
assert( (f()) === f );
assert( (f(1,2)) === 21/1 );
assert( (f(1)) === 10*b+1 );
C = A/(a^2+b^2)
g = sub(f, C);
assert( (g()) === g );
assert( (g(10*a)) === 10*a+10*b );
assert( (g(10*a^2,b^2)) == 0 );
--
R = ZZ[]
f = 4_R
assert( (f()) === 4 );
--
W = ZZ[w_1..w_2]
S = W[x..y]
T = W[t]
f = w_2*t^3+w_1*t^2+t
assert( (sub(f, {T_0 => S_0})) === w_2*x^3+w_1*x^2+x );
--
K = frac((ZZ/32749)[a..c]/c)
S = K[s]
substitute(s^2, { s => -s - 1 })
---
needsPackage "AssociativeAlgebras"
R = QQ<|e_0..e_2,t|>
sub(t, {t => 1})
