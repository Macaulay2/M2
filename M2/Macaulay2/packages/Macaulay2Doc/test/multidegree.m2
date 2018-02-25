S = QQ[a..d, Degrees => {{2,-1},{1,0},{0,1},{ -1,2}}];
A = degreesRing S;
assert( heft S == {1,1} )
M = S^1/(b^2,b*c,c^2)
assert ( 3 == degree M )
assert( 3*T_0*T_1 == multidegree M )
N = S^1/a
assert( 2*T_0-T_1 == multidegree N )
assert( 1 == degree N )
M' = S^1/(a^2,a*b,b^2);
assert( 6*T_0^2-3*T_0*T_1 == multidegree M' )
assert ( 3 == degree M' )
-- now a case where heft *isn't* {1...1} but post-#737-fix should still work
R = QQ[x,y,Degrees=>{{1,-1},{1,1}}];
B = degreesRing R;
assert(multidegree ideal(x,y) == (T_0+T_1)*(T_0-T_1))
