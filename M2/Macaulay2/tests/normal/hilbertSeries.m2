-- hilbertSeries assumes the ring is a polynomial ring and gets into trouble when it's not.
-- To fix: check whether Mike's code handles the numerator correctly, first.

R = QQ[x,y]/x
M = R^1
fix = M -> (
     f := presentation ring M;
     coker f ** coker lift(presentation M,ring f))
M' = fix M
hilbertSeries M'
hilbertSeries M
assert( oo === ooo )

x = symbol x
a=b=2
R=ZZ/31991[x_1..x_4, Degrees=>{{1,0},{-2,1},{1,0},{0,1}}, Heft=>{1,3}];
OM = ker matrix{{x_1,-2*x_2,x_3,0},{0,x_2,0,x_4}};
k=16*max(abs(a),abs(b));
A=coker matrix{{(x_3*x_4)^k,(x_1*x_4)^k,(x_1*x_2)^k,(x_2*x_3)^k}};
DR = degreesRing R
hilbertSeries OM
hilbertSeries(OM, Reduce => true)
hilbertSeries(OM, Order => 7)
assert( oo == DR_0^2+2*DR_0^3+DR_1+3*DR_0^4+2*DR_0*DR_1+DR_0^(-2)*DR_1^2+4*DR_0^5+4*DR_0^2*DR_1+4*DR_0^(-1)*DR_1^2+DR_0^(-4)*DR_1^3+5*DR_0^6+6*DR_0^3*DR_1
     + 7*DR_1^2+4*DR_0^(-3)*DR_1^3+DR_0^(-6)*DR_1^4 )

--

R = ZZ/101[x,y]
M = R^1/x
T = degreesRing R
t = T_0
assert( hilbertSeries (M, Order => 5) == t^4+t^3+t^2+t+1 )
assert( hilbertSeries (M, Order => 4) == t^3+t^2+t+1 )
assert( hilbertSeries (M, Order => 7) == t^6+t^5+t^4+t^3+t^2+t+1 )
