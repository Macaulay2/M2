-- this works now, so make a test file

a=b=2
R=ZZ/31991[x_1..x_4, Degrees=>{{1,0},{-2,1},{1,0},{0,1}}, Heft=>{1,3}];
OM = ker matrix{{x_1,-2*x_2,x_3,0},{0,x_2,0,x_4}};
k=16*max(abs(a),abs(b));
A=coker matrix{{(x_3*x_4)^k,(x_1*x_4)^k,(x_1*x_2)^k,(x_2*x_3)^k}};
describe degreesRing A
hilbertSeries OM
hilbertSeries(OM, Reduce => true)
hilbertSeries(OM, Order => 6)

