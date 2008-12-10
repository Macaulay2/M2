e = {1,1}..{3,3}
s = set e
d = {{0,0},{0,1},{0,-1},{1,0},{ -1,0}}
R = ZZ/2[y_{1,1}..y_{3,3},x_{1,1}..x_{3,3},Degrees => {9:{1,2},9:{1,1}}]/ideal (
     splice apply(e, i -> (x_i^2,y_i^2,x_i*y_i)),
     apply(e, i -> product(select(d, j->s#?(i+j)),j->y_(i+j))));
time hs = hilbertSeries(R,Reduce=>true);
a = time (
     HS = numerator hs;
     use ring HS;
     hilbFunc = (k,u) -> coefficient(T_0^k*T_1^u,HS);
     hilbFunc(3,6))
assert( a == 80 )
b = time hilbertFunction({3,6},R)			   -- this was too slow
assert( a == b )
assert( 10 == hilbertFunction({9,15},R) )
