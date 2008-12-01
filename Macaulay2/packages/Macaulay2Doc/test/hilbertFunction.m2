e = {1,1}..{3,3}
s = set e
d = {{0,0},{0,1},{0,-1},{1,0},{ -1,0}}
R = ZZ/2[y_{1,1}..y_{3,3},x_{1,1}..x_{3,3},Degrees => {9:{1,2},9:{1,1}}]/ideal (
     splice apply(e, i -> (x_i^2,y_i^2,x_i*y_i)),
     apply(e, i -> product(select(d, j->s#?(i+j)),j->y_(i+j))));
a = timing (
     HS = numerator reduceHilbert hilbertSeries R;
     use ring HS;
     hilbFunc = (k,u) -> coefficient(T_0^k*T_1^u,HS);
     hilbFunc(3,6))
-- errorDepth = 0; alarm 1; hilbertFunction({3,6},R)
b = timing hilbertFunction({3,6},R)				    -- this is too slow
assert( first b < 5 * first a )
