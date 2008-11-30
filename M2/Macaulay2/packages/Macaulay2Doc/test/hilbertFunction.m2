s = set ({1,1}..{3,3})
A = ZZ/2[y_{1,1}..y_{3,3},x_{1,1}..x_{3,3},Degrees => {9:{1,2},9:{1,1}}];
I = ideal (
     apply({1,1}..{3,3}, i -> x_i^2),
     apply({1,1}..{3,3}, i -> y_i^2),
     apply({1,1}..{3,3}, i -> x_i*y_i),
     apply({1,1}..{3,3}, i -> product(select({{0,0},{0,1},{0,-1},{1,0},{ -1,0}}, j->s#?(i+j)),j->y_(i+j))));
time HS = numerator reduceHilbert hilbertSeries I
R = A/I
use ring HS
hilbFunc = (k,u) -> coefficient(T_0^k*T_1^u,HS)
a = timing hilbFunc(3,6)
b = timing hilbertFunction({3,6},R)				    -- this is too slow
assert( first b < 100 * first a )
