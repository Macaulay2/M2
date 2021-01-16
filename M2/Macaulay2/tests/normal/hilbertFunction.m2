-- see https://github.com/Macaulay2/M2/issues/1701
S = ZZ/101[a,b,c]
I = monomialIdeal(a^2, b^2)
assert(hilbertFunction(2, I) == 4)
peek I.cache
assert(hilbertFunction(3, I) == 4)
peek I.cache
assert(hilbertFunction(2, I) == 4)

--

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

--

R=ZZ/31991[x_1..x_4, Degrees=>{{1,0},{-2,1},{1,0},{0,1}}, Heft=>{1,3}];
OM = ker matrix{{x_1,-2*x_2,x_3,0},{0,x_2,0,x_4}};
a=2
b=2
-- k=32
k=12
A=coker matrix{{(x_3*x_4)^k,(x_1*x_4)^k,(x_1*x_2)^k,(x_2*x_3)^k}};
E2=Ext^2(A,OM);
time size hilbertSeries(E2,Order => 1)			    -- this computes a lot of numbers
assert( oo == 1017 )
time r = hilbertFunction({0,0},E2)
time r' = numgens source basis({0,0},E2)		    -- this shows basis can be a lot faster than hilbertFunction, which uses hilbertSeries
assert( r == r' )
