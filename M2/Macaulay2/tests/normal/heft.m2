dgs = {{1, 0, 3, 6}, {1, 6, 7, 5}, {1, 7, 2, 2}, {1, 5, 0, 6}, {1, 8, 7, 5}, {1, 4, 6, 0}, {1, 3, 1, 0}}
R = QQ[a..g, Degrees => dgs]
heft R
assert( heft R == {1,0,0,0} )

-- check that a bug doesn't reappear.  It was related to the heft vector.

A = transpose matrix {{1,0,0},{0,-1,0},{0,0,-1},{1,1,0},{1,0,1},{1,1,1}};
S = QQ[U1,Um2,Um3,U12,U13,U123,Degrees=>entries transpose A];
f = U12*U13-U1*U123;
I = ideal(Um2*U12-U1,Um2*U123-U13,Um3*U13-U1,Um3*U123-U12,U12*U13-U1*U123);
assert ( f % I == 0 )

