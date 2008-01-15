-- if there is no solution to Ax=b, 
-- then "solve" should return an error statement.

R=ZZ/11;

--Example 1
A = matrix(R,{{1,2,3},{4,5,6},{1,2,3}});
b = transpose matrix(R,{{1,1,0}});
assert try (x = solve(A,b);false) else true
b = transpose matrix(R,{{-5,4,-5}});
x = solve(A,b); -- should give an error, but happily computes some bogus answer
assert(A*x == b)

assert try(A^-1;false) else true

--Example 2
A = matrix(R,{{1,2,3},{4,5,6},{1,1,1}});
b = transpose matrix(R,{{1,1,1}});
try(x = solve(A,b);false) else true -- should give an error, but happily computes some bogus answer
assert try(A^-1;false) else true

--Also the "inverse" function will compute something, even for a singular matrix.
A = matrix {{1.,2,3},{4,5,6},{1,1,1}};
assert try(inverse A; false) else true

--Example: number of columns is smaller than the number of rows
A = matrix(R,{{1,2},{4,5},{1,2}})
LU A
--how do I use "LU"?

A = matrix(R,{{1,2,3,0},{4,5,6,0},{1,2,3,0},{1,1,1,1}})
A1 = matrix(RR_53,{{1,2,3,0},{4,5,6,0},{1,2,3,0},{1,1,1,1}})
LU A1
LU A
oo_1 * oo_2

A = matrix(R,{{1,0,0},{2,0,0}})
LU A

A = matrix(R,{{1,0,0},{2,1,0}})
LU A

A = matrix(R,{{1,0,0},{2,0,0},{1,0,0}})
LU oo
