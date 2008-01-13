-- if there is no solution to Ax=b, 
-- then "solve" should return an error statement.

R=ZZ/11;

--Example 1
A = matrix(R,{{1,2,3},{4,5,6},{1,2,3}});
b = transpose matrix(R,{{1,1,0}});
x = solve(A,b); -- should give an error, but happily computes some bogus answer
A*x == b

--Example 2
A = matrix(R,{{1,2,3},{4,5,6},{1,1,1}});
b = transpose matrix(R,{{1,1,1}});
x = solve(A,b); -- should give an error, but happily computes some bogus answer
A*x == b

--Also the "inverse" function will compute something, even for a singular matrix.
A = matrix {{1.,2,3},{4,5,6},{1,1,1}};
I = inverse A; -- should give an error, but happily computes some bogus answer
A*I

A = matrix {{1.,2,3},{4,5,6},{1,1,1}};
I = inverse A; -- should give an error, but happily computes some bogus answer
A*I

--how do I use "LU"?
