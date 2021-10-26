-- Problem: Fastnonminimal code isn't so fast sometimes!
--   Why not?
-- To do: either fix code to work better, or implement nonF4 version which is fast, 
--   or implement a switch select in cases where F4 code might not be so good.
restart
n = 3
c = 3
m = n+c-1
S = ZZ/101[x_1..x_(n*m)]
I = minors(n, genericMatrix(S,x_1,m,n));

apply(4, i->elapsedTime res(S^1/I^(i+1)))
apply(4, i->elapsedTime res(S^1/I^(i+1), FastNonminimal => true))

elapsedTime M = S^1/I^4;
elapsedTime gens gb presentation M;

M = S^1/I^4;
elapsedTime res M

M = S^1/I^4;
elapsedTime res(M, Strategy=>0)

M = S^1/I^4;
elapsedTime res(M, Strategy=>2)

M = S^1/I^4;
elapsedTime res(M, Strategy=>3)

-- FastNonminimal is quite slow here.  Why?
M = S^1/I^4;
elapsedTime res(M, FastNonminimal=>true)
J = I^4;
elapsedTime res(J, FastNonminimal=>true)
