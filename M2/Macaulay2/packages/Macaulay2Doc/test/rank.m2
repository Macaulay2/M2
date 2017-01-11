assert ( 8 == rank coker (random (ZZ^11, ZZ^3, MaximalRank => true) * matrix "2,,;,5,;,,11" * random(ZZ^3,ZZ^5,MaximalRank => true)))
assert ( 8 == rank coker (random (QQ^11, QQ^3, MaximalRank => true) * matrix "2,,;,5,;,,11" * random(QQ^3,QQ^5,MaximalRank => true)))
assert ( 3 == rank (random (ZZ^11, ZZ^3, MaximalRank => true) * matrix "2,,;,5,;,,11" * random(ZZ^3,ZZ^5,MaximalRank => true)))
assert ( 3 == rank (random (QQ^11, QQ^3, MaximalRank => true) * matrix "2,,;,5,;,,11" * random(QQ^3,QQ^5,MaximalRank => true)))

R = ZZ/101[x_0..x_4]/(x_0^5 + x_1^5 + x_2^5 + x_3^5 + x_4^5 + 2*x_0*x_1*x_2*x_3*x_4);
M = cokernel map((R)^{{ -2},{ -2},{ -1},{ -1},{ -1},{ -1}},(R)^{{ -4},{ -4}},{{4*x_3^2, x_0^2+10*x_2^2}, {9*x_0^2+x_2^2, x_1^2}, {8*x_2^3, 9*x_2^3}, {2*x_3^3, 7*x_3^3}, {4*x_1^3, 9*x_1^3+x_2^3}, {9*x_1^3, x_1^3+7*x_4^3}});
assert ( rank M == 4 )

end


R = QQ[x,y]
r = rank coker (random (R^11, R^3, MaximalRank => true) * matrix "x3+2,,;,5,;,,x+1" * random(R^3,R^5,MaximalRank => true))
assert ( 3 == r )

-- git issue 359.  For awhile, this gave the incorrect answer of 12.
k = ZZ/101
x = symbol x;
R = k[x_0..x_5]
M = random(R^6, R^{6:-1})
M = M - transpose M
S = R/pfaffians(6,M)
N = sub(M,S)
assert(rank N == 4)
