-- bug from Frank Moore, email of Feb 20, 2013.
-- this crashes at about 3 GB
restart
r = 3
N = (2*r+1)*(r)
Q = QQ[x_0..x_(N-1)]
M = genericSkewMatrix(Q,(2*r+1))
I = pfaffians(4,M)
R = Q/I
betti prune HH koszul vars R
----------------
end

restart
r = 3
N = (2*r+1)*(r)
Q = QQ[x_0..x_(N-1)]
M = genericSkewMatrix(Q,(2*r+1))
I = pfaffians(4,M)
R = Q/I
C = koszul vars R
HC = HH C
PHC = prune HC
betti PHC

restart
r = 3
N = (2*r+1)*(r)
Q = ZZ/101[x_0..x_(N-1), MonomialSize=>8]
M = genericSkewMatrix(Q,(2*r+1))
I = pfaffians(4,M)
R = Q/I
C = koszul vars R
HC = HH C
PHC = prune HC
betti PHC


CQ = koszul vars Q;
betti CQ
