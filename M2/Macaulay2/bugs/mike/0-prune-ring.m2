-- THIS SEEMS TO WORK NOW (1/26/08)

-- We need prune Ring working for towers.
R = QQ[x, y, z]/(-z^9+x*y^2)
S = R [w_0, w_1, w_2, Degrees => {{1, 1}, {1, 1}, {1, 1}}, Heft => {0, 1}]
I = ideal(z*w_1-y*w_2,z*w_0-x*w_2,y*w_0-x*w_1,x*y*w_1-z^8*w_2,x*w_1^2-z^7*w_2^2,w_0*w_1^2-z^6*w_2^3)
res I -- oops  WORKS NOW (12/12/08)
prune(S/I)
