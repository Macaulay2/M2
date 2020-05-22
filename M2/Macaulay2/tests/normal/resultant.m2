R = QQ[a..d,x]
for i to 11 do for j to 5 do assert ( resultant (c*(x-a)^i, d*(x-b)^j, x) == d^i * c^j * (a-b)^(i*j) )

R = QQ[p_0..p_3, q_0..q_4, x]
P = sum (4, i -> p_i * x^i)
Q = sum (5, i -> q_i * x^i)
sylvesterMatrix (P,Q,x)
assert( entries oo == {{p_3,p_2,p_1,p_0,0,0,0},{0,p_3,p_2,p_1,p_0,0,0},{0,0,p_3,p_2,p_1,p_0,0},{0,0,0,p_3,p_2,p_1,p_0},{q_4,q_3,q_2,q_1,q_0,0,0},{0,q_4,q_3,q_2,q_1,q_0,0},{0,0,q_4,q_3,q_2,q_1,q_0}} )
