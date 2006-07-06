R = ZZ/101[x_0..x_4];
M = matrix{{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}}
V = Proj(R/minors(2,M));
degree V
dim V
dim minors(2,M)
