R = ZZ/101[x_0..x_3];
M = matrix{{x_0,x_1,x_2},{x_1,x_2,x_3}}
V = Proj(R/minors(2,M));
codim V
