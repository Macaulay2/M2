V = Proj(QQ[a,b,c]/ideal(b^2*c-a^2*(a+c)))
genus V
R = ZZ/101[x_0..x_5];
M = random(R^4, R^{4:-1});
I = minors(3, M+transpose(M));
V = Proj(R/I);
genus V
