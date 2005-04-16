R = ZZ/101[a..e];
M = matrix{{a^2*b-c^2, a*b^2-d^3, c^5-d},{a^2*b, b*c*d, c^5}}
ideal M
I = ideal(a^2*b-c^2+c*d, a*b^2-b*d^3, c^5,d+e);
generators I
