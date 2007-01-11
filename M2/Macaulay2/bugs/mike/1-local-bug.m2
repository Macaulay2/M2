-- see also bugs/mike/5-localgb-slow.m2

kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = z^4*t^3*y + t^7
H = x^5 + y^4
M = matrix{{F,G,H}}
time gens(gb1 = gb M);
G = reverse flatten entries gens gb1
netList G
(a1 = (z^10*x^5*G#0 - t*G#4)) % gb1
(a1 = (z^10*x^5*G#0 - t*G#4)) % gb1
