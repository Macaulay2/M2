-- Yanked from test/engine/raw-localgb.m2

ring R = 101, (t,x,y,z),ds;
poly F = 4*t^2*z+6*z^3*t+3*z^3+t*z;
poly G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7;
poly H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5;
ideal I = F,G,H;
timer=1;
option(prot);
option(teach);
std(I);
-- 
-- [255:2]4(2)s8s13s14.15.16s1721-s2223...24...s(4)25...-.26...-....s27(3).....-28-.....-.29.30.31.32.33.34.35.36.37.38.39.40.41.42.43.44.45.46.47.48.49.50.51.52.53.54.55
-- product criterion:7 chain criterion:3
-- _[1]=tz+4t2z+3z3+6tz3
-- _[2]=y5-12t3yz+3ty2z2-9tyz3-18t2yz3+x8
-- _[3]=t7-32t3x2yz4-32t2xy3z7
-- _[4]=x2yz11+4tz13+49t12z3+46t10z5-33t8z7-26t5x2yz7-33t6z9+27t3x2yz9+45txy3z10+28t4z11-2tx2yz11-32t2z13+23t11z5-28t9z7+13t7z9-39t4x2yz9-11t2xy3z10+19t5z11-8t2x2yz11+21t3z13
-- _[5]=y4z15-23x2yz16+3tyz17+50t13y4z3-8t11y4z5-39t12yz7-14t9y4z7+25t6x2y5z7-16t10yz9+47t7y4z9+31t4x2y5z9-43t2xy7z10-50t8yz11+31t5x2y2z11+50t5y4z11+32t2x2y5z11-16t5x2yz12-14xy7z12-50t6yz13-t3x2y2z13-36t3y4z13+50x2y5z13-24t3x2yz14-42txy4z14+21t4yz15-10ty4z15-9tx2yz16-24t2yz17-26t12y4z5+23t10y4z7-8t11yz9+34t8y4z9-13t5x2y5z9-42x10z11-21t9yz11+34t6y4z11-37t3x2y5z11-28txy7z12+35t7yz13-4t4x2y2z13+14t4y4z13-tx2y5z13-24t4x2yz14+17t2xy4z14-11t5yz15-24t2y4z15-27t2x2yz16+41t3yz17
-- _[6]=y3z19-23x2z20+3tz21-27t12xy6z4+38t12x2y4z5+20t10xy6z6+2t10x2y4z7-39t13x2z8+12t8xy6z8+37t5x3y7z8+35t11x2yz9-13t11y3z9-47t8x2y4z9+19t5x4y5z9-18t11x2z10+12t6xy6z10-19t3x3y7z10-39t12z11+47x12z11+4t9x2yz11-39t9y3z11-37t6x2y4z11-11t3x4y5z11+2tx2y9z11+19t9x2z12-31t6x4yz12-t4xy6z12+29tx3y7z12-16t10z13-38t7x2yz13+33t4x4y2z13+17t7y3z13-19t4x2y4z13-42tx4y5z13+30t7x2z14+4t4x4yz14+45t2xy6z14-50t8z15-7t5x2yz15-26t2x4y2z15+17t5y3z15-21t2x3y3z15+41t2x2y4z15-39t5x2z16+3t2x4yz16-40x3y4z16-28xy6z16-50t6z17+15t3x2yz17-13x4y2z17+7t3y3z17+19x3y3z17+2x2y4z17+20t3x2z18+39x4yz18-42txy3z18+21t4z19-46tx2yz19-8ty3z19+28tx2z20-24t2z21+10t11xy6z6-44t11x2y4z7+t9xy6z8-31t9x2y4z9-8t12x2z10+32t7xy6z10+5t4x3y7z10+2t10x2yz11+31t10y3z11+42t7x2y4z11-22t4x4y5z11+4t2x2y9z11-24t10x2z12+39t5xy6z12-43t2x3y7z12-8t11z13-20t8x2yz13-7t8y3z13+42t5x2y4z13+17t2x4y5z13+26t8x2z14+4t5x4yz14-26t3xy6z14-21t9z15-34t6x2yz15-t3x4y2z15-22t6y3z15+12t3x2y4z15+26t6x2z16-46t3x4yz16+21tx3y4z16+45txy6z16+35t7z17+24t4x2yz17-26tx4y2z17+30t4y3z17+38tx3y3z17+4tx2y4z17-19t4x2z18-23tx4yz18+17t2xy3z18-11t5z19+9t2x2yz19-20t2y3z19+47t2x2z20+41t3z21
-- _[7]=x4z20+35tx2z21-13ty2z21-12t12x3y6z4-28t12x4y4z5-36t10x3y6z6-44t10x4y4z7+50t13x4z8+39t8x3y6z8-6t5x5y7z8+38t11x4yz9-17t11x2y3z9+24t8x4y4z9-14t5x6y5z9-8t11x4z10+39t6x3y6z10+14t3x5y7z10+50t12x2z11-24x14z11+13t9x4yz11-33t12y2z11+50t9x2y3z11+6t6x4y4z11+40t3x6y5z11-44tx4y9z11-14t9x4z12-25t6x6yz12+22t4x3y6z12-32tx5y7z12+49t10x2z13+28t7x4yz13+2t10y2z13-19t4x6y2z13+30t7x2y3z13+14t4x4y4z13+15tx6y5z13+47t7x4z14+13t4x6yz14+20t2x3y6z14-11t8x2z15-48t5x4yz15-19t8y2z15-34t2x6y2z15-37t5x2y3z15-43t2x5y3z15+7t2x4y4z15+50t5x4z16+35t2x6yz16-29x5y4z16+10x3y6z16-11t6x2z17-27t3x4yz17-19t6y2z17-17x6y2z17+36t3x2y3z17-14x5y3z17-44x4y4z17-36t3x4z18-50x6yz18+15tx3y3z18-20txy5z18+43t4x2z19+2tx4yz19+10t4y2z19+31tx2y3z19-10tx4z20+23t2x2z21+3t2y2z21-18t11x3y6z6-42t11x4y4z7-22t9x3y6z8-25t9x4y4z9-26t12x4z10+3t7x3y6z10-9t4x5y7z10-44t10x4yz11+25t10x2y3z11-15t7x4y4z11-21t4x6y5z11+13t2x4y9z11+23t10x4z12-50t5x3y6z12+37t2x5y7z12-26t11x2z13+36t8x4yz13+t11y2z13-48t8x2y3z13-15t5x4y4z13+30t2x6y5z13+34t8x4z14+13t5x6yz14-34t3x3y6z14-43t9x2z15+41t6x4yz15-10t9y2z15+22t3x6y2z15-21t6x2y3z15+39t3x4y4z15+34t6x4z16+2t3x6yz16+43tx5y4z16+20tx3y6z16+38t7x2z17-23t4x4yz17-17t7y2z17-34tx6y2z17-3t4x2y3z17-28tx5y3z17+13tx4y4z17+14t4x4z18+tx6yz18+30t2x3y3z18-40t2xy5z18+40t5x2z19+4t2x4yz19+14t5y2z19-39t2x2y3z19-24t2x4z20+7t3x2z21-43t3y2z21
kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7
H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5
M = matrix{{F,G,H}}
gbTrace=3
time gens gb(M);

-- homogeneous version
R = ZZ/101[h,t,x,y,z]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7
H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5
M = matrix{{F,G,H}}
M = homogenize(M,h)
hf = poincare coker M
R = ZZ/101[h,t,x,y,z,MonomialOrder=>Eliminate 1]
M = substitute(M,R)
installHilbertFunction(M,hf)
gbTrace=3
time gens gb M;
mingens ideal substitute(leadTerm gens gb M, h=>1)
transpose gens gb M

-- in singular.  This is very fast.
ring R = 101, (t,x,y,z),ds;
poly F = 4*t^2*z+6*z^3*t+3*z^3+t*z;
poly G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7;
poly H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5;
ideal I = F,G,H;
timer=1;
option(prot);
std(I);

poly F = 4*t^2*z+6*z^3*t+3*z^3+t*z;
poly G = z^4*t^3*y + t^7;
poly H = x^5 + y^4;

