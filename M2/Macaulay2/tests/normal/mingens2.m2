R = QQ[x,y,MonomialOrder => Position => Down ]
f = transpose matrix {{0,1-y,x+x^2}, {x,y,0}, {x,0,y}}
g = gens gb (f, StopWithMinimalGenerators=>true, Syzygies=>false, ChangeMatrix=>false)
g = mingens gb (f, StopWithMinimalGenerators=>true, Syzygies=>false, ChangeMatrix=>false)
assert(image g == image f)


R = ZZ/32749[x,y,z,t]
g = map(R^{-1, 0, 0}, R^{-3, -3, -3, -4, -4, -4, -4, -4, -4, -4, -4, -5, -5, -5}, {{-2626*y^2+12125*x*t, -9334*y*t, -1234*y^2+1851*x*t, 8188*t^3, -t^3, -13469*x*y^2-6054*y^2*z+14617*x^2*t+14475*x*z*t-5394*z^2*t, 12428*x*y^2-10359*y^2*z-2709*x^2*t-9244*x*z*t+8408*z^2*t, 3715*x^2*y-7197*x*y*z-13840*y*z^2+11989*t^3, -2626*x^3+6868*x^2*z-6878*x*z^2-6878*y*t^2, -7197*x^2*y-10213*x*y*z-6920*y*z^2-10380*t^3, -1234*x^3-617*x^2*z+617*x*z^2+617*y*t^2, 5647*x*y^2*t-2721*y^2*z*t-12965*x^2*t^2+14112*x*z*t^2+6339*z^2*t^2, -13438*x^4+1514*x^3*z-8548*x^2*z^2+10542*x*z^3+447*z^4-11923*y^3*t-6949*x*y*t^2+9432*y*z*t^2, -1234*x^2*z^2-617*x*z^3+617*z^4+7388*y^3*t-6390*x*y*t^2+6875*y*z*t^2}, {y^3+9299*x*y*t+9700*y*z*t, 4651*y^2*t-14017*x*t^2, 6303*x*y*t-5069*y*z*t, x*y^2*z-13099*y^2*z^2-6550*x^3*t-13100*x^2*z*t-6550*x*z^2*t+13099*z^3*t+16374*y*t^3, x^2*y^2+6549*y^2*z^2-13100*x^3*t+6550*x^2*z*t-13099*x*z^2*t-6549*z^3*t+y*t^3, 3856*x^2*y*t-7009*x*y*z*t+638*y*z^2*t+10865*t^4, -10614*x^2*y*t-14152*x*y*z*t+8891*y*z^2*t+12923*t^4, 5536*x^3*t-15821*x^2*z*t-4050*x*z^2*t-16141*z^3*t+15326*y*t^3, x^3*y+13748*x^2*y*z-4856*x*y*z^2-4848*y^2*t^2, 2768*x^3*t+13344*x^2*z*t+903*x*z^2*t+8304*z^3*t+10591*y*t^3, -1234*x^2*y*z-13840*x*y*z^2-13840*y^2*t^2, x^5+5*x^2*z^3+5*x*z^4-2143*x^2*y*t^2-6894*x*y*z*t^2+8759*y*z^2*t^2-11621*t^5, x^2*y*z^2-10915*x*y*z^3-13636*y*z^4+14316*x*y^2*t^2+15412*y^2*z*t^2+7507*x^2*t^3+11854*x*z*t^3+12742*z^2*t^3, -1234*x*y*z^3-13840*y*z^4+2397*x*y^2*t^2-4016*y^2*z*t^2-9346*x^2*t^3+5500*x*z*t^3+12606*z^2*t^3}, {-16305*y^2*z+8147*x^2*t-x*z*t, y^3-8565*x*y*t-11589*y*z*t, x*y^2-8086*y^2*z-9301*x^2*t+2428*x*z*t, 15914*y^2*t^2-2072*x*t^3+15022*z*t^3, 614*y^2*t^2+13679*x*t^3-9113*z*t^3, x^3*t-6168*x^2*z*t-152*x*z^2*t+727*z^3*t+1116*y*t^3, y^2*z^2+10887*x^2*z*t-16279*x*z^2*t-10556*z^3*t-7591*y*t^3, x^2*y*z-15006*x*y*z^2+8248*y*z^3+3876*y^2*t^2-7519*x*t^3+5093*z*t^3, -12924*x^3*z-2034*x^2*z^2+843*x*z^3-8308*x*y*t^2+843*y*z*t^2, x^3*y+13548*x*y*z^2+4124*y*z^3-6356*y^2*t^2+12615*x*t^3-13828*z*t^3, x^4+10915*x^3*z+807*x^2*z^2+4043*x*z^3+4850*x*y*t^2+4043*y*z*t^2, -11722*x^2*z*t^2+9636*x*z^2*t^2-1929*z^3*t^2+4132*y*t^4, -13262*x^2*z^3-6973*x*z^4+169*z^5-14463*x*y*z*t^2-12512*y*z^2*t^2+12466*t^5, x^3*z^2+10915*x^2*z^3+807*x*z^4+4043*z^5+7655*x*y*z*t^2-15561*y*z^2*t^2-14150*t^5}});
r = map(R^{-1, 0, 0}, R^{-5, -5, -6, -6, -6, -7, -4, -4, -5, -5, -5, -6, -4, -4, -5, -5, -5, -6}, {{y^4-2*x*y^2*t-y^2*z*t+x^2*t^2, x^2*y^2-10915*x*y^2*z-10917*x^3*t+10916*x^2*z*t-10916*x*z^2*t-10916*y*t^3, x*y^2*z^2+11909*x^4*t+5954*x^3*z*t+2977*x^2*z^2*t+11910*x*z^3*t-2978*y^3*t^2+14887*x*y*t^3+11910*y*z*t^3, x*y^3*z-13099*y^3*z^2-6550*x^3*y*t-13100*x^2*y*z*t-6550*x*y*z^2*t+13099*y*z^3*t+13100*y^2*t^3+13099*x*t^4, x^5+5*x^2*z^3+5*x*z^4-3*x*y^3*t-4*y^3*z*t+4*x^2*y*t^2+10*x*y*z*t^2+5*y*z^2*t^2, y^2*z^4-8932*x^4*z*t+11909*x^3*z^2*t+5954*x^2*z^3*t-8934*x*z^4*t-z^5*t+2*x*y^3*t^2-5952*y^3*z*t^2-x^2*y*t^3-2979*x*y*z*t^3-8934*y*z^2*t^3+t^6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, y^4-2*x*y^2*t-y^2*z*t+x^2*t^2, x^2*y^2-10915*x*y^2*z-10917*x^3*t+10916*x^2*z*t-10916*x*z^2*t-10916*y*t^3, x*y^2*z^2+11909*x^4*t+5954*x^3*z*t+2977*x^2*z^2*t+11910*x*z^3*t-2978*y^3*t^2+14887*x*y*t^3+11910*y*z*t^3, x*y^3*z-13099*y^3*z^2-6550*x^3*y*t-13100*x^2*y*z*t-6550*x*y*z^2*t+13099*y*z^3*t+13100*y^2*t^3+13099*x*t^4, x^5+5*x^2*z^3+5*x*z^4-3*x*y^3*t-4*y^3*z*t+4*x^2*y*t^2+10*x*y*z*t^2+5*y*z^2*t^2, y^2*z^4-8932*x^4*z*t+11909*x^3*z^2*t+5954*x^2*z^3*t-8934*x*z^4*t-z^5*t+2*x*y^3*t^2-5952*y^3*z*t^2-x^2*y*t^3-2979*x*y*z*t^3-8934*y*z^2*t^3+t^6, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, y^4-2*x*y^2*t-y^2*z*t+x^2*t^2, x^2*y^2-10915*x*y^2*z-10917*x^3*t+10916*x^2*z*t-10916*x*z^2*t-10916*y*t^3, x*y^2*z^2+11909*x^4*t+5954*x^3*z*t+2977*x^2*z^2*t+11910*x*z^3*t-2978*y^3*t^2+14887*x*y*t^3+11910*y*z*t^3, x*y^3*z-13099*y^3*z^2-6550*x^3*y*t-13100*x^2*y*z*t-6550*x*y*z^2*t+13099*y*z^3*t+13100*y^2*t^3+13099*x*t^4, x^5+5*x^2*z^3+5*x*z^4-3*x*y^3*t-4*y^3*z*t+4*x^2*y*t^2+10*x*y*z*t^2+5*y*z^2*t^2, y^2*z^4-8932*x^4*z*t+11909*x^3*z^2*t+5954*x^2*z^3*t-8934*x*z^4*t-z^5*t+2*x*y^3*t^2-5952*y^3*z*t^2-x^2*y*t^3-2979*x*y*z*t^3-8934*y*z^2*t^3+t^6}});
M = subquotient(g,r);
assert isHomogeneous M

num = degree (M / ideal vars R)	-- the number of minimal generators
assert ( num == 10 )
assert ( num == rank source mingens M )
assert ( num == rank source gens trim M )
assert ( num == rank target presentation prune M )

N = image presentation prune M
num' = degree (N / ideal vars R)	     -- the number of minimal relations when pruned
assert( num' == 26 )
assert ( num' == rank source presentation prune M )

P = image relations M			     -- the number of minimal relations
num'' = degree (P / ideal vars R)
assert( num'' == 18 )
assert( rank source relations trim M ==  num'' )
