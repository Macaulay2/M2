setRandomSeed()
K = ZZ/103; 
A = K[x,y,z];
J = ideal(x^3,y^4,z^5)
B = A/J;
f = random (B^3, B^{-2,-3})
toString f
assert( f == matrix {{-28*x^2-31*x*y-24*y^2-4*x*z-49*y*z-19*z^2, -44*x^2*y-4*x*y^2-49*y^3+30*x^2*z-51*x*y*z+51*y^2*z+23*x*z^2-19*y*z^2+42*z^3},
     {47*x^2-6*x*y-49*y^2+9*x*z+47*y*z-25*z^2, 16*x^2*y-9*x*y^2-31*y^3+34*x^2*z-2*x*y*z-16*y^2*z-23*x*z^2+14*y*z^2+50*z^3},
     {-36*x^2-44*x*y-18*y^2+11*x*z-18*y*z+21*z^2, -36*x^2*y+28*x*y^2-21*y^3-x^2*z-8*x*y*z+6*y^2*z+37*x*z^2+27*y*z^2+43*z^3}} )
