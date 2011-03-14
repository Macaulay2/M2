QQ[e,f,g,h];
F=-3*e*f+9/2*g*h-1/48*h^4;
G=e^3+f^3+27/4*g^2+3/4*e*f*h^2-5/8*g*h^3-1/864*h^6;
dis=ideal (4*F^3+27*G^2);
sindis=ideal jacobian dis;
sindis = trim sindis
primaryDecomposition sindis

R = QQ[x,y,z]
I = ideal(
    x^2+x*y^2*z-2*x*y+y^4+y^2+z^2,
    -x^3*y^2+x*y^2*z+x*y*z^3-2*x*y+y^4,
    -2*x^2*y+x*y^4+y*z^4-3)
time primaryDecomposition I

