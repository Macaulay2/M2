kk = ZZ/101
S = kk[x,y,z,u]
R = S [w_0, w_1, w_2, w_3, w_4, Degrees => {5:{1, 3}}]
J = ideal(x*y*w_2+x*y*w_3+(-z^2+z*u-u^2)*w_4,
    (x*z+x*u)*w_1-y^2*w_4,
    (y*z+y*u)*w_0-x^2*w_4,
    (x*y*z+x*y*u)*w_3-u^3*w_4,
    u^3*w_2-z^3*w_3,
    u^3*w_1-y^3*w_3,
    z^3*w_1-y^3*w_2,
    u^3*w_0-x^3*w_3,
    z^3*w_0-x^3*w_2,
    y^3*w_0-x^3*w_1,
    (z^2+2*z*u+u^2)*w_0*w_1-x*y*w_4^2,
    x^2*u*w_1*w_2+x^2*u*w_1*w_3-3*x*y^2*w_3*w_4+(-y*z*u+2*y*u^2)*w_4^2,
    y^2*u*w_0*w_2+y^2*u*w_0*w_3-3*x^2*y*w_3*w_4+(-x*z*u+2*x*u^2)*w_4^2,
    (z+2*u)*w_0*w_1*w_2+(-2*z-u)*w_0*w_1*w_3+(-z+u)*w_4^3,
    x^2*w_0*w_1^2*w_2^2+2*x^2*w_0*w_1^2*w_2*w_3+x^2*w_0*w_1^2*w_3^2-9*y*u*w_0*w_1*w_3*w_4^2-5*x^2*w_1*w_2*w_4^3+13*x^2*w_1*w_3*w_4^3+(4*y*z-5*y*u)*w_4^5,
    y^2*w_0^2*w_1*w_2^2+2*y^2*w_0^2*w_1*w_2*w_3+y^2*w_0^2*w_1*w_3^2-9*x*u*w_0*w_1*w_3*w_4^2-5*y^2*w_0*w_2*w_4^3+13*y^2*w_0*w_3*w_4^3+(4*x*z-5*x*u)*w_4^5,
    u*w_0^2*w_1^2*w_2^2+5*u*w_0^2*w_1^2*w_2*w_3+(-9*z-5*u)*w_0^2*w_1^2*w_3^2-2*u*w_0*w_1*w_2*w_4^3+(-9*z+4*u)*w_0*w_1*w_3*w_4^3+u*w_4^6,
    w_0^3*w_1^3*w_2^3+3*w_0^3*w_1^3*w_2^2*w_3+3*w_0^3*w_1^3*w_2*w_3^2+w_0^3*w_1^3*w_3^3-3*w_0^2*w_1^2*w_2^2*w_4^3+21*w_0^2*w_1^2*w_2*w_3*w_4^3-3*w_0^2*w_1^2*w_3^2*w_4^3+3*w_0*w_1*w_2*w_4^6+3*w_0*w_1*w_3*w_4^6-w_4^9)
(J',F) = flattenRing J
R' = ring J'
time res F J -- very fast
time res J -- was very long

end

restart
load "/Users/mike/src/Snowbird/ReesAlgebra2.m2"
kk = ZZ/101
S=kk[x,y,z,u]
j=ideal(x^3,y^3,z^3,u^3,x*y*z+x*y*u)
jR=reesIdeal j
f=(flattenRing jR)_1
time res (f jR) -- less than .01 sec
time res jR --more than 100 sec (I didn't wait.)


