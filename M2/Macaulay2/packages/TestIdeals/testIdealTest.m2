--tests of test ideals of ambient rings
TEST /// 
    R1 = ZZ/7[x,y,z]/ideal(x^3+y^3+z^3); --elliptic curve
    assert(testIdeal(R1, FrobeniusRootStrategy=>Substitution) == ideal(x,y,z));
    assert(testIdeal(R1, FrobeniusRootStrategy=>MonomialBasis) == ideal(x,y,z));
///

TEST ///    
    R2 = ZZ/3[x,y,z]/ideal(x*y^2-z^2); --pinch point
    assert(testIdeal(R2, FrobeniusRootStrategy=>Substitution) == ideal(y, z));
    assert(testIdeal(R2, FrobeniusRootStrategy=>MonomialBasis) == ideal(y, z));    
///

TEST ///       
    R3 = ZZ/5[x,y,z]/ideal(x*y-z^2); --quadric cone
    assert(testIdeal(R3, FrobeniusRootStrategy=>Substitution) == ideal(sub(1, R3)));
    assert(testIdeal(R3, FrobeniusRootStrategy=>MonomialBasis) == ideal(sub(1, R3)));    
///

TEST ///    
    T4 = ZZ/7[x,y]; --veronese in 2 variables
    S4 = ZZ/7[a,b,c,d,e];
    g = map(T4, S4, {x^4, x^3*y, x^2*y^2, x*y^3, y^4});
    R4 = S4/(ker g);
    assert(testIdeal(R4, FrobeniusRootStrategy=>Substitution) == ideal(sub(1, R4)) );
    assert(testIdeal(R4, FrobeniusRootStrategy=>MonomialBasis) == ideal(sub(1, R4)) );    
///

TEST ///    
    T5 = ZZ/11[x,y,z]; --veronese in 3 variables
    S5 = ZZ/11[a,b,c,d,e,f];
    g = map(T5, S5, {x^2, x*y, x*z, y^2, y*z, z^2});
    R5 = S5/(ker g);
    assert(testIdeal(R5, FrobeniusRootStrategy=>Substitution) == ideal(sub(1, R5)));
    assert(testIdeal(R5, FrobeniusRootStrategy=>MonomialBasis) == ideal(sub(1, R5)));    
///

TEST ///    
    R6 = ZZ/2[x,y,z]/ideal(z^2-x*y*z-x^2*y-x*y^2); --nonstandard D4-singularity in char 2 (Z/2 quotient)
    assert(testIdeal(R6, FrobeniusRootStrategy=>Substitution) == ideal(x,y,z));
    assert(testIdeal(R6, FrobeniusRootStrategy=>MonomialBasis) == ideal(x,y,z));    
///

TEST ///        
    S7 = ZZ/2[xu, yu, zu, xv, yv, zv, xw, yw, zw];
    EP7 = ZZ/2[x,y,z,u,v,w]/ideal(y^2*z+z^2*y+x^3, v^2*w+w^2*v+u^3); --cone over product of supersingular elliptic curves in char 2
    g = map(EP7, S7, {x*u, y*u, z*u, x*v, y*v, z*v, x*w, y*w, z*w});
    R7 = S7/(ker g);
    assert(testIdeal(R7, AssumeDomain=>true) == ideal(xu, yu, zu, xv, yv, zv, xw, yw, zw));
///

TEST /// --test for weird input
    R = ZZ/7[];
    assert(testIdeal(R, FrobeniusRootStrategy=>Substitution) == ideal(sub(1, R)));
    assert(testIdeal(R, FrobeniusRootStrategy=>MonomialBasis) == ideal(sub(1, R)));    
///

 
 --tests of test ideals of polynomials in polynomial rings
TEST ///
    R = ZZ/101[x,y];
    assert(testIdeal(5/6-1/(6*101), y^2-x^3, FrobeniusRootStrategy=>Substitution) == ideal(x,y));
    assert(testIdeal(5/6-1/(6*101), y^2-x^3, FrobeniusRootStrategy=>MonomialBasis) == ideal(x,y));    
    assert(testIdeal(5/6-2/(6*101), y^2-x^3, FrobeniusRootStrategy=>Substitution) == ideal(sub(1, R)));
    assert(testIdeal(5/6-2/(6*101), y^2-x^3, FrobeniusRootStrategy=>MonomialBasis) == ideal(sub(1, R)));        
///

TEST ///
    R = ZZ/13[x,y];
    assert(testIdeal(2/3, y*x*(x+y), FrobeniusRootStrategy=>Substitution) == ideal(x,y));
    assert(testIdeal(2/3, y*x*(x+y), FrobeniusRootStrategy=>MonomialBasis) == ideal(x,y));
    assert(testIdeal(2/3-1/100, y*x*(x+y), FrobeniusRootStrategy=>Substitution) == ideal(sub(1, R)));
    assert(testIdeal(2/3-1/100, y*x*(x+y), FrobeniusRootStrategy=>MonomialBasis) == ideal(sub(1, R)));    
///    

TEST /// --test for weird input
    R = ZZ/7[x];
    assert(testIdeal(1/2, sub(0, R), FrobeniusRootStrategy=>Substitution) == ideal(sub(0, R)));
    assert(testIdeal(1/2, sub(0, R), FrobeniusRootStrategy=>MonomialBasis) == ideal(sub(0, R)));    
///

TEST /// --test the isFRegular function, including in the non-Q-Gorenstein case
    R = ZZ/7[x,y,z]/ideal(x^3+y^3+z^3);
    assert( isFRegular(R) == false);
    assert( isFRegular(R, QGorensteinIndex => infinity) == false);
    assert( isFRegular(R, QGorensteinIndex => infinity, MaxCartierIndex=>20) == false);
    S = ZZ/7[x,y,z, u,v,w];
    I = minors(2, matrix{{x,y,z},{u,v,w}});
    T = S/I;
    assert( isFRegular(T, QGorensteinIndex => infinity, MaxCartierIndex=>30) == true);  
    A = ZZ/11[x,y,z]/ideal(x^2-y^3+z^5);
    assert(isFRegular(A) == true);
///

TEST /// --test the isFRegular function for non-Q-Gorenstein pairs (or at least when we forget the Gorenstein property)
    R = ZZ/7[x,y];
    f = y^2-x^3;
    assert(isFRegular(5/6, f) == false);
    assert(isFRegular(5/6, f, QGorensteinIndex=>infinity) == false);
    assert(isFRegular(5/6, f, QGorensteinIndex=>infinity, DepthOfSearch=> 10) == false);
    assert(isFRegular(5/6-1/100, f, QGorensteinIndex=>infinity, DepthOfSearch=> 10) == true);
///
