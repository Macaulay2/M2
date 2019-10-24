
-- load "c:/Berkeley-2017/Workshop-2017-Berkeley/Fsing/TestIdeals.m2"
TEST ///

p=2;
R=ZZ/p[x_1..x_5];
E=matrix {{x_1,x_2,x_2,x_5},{x_4,x_4,x_3,x_1}};
I=minors(2,E);

time assert(isCohenMacaulay(R/I) == true);

Omega=canonicalIdeal(R/I);
time assert(substitute(Omega,R)==ideal(x_1, x_4, x_5));
--u=finduOfIdeal(I,Omega);
time tau=testModule(R/I);
assert((tau#1)==Omega);
assert((tau#2)== x_1^3*x_2*x_3+x_1^3*x_2*x_4+x_1^2*x_3*x_4*x_5+x_1*x_2*x_3*x_4*x_5+x_1*x_2*x_4^2*x_5+x_2^2*x_4^2*x_5+x_3*x_4^2*x_5^2+x_4^3*x_5^2);
assert(substitute( (tau#0):(tau#1),R)==ideal(x_1, x_2, x_3+x_4));
time assert(isFRational(R/I)==false);



S=ZZ/101[a,b,x,y,u,v, MonomialOrder=>ProductOrder{2,4}];
time assert(isCohenMacaulay(S) == true);
J=ideal(x-a^4, y-a^3*b, u-a*b^3, v-b^4);
G=gens gb J;
K=selectInSubring(1,G);
time assert(isCohenMacaulay(S/ideal(K)) == false);

pp=11;
R=ZZ/pp[X_1..X_3];
I=ideal(X_1^3+X_2^3+X_3^3);
tau=testModule(CurrentRing => R/I);
time assert(substitute( tau#0,R)==ideal(X_1, X_2, X_3));
time assert(isFRational(R/I)==false);


///

TEST /// --an easy veronese
    T = ZZ/5[x,y];
    S = ZZ/5[a,b,c,d];
    g = map(T, S, {x^3, x^2*y, x*y^2, y^3});
    R = S/(ker g);
    assert( isCohenMacaulay(R) );
    assert( isFRational(R) );
///

TEST /// --test for weird user inputs
    R = ZZ/11[];
    assert(isFRational(R));
///

TEST /// --an old F-rational but not F-regular Hochster-Huneke example "Tight closure of parameter ideals and splitting in module-Finite extensions"
    T = ZZ/7[x,y,z]/ideal(x^3-y*z*(y+z));
    S = ZZ/7[a,b,c,d,e];
    g = map(T, S, {x, y^3, y^2*z, y*z^2, z^3});
    R = S/(ker g);
    assert(isFRational(R));
    assert(not isFRegular(R));
///
