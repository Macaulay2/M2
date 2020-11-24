///////////////////////////////////////////////////////////////////////
//
//            Examples for primary decomposition
//
///////////////////////////////////////////////////////////////////////

needsPackage "PrimaryDecomposition"

-- 'clear' is now defined in primdecomp.m2, for ease of use.

(I = clear I; time res coker gens I)           -- seconds
(I = clear I; time primaryDecomposition I)     -- 
(I = clear I; time minimalPrimes I)                -- 
(I = clear I; time associatedPrimes(I,Strategy=>1)) -- 
(I = clear I; time associatedPrimes(I,Strategy=>2)) -- 
(I = clear I; time radical I)                  -- 
(I = clear I; time topComponents I)                      -- 
(I = clear I; time removeLowestDimension I)    -- 
(I = clear I; time (I : ideal vars R))         -- 
(I = clear I; time saturate(I, ideal vars R))  -- 
    
////////////////////////////////////////////////////////////////////////
// a starter: 5 Komponenten
//6 variables, 12 generators in degree 5 and 17 in degree 6 
//min_ass_prim_l        :   0 sec
//prim_dec_tree_charsets:   2 sec
//newMinAssPrimes       :     sec
//minAssPrimes          :   1 sec
//decomp                :   6 sec/15
////////////////////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e,f]
I = ideal(
    a^2*c*d*f^2,
    b^2*c*d*f^2,
    a^2*b*d*f^2,
    b^3*d*f^2,
    a^3*d*f^2,
    a*b^2*d*f^2,
    a^2*c*d*e,
    b^2*c*d*e,
    a^2*b*d*e,
    b^3*d*e,
    a^3*d*e,
    a*b^2*d*e,
    a^2*c*d^2,
    b^2*c*d^2,
    a^2*b*d^2,
    b^3*d^2,
    a^3*d^2,
    a*b^2*d^2,
    a^2*c^2*f^2,
    b^2*c^2*f^2,
    a^2*b*c*f^2,
    b^3*c*f^2,
    a^3*c*f^2,
    a*b^2*c*f^2,
    a^2*b^2*f^2,
    b^4*f^2,
    a^3*b*f^2,
    a*b^3*f^2,
    a^4*f^2)
    
(I = clear I; time primaryDecomposition I)     -- 4.89 [24.67 seconds] (5 components) (2020: 0.0026501 seconds)
(I = clear I; time minimalPrimes I)                --  .3 [1.92 -  2.99]
(I = clear I; time radical I)                  -- 2.48 [30.48]
(I = clear I; time topComponents I)                      --  .22 [2.67]
(I = clear I; time removeLowestDimension I)    --  .46 [14.81]
(I = clear I; time (I : ideal vars R))         --  .09 [0.57]
(I = clear I; time saturate(I, ideal vars R))  --  .07 [0.53]
(I = clear I; time associatedPrimes I)                      -- 1.35
elapsedTime primaryDecomposition comodule I     -- 0.867021 seconds

///////////////////////////////////////////////////////
//Sturmfels: 16 Komponenten
// The permanents of a generic 3x3-matrix
// 9 variables, 9 generators in degree 2
//min_ass_prim_l        :  11 sec
//prim_dec_tree_charsets:  37 sec
//newMinAssPrimes       :     sec
//minAssPrimes          :   9 sec
//decomp                :  30 sec/75
//////////////////////////////////////////////////////
    
R = ZZ/32003[b,s,t,u,v,w,x,y,z]
I = ideal(
    b*v+s*u,
    b*w+t*u,
    s*w+t*v,
    b*y+s*x,
    b*z+t*x,
    s*z+t*y,
    u*y+v*x,
    u*z+w*x,
    v*z+w*y)

(I = clear I; time res coker gens I)           -- .07 [0.76]
(I = clear I; time primaryDecomposition I)     -- 58.76 [256.0 seconds (roughly 89-99 sec with Singular)] (2020: 2.95525 seconds)
                                               --        343.24 seconds now [4/23/97]. what happened?
					       --        with debugging: 296.08 sec????
					       --        with fastgb.m2: 223.87!
					       --        fastgb.m2, sorting mingens: 219.83, 213.57
(I = clear I; time minimalPrimes I)                -- 4.14  [33.72 (used to be) 48.3 seconds]
                                               --        (the char sets takes about 20 sec)
					       --         Singular time = 31 seconds
(I = clear I; time radical I)                  -- ????
(I = clear I; time topComponents I)                      --  .59
(I = clear I; time removeLowestDimension I)    --  .46  [2.49]
(I = clear I; time (I : ideal vars R))         --  .12  [0.59]
(I = clear I; time saturate(I, ideal vars R))  --  .13  [1.62]
elapsedTime primaryDecomposition comodule I      -- 1.82998 seconds
    
////////////////////////////////////////////////////////////
//J_S/Y:  11 Komponenten
// 3 variables, 5 generators of degree 5,
// 3 of degree 6 and 2 of degree 7 (inhomogeneous)
//min_ass_prim_l        :   1 sec
//prim_dec_tree_charsets:   3 sec
//newMinAssPrimes       :     sec
//minAssPrimes          :   0 sec
//decomp                :   4 sec/10
////////////////////////////////////////////////////////////
    
R = ZZ/32003[x,y,z]
I = ideal(
    x*y^2*z^2-x*y^2*z+x*y*z^2-x*y*z,
    x*y^3*z+x*y^2*z,
    x*y^4-x*y^2,
    x^2*y*z^2-x^2*y*z,
    x^2*y^3-x^2*y^2,
    x^4*z^3-x^4*z^2+2*x^3*z^3-2*x^3*z^2+x^2*z^3-x^2*z^2,
    x^2*y^2*z,
    x^4*y*z+x^3*y*z,
    2*x^4*y^2+6*x^3*y^2+6*x^2*y^2+x*y^3+x*y^2,
    x^5*z+x^4*z^2+x^4*z+2*x^3*z^2-x^3*z+x^2*z^2-x^2*z,
    x^6*y+3*x^5*y+3*x^4*y+x^3*y)

(I = clear I; time res coker gens I)           --  .13 [0.47 seconds]
(I = clear I; time primaryDecomposition I)     -- 6.61 [22.54 seconds] (2020: 0.435623 seconds)
(I = clear I; time minimalPrimes I)                --  .23 [0.8 sec]
(I = clear I; time radical I)                  -- 2.1  [8.48 sec]
(I = clear I; time topComponents I)                      --  .4  [0.98 sec]
(I = clear I; time removeLowestDimension I)    --  .68 [3.58 sec]
(I = clear I; time (I : ideal vars R))         --  .07 [0.27 sec]
(I = clear I; time saturate(I, ideal vars R))  --  .09 [0.41 sec]
elapsedTime primaryDecomposition comodule I      -- 0.532823 seconds
    
/////////////////////////////////////////////////////////////
//ST_S/Y:  4 Komponenten 
// 9 variables, 4 generators of degree 2
//min_ass_prim_l        :   3 sec
//prim_dec_tree_charsets:  24 sec
//newMinAssPrimes       :     sec
//minAssPrimes          :   1 sec
//decomp                :  12 sec/29
////////////////////////////////////////////////////////////
    
R = ZZ/32003[b,s,t,u,v,w,x,y,z]
I = ideal(
    s*u-b*v,
    t*v-s*w,
    v*x-u*y,
    w*y-v*z)
    
(I = clear I; time res coker gens I)           -- 0.00 [0.05]
(I = clear I; time primaryDecomposition I)     -- 20.28 [136.46 (with lots of debug output)] (2020: 0.950533 seconds)
     	       	    	      	   	       --        fastgb 86.32
     	       	    	      	   	       --        Singular92i: 45 sec
(I = clear I; time minimalPrimes I)                --  1.11  [7.19 sec]
(I = clear I; time radical I)                  --  5.01
(I = clear I; time topComponents I)                      --   .18
(I = clear I; time removeLowestDimension I)    --   .2
(I = clear I; time (I : ideal vars R))         --   .06
(I = clear I; time saturate(I, ideal vars R))  --   .04
elapsedTime primaryDecomposition comodule I      -- 0.699619 seconds
    
/////////////////////////////////////////////////////////////
//Bu_S/Y  (Wang2): 10 Komponenten 
// Example Bu von S/Y, 8 variables, 1 generator of degree 1,
//1 of degree 2, 2 of degree 3, 4 of degree 4 (inhomogeneous)
//min_ass_prim_l        :   3 sec
//prim_dec_tree_charsets: 231 sec
//newMinAssPrimes       :     sec
//minAssPrimes          :     sec
//decomp                :     sec/
/////////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e,f,g,h]
I = ideal(
    h+f+e-d-a,
    2*f*b+2*e*c+2*d*a-2*a^2-a-1,
    3*f*b^2+3*e*c^2-3*d*a^2-d+3*a^3+3*a^2+4*a,
    6*g*e*b-6*d*a^2-3*d*a-d+6*a^3+6*a^2+4*a,
    4*f*b^3+4*e*c^3+4*d*a^3+4*d*a-4*a^4-6*a^3-10*a^2-a-1,
    8*g*e*c*b+8*d*a^3+4*d*a^2+4*d*a-8*a^4-12*a^3-14*a^2-3*a-1,
    12*g*e*b^2+12*d*a^3+12*d*a^2+8*d*a-12*a^4-18*a^3-14*a^2-a-1,
    -24*d*a^3-24*d*a^2-8*d*a+24*a^4+36*a^3+26*a^2+7*a+1)

elapsedTime primaryDecomposition I      -- 19.2477 seconds
elapsedTime primaryDecomposition minimalPresentation I      -- 17.8112 seconds
elapsedTime associatedPrimes comodule I      -- 11.7443 seconds
elapsedTime primaryDecomposition comodule I      -- +29.3355 seconds
elapsedTime associatedPrimes comodule minimalPresentation I      -- 5.41741 seconds
elapsedTime primaryDecomposition comodule minimalPresentation I      -- +31.4491 seconds

/////////////////////////////////////////////////////////////
//Go_S/Y:   3 Komponenten (equidimensional)
//17 variables, 19 generators of degree 2 (inhomogeneous)
//min_ass_prim_l        :   4 sec
//prim_dec_tree_charsets:  25 sec
//newMinAssPrimes       :     sec
//minAssPrimes          : 125 sec
//decomp                : 125 sec/240
////////////////////////////////////////////////////////////
//M2 initial time       : 151.47 sec !! Need to fix this badly!!
//////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e,f,g,h,j,k,l,x,n,o,p,q,y]
I = ideal(
    d*k,
    e*g+d*j+c*k+l,
    b*h,
    e*l,
    b*f+b*g+a*h+d*h+b*k+h+o,
    a*f+d*f+a*g+d*g+c*h+e*h+b*j+a*k+d*k+b*l+f+g+k+x+n+q,
    c*f+e*f+c*g+e*g+a*j+d*j+c*k+e*k+a*l+d*l+j+l+p+y-1,
    c*j+e*j+c*l+e*l,
    e*j+c*l+2*e*l,
    e*f+e*g+d*j+c*k+2*e*k+a*l+2*d*l+l+y,
    d*f+d*g+e*h+a*k+2*d*k+b*l+k+q,
    d*h+b*k,
    e*k+d*l,
    2*c*j+e*j+c*l,
    c*f+2*c*g+e*g+a*j+d*j+c*k+2*j+l+p,
    a*g+d*g+c*h+b*j+f+2*g+k+n,
    b*g+h,
    e*j+c*l,
    d*g+k)
    
(I = clear I; time res coker gens I)           -- TOO LONG at moment
(I = clear I; time primaryDecomposition I)     -- alas, doesn't yet work... (2020: 1.44395 seconds)
(I = clear I; time minimalPrimes I)                -- 35.68
(I = clear I; time radical I)                  -- 
(I = clear I; time topComponents I)                      -- 
(I = clear I; time removeLowestDimension I)    -- 
(I = clear I; time (I : ideal vars R))         -- 29.9 sec
(I = clear I; time saturate(I, ideal vars R))  -- 48.48 sec

elapsedTime associatedPrimes comodule I      -- ?, > 11 seconds
M = comodule minPres I
elapsedTime associatedPrimes M      -- 0.0695831 seconds
elapsedTime primaryDecomposition M      -- +0.0521103 seconds
    
/////////////////////////////////////////////////////////////
//Wang:
//17 variables, 4 generators of degree 1,7 of degree 2 
//min_ass_prim_l        :  17 sec
//prim_dec_tree_charsets:  nach einer Stunde bei 200 MB abgebrochen
//newMinAssPrimes       :     sec
//minAssPrimes          :     sec
//decomp                :     sec/
////////////////////////////////////////////////////////////
Factorisation over algebraic function field required!
    
R = ZZ/32003[a,b,c,d,f,g,h,k,l,s,t,u,v,w,x,y,z]
I = ideal(
    -a*b-a*d+2*a*h,
    a*d-b*d-c*f-2*a*h+2*b*h+2*c*k,
    a*b-a*d-2*b*h+2*d*h-2*c*k+2*f*k+2*g*l,
    a*c-2*c*s-a*t+2*b*t,
    a*c-c*s-2*a*t+b*t,
    -d-3*s+4*u,
    -f-3*t+4*v,
    -g+4*w,
    -a+2*x,
    -b^2-c^2+2*b*x+2*c*y,
    -d^2-f^2-g^2+2*d*x+2*f*y+2*g*z)
    
-- 6 components
elapsedTime primaryDecomposition minimalPresentation I      -- 131.985 seconds
elapsedTime associatedPrimes comodule I      -- ?, > 100 seconds
elapsedTime associatedPrimes comodule minimalPresentation I      -- 20.9314 seconds
elapsedTime primaryDecomposition comodule minimalPresentation I      -- +3.19141 seconds
    
//////////////////////////////////////////////////////////
//Horrocks:  6 Komponenten
// The zero-scheme of a general section of the
// Horrocks bundle on P^5
// 6 variables, 14 generators of degree 4, 19 of degree 5
//min_ass_prim_l        :  13 sec
//prim_dec_tree_charsets:  16 sec
//newMinAssPrimes       :     sec
//minAssPrimes          :   5 sec
//decomp                :   5 sec/13
//////////////////////////////////////////////////////////
//M2 initial time       : 67 sec
//////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e,f]
I = ideal(
    a*d*e*f+3/2*b*e^2*f-1/2*c*e*f^2,
    a*d^2*f+5/4*b*d*e*f+1/4*c*d*f^2,
    a*d^2*e+3/4*b*d*e^2+7/4*c*d*e*f,
    a*c*d*e+3/2*b*c*e^2-1/2*c^2*e*f,
    a*c*d^2+5/4*b*c*d*e+1/4*c^2*d*f,
    a*b*d*f+3/2*b^2*e*f-1/2*b*c*f^2,
    a*b*d*e+3/4*b^2*e^2-a*c*d*f+1/2*b*c*e*f-1/4*c^2*f^2,
    a*b*d^2+3/4*b^2*d*e+7/4*b*c*d*f,
    a*b*c*d+3/2*b^2*c*e-1/2*b*c^2*f,
    a^2*d*f+5/4*a*b*e*f+1/4*a*c*f^2,
    a^2*d*e+3/4*a*b*e^2+7/4*a*c*e*f,
    a^2*d^2-9/16*b^2*e^2+2*a*c*d*f-9/8*b*c*e*f+7/16*c^2*f^2,
    a^2*c*d+5/4*a*b*c*e+1/4*a*c^2*f,
    a^2*b*d+3/4*a*b^2*e+7/4*a*b*c*f,
    b*c^3*d+1/4*c*d^3*e,
    b^2*c^2*e-1/3*b*c^3*f+2/3*c*d^2*e*f,
    b^2*c^2*d-1/2*a*d^4-3/8*b*d^3*e-1/8*c*d^3*f,
    b^3*c*e-1/3*b^2*c^2*f+2/3*b*d^2*e*f,
    b^3*c*d+3/4*b*d^3*f,
    a*c^3*e-1/3*c*d*e^3,
    a*c^3*d-3/4*b*c^3*e-1/2*c*d^2*e^2+1/4*c^4*f,
    a*b*c^2*f-c*d*e*f^2,
    a*b*c^2*e-c*d*e^2*f,
    a*b^2*c*f-b*d*e*f^2,
    a*b^3*f-3*b*d*f^3,
    a*b^3*d+3/4*b^4*e-1/4*b^3*c*f-3/2*b*d^2*f^2,
    a^2*c^2*e-2/3*a*d*e^3-1/2*b*e^4+1/6*c*e^3*f,
    a^2*b*c*f+3/2*b*e^2*f^2-1/2*c*e*f^3,
    a^2*b*c*e+3/2*b*e^3*f-1/2*c*e^2*f^2,
    a^2*b^2*f-2*a*d*f^3+3/2*b*e*f^3-1/2*c*f^4,
    a^3*c*e+4/3*a*e^3*f,
    a^3*b*f+4*a*e*f^3,
    a^4*d+3/4*a^3*b*e+1/4*a^3*c*f-2*a*e^2*f^2)
    
(I = clear I; time res coker gens I)           --   .03
(I = clear I; time primaryDecomposition I)     -- 11.58 [67.26 sec (lots of debug output)] (2020: 0.645988 seconds)
     	       	    	      	   	       --        fastgb 54.94
(I = clear I; time minimalPrimes I)                --  6.65 [25.69 sec]
(I = clear I; time radical I)                  --  ????
(I = clear I; time topComponents I)                      --   .58
(I = clear I; time removeLowestDimension I)    --   .46
(I = clear I; time (I : ideal vars R))         --   .17
(I = clear I; time saturate(I, ideal vars R))  --   .15

elapsedTime primaryDecomposition comodule I;      -- 0.548052 seconds
    
//////////////////////////////////////////////////////////
//Arnborg-Lazard:   14 Komponenten
//min_ass_prim_l        :   4 sec
//prim_dec_tree_charsets:  20 sec
//newMinAssPrimes       :     sec
//minAssPrimes          :   1 sec
//decomp                :   1 sec/2
//////////////////////////////////////////////////////////
//M2 initial time       : core dump
//////////////////////////////////////////////////////////

R = ZZ/32003[x,y,z]
I = ideal(
    x^2*y*z+x*y^2*z+x*y*z^2+x*y*z+x*y+x*z+y*z,
    x^2*y^2*z+x^2*y*z+x*y^2*z^2+x*y*z+x+y*z+z,
    x^2*y^2*z^2+x^2*y^2*z+x*y^2*z+x*y*z+x*z+z+1)
XXX    
(I = clear I; time res coker gens I)           --   .25
(I = clear I; time primaryDecomposition I)     --  PROBLEMS: figure it out. (2020: 3.78713 seconds)
(I = clear I; time minimalPrimes I)                --  3.01 [12.0 sec (or actually around 144 sec??)]
(I = clear I; time radical I)                  -- 16.03
(I = clear I; time topComponents I)                      --  2.58 
(I = clear I; time removeLowestDimension I)    --   .5
(I = clear I; time (I : ideal vars R))         --   .17
(I = clear I; time saturate(I, ideal vars R))  --   .17

elapsedTime primaryDecomposition comodule I      -- 0.696629 seconds

restart
load "c3primedec.m2"
time newminimalPrimes I
gbTrace 3
C = PDinitialize(I,1)
time PDnext C;
time PDdonode C;
time PDnext C;
peek C

//////////////////////////////////////////////////////////
//Schwarz:  12  Komponenten
//min_ass_prim_l        :  nach 2 Stunden und 150 MB abgebrochen
//prim_dec_tree_charsets:  nach 2 Stunden und 150 MB abgebrochen
//newMinAssPrimes       :     sec
//minAssPrimes          : 28  sec
//decomp                : 28  sec/38
//////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e,h]
I = ideal(
    -a*b-b^2-2*d*e-2*c*h,
    -a*c-2*b*c-e^2-2*d*h,
    -c^2-a*d-2*b*d-2*e*h,
    -2*c*d-a*e-2*b*e-h^2,
    -d^2-2*c*e-a*h-2*b*h)
    
elapsedTime primaryDecomposition I      -- 3.9397 seconds
elapsedTime primaryDecomposition comodule I      -- 1.5859 seconds
    
//////////////////////////////////////////////////////////
//Katsura4:  9  Komponenten
//min_ass_prim_l        :   5 sec
//prim_dec_tree_charsets:  10 sec
//newMinAssPrimes       :     sec
//minAssPrimes          :   2 sec
//decomp                :   2 sec/4
//////////////////////////////////////////////////////////
//Factorisation over algebraic function field required!

R = ZZ/32003[w,t,u,x,y,z]
I = ideal(
    2*x^2+2*y^2+2*z^2+2*t^2+u^2-u,
    x*y+2*y*z+2*z*t+2*t*u-t,
    2*x*z+2*y*t+t^2+2*z*u-z,
    2*x*t+2*z*t+2*y*u-y,
    2*x+2*y+2*z+2*t+u-1)

elapsedTime primaryDecomposition I      -- 1.57136 seconds
elapsedTime primaryDecomposition minimalPresentation I      -- 1.44268 seconds
elapsedTime primaryDecomposition comodule I      -- 0.501794 seconds
elapsedTime primaryDecomposition comodule minimalPresentation I      -- 0.448015 seconds
    
//////////////////////////////////////////////////////////
//Katsura5:  6  Komponenten
//min_ass_prim_l        :  nach 70 Minuten bei 5 MB abgebrochen
//prim_dec_tree_charsets:  nach 70 Minuten bei 5 MB abgebrochen  
//newMinAssPrimes       :     sec
//minAssPrimes          :  15 sec
//decomp                :  15 sec/
//////////////////////////////////////////////////////////
    
R = ZZ/32003[x,y,z,t,u,v,w]
I = ideal( 
    2*x^2+2*y^2+2*z^2+2*t^2+2*u^2+v^2-v, 
    x*y+y*z+2*z*t+2*t*u+2*u*v-u, 
    2*x*z+2*y*t+2*z*u+u^2+2*t*v-t, 
    2*x*t+2*y*u+2*t*u+2*z*v-z, 
    t^2+2*x*v+2*y*v+2*z*v-y, 
    2*x+2*y+2*z+2*t+2*u+v-1)

elapsedTime primaryDecomposition I      -- 1.61477 seconds
elapsedTime primaryDecomposition minimalPresentation I      -- 1.66211 seconds
elapsedTime primaryDecomposition comodule I      -- 1.34095 seconds
elapsedTime primaryDecomposition comodule minimalPresentation I      -- 0.792373 seconds
    
//////////////////////////////////////////////////////////
//Cyclic roots 5 homog:  25  Komponenten
//min_ass_prim_l        :  31 sec
//prim_dec_tree_charsets:  nach 2 Stunden und 295 MB abgebrochen
//newMinAssPrimes       :     sec
//minAssPrimes          :  55 sec
//decomp                :  85 sec/124
//////////////////////////////////////////////////////////
//Factorisation over algebraic function field required!
    
R = ZZ/32003[a,b,c,d,e,h]
I = ideal(
    a+b+c+d+e,
    d*e+1*c*d+1*b*c+1*a*e+1*a*b,
    c*d*e+1*b*c*d+1*a*d*e+1*a*b*e+1*a*b*c,
    b*c*d*e+1*a*c*d*e+1*a*b*d*e+1*a*b*c*e+1*a*b*c*d,
    a*b*c*d*e-h^5)

(I = clear I; time res coker gens I)           -- 6.66 sec (is a complete intersection).
(I = clear I; time primaryDecomposition I)     -- (2020: 23.2279 seconds)
(I = clear I; time minimalPrimes I)                -- 47.32 (probably missing components...)
(I = clear I; time radical I)                  -- 
(I = clear I; time topComponents I)                      -- 
(I = clear I; time removeLowestDimension I)    -- 
(I = clear I; time (I : ideal vars R))         -- 
(I = clear I; time saturate(I, ideal vars R))  -- 

elapsedTime primaryDecomposition minimalPresentation I      -- 20.3554 seconds
elapsedTime primaryDecomposition comodule I      -- 17.8611 seconds
elapsedTime primaryDecomposition comodule minimalPresentation I      -- 14.8029 seconds
    
//////////////////////////////////////////////////////////
//Cyclic roots 5:  20  Komponenten
//min_ass_prim_l        :  nach 90 min und 85 MB abgebrochen
//prim_dec_tree_charsets:  nach 90 min und 85 MB abgebrochen     
//newMinAssPrimes       :     sec
//minAssPrimes          :  12 sec
//decomp                :  12 sec/28
//////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e]
I = ideal(
    a+b+c+d+e,
    d*e+1*c*d+1*b*c+1*a*e+1*a*b,
    c*d*e+1*b*c*d+1*a*d*e+1*a*b*e+1*a*b*c,
    b*c*d*e+1*a*c*d*e+1*a*b*d*e+1*a*b*c*e+1*a*b*c*d,
    a*b*c*d*e-1)
    
elapsedTime primaryDecomposition I      -- 9.21096 seconds
elapsedTime primaryDecomposition minimalPresentation I      -- 9.1964 seconds
elapsedTime primaryDecomposition comodule I      -- 4.12646 seconds
elapsedTime primaryDecomposition comodule minimalPresentation I      -- 4.00272 seconds
    
//////////////////////////////////////////////////////////
//Cyclic roots 4:  8  Komponenten
//min_ass_prim_l        :   1 sec
//prim_dec_tree_charsets:   3 sec
//newMinAssPrimes       :     sec
//minAssPrimes          :   1 sec
//decomp                :   3 sec/8
//////////////////////////////////////////////////////////
//Factorisation over algebraic function field required!
    
R = ZZ/32003[a,b,c,d]
I = ideal(
    a+b+c+d,
    1*c*d+1*b*c+1*a*b+a*d,
    1*b*c*d+1*a*b*c+a*b*d+a*c*d,
    1*a*b*c*d-1)
    
elapsedTime primaryDecomposition I      -- 0.739036 seconds
elapsedTime primaryDecomposition minimalPresentation I      -- 0.738273 seconds
elapsedTime primaryDecomposition comodule I      -- 0.304861 seconds
elapsedTime primaryDecomposition comodule minimalPresentation I      -- 0.290187 seconds
    
//////////////////////////////////////////////////////////
//Kahn4:  1  Komponente
//min_ass_prim_l        :   1  sec
//prim_dec_tree_charsets:  10  sec
//newMinAssPrimes       :      sec
//minAssPrimes          :   4  sec
//decomp                :   4  sec/9
//////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e]
I = ideal(
    a^4-b^4,
    b^4-c^4,
    c^4-d^4,
    d^4-e^4,
    a^3*b+b^3*c+c^3*d+d^3*e+e^3*a)

(I = clear I; time res coker gens I)           -- amazingly hard...C.I. so could write it down
     	       	    	      	   	       -- but slanted degree: 261.09 (min betti nums buggy)
(I = clear I; time primaryDecomposition I)     -- fastgb 27.31 (2020: 3.54374 seconds)
(I = clear I; time minimalPrimes I)                -- 2.35
(I = clear I; time radical I)                  -- 
(I = clear I; time topComponents I)                      -- 
(I = clear I; time removeLowestDimension I)    -- 
(I = clear I; time (I : ideal vars R))         -- 
(I = clear I; time saturate(I, ideal vars R))  -- 

elapsedTime primaryDecomposition comodule I      -- 0.0883182 seconds / 3.29674 seconds
    
//////////////////////////////////////////////////////////
//Iarrobino:  1  Komponente
//min_ass_prim_l        :   4  sec
//prim_dec_tree_charsets:   4  sec
//newMinAssPrimes       :      sec
//minAssPrimes          :   0  sec
//decomp                :   0  sec/0
//////////////////////////////////////////////////////////

R = ZZ/32003[u,v,w,x,y,z]
I = ideal(
    x*y+y^2+u*z-w*z-x*z+y*z, 
    x^2-y^2+u*z-w*z-x*z+z^2,
    w*y, 
    w*x-u*z+y*z,
    w^2-y^2+u*z-z^2, 
    v*z-y*z-z^2, 
    v*y-y^2-w*z+x*z+y*z+z^2, 
    v*x-y^2+u*z+y*z-z^2, 
    v*w-y^2+u*z+y*z-z^2, 
    v^2+y^2+u*z-x*z-y*z-z^2, 
    u*y+y^2+y*z, 
    u*x-y^2+w*z-x*z-z^2,
    u*w-y^2+u*z+y*z-z^2, 
    u*v-y^2-x*z+y*z, 
    u^2+y*z)

(I = clear I; time res coker gens I)           -- 8.48
(I = clear I; time primaryDecomposition I)     -- fastgb: 6.0 sec (2020: 0.0548841 seconds)
     	       	    	      	   	       -- could notice: codim=numvars, homog, therefore
					       -- already primary to max ideal
(I = clear I; time minimalPrimes I)                -- 5.36
(I = clear I; time radical I)                  -- 
(I = clear I; time topComponents I)                      -- 
(I = clear I; time removeLowestDimension I)    -- 
(I = clear I; time (I : ideal vars R))         -- 
(I = clear I; time saturate(I, ideal vars R))  -- 

elapsedTime primaryDecomposition comodule I      -- 0.0074666 seconds
    
//////////////////////////////////////////////////////////
//Marko:  30  Komponenten
//min_ass_prim_l        :  763 sec
//prim_dec_tree_charsets:  778 sec
//newMinAssPrimes       :      sec
//minAssPrimes          :    9 sec
//decomp                :    9 sec/22
//////////////////////////////////////////////////////////
//Factorisation over algebraic function field required!
    
R = ZZ/32003[a,b,c,d,e,f,g,h,k,o] 
I = ideal( 
    o+1,
    k^4+k,
    h*k,
    h^4+h,
    g*k,
    g*h,
    g^3+h^3+k^3+1,
    f*k,
    f^4+f,
    e*h,
    e*f,
    f^3*h^3+e^3*k^3+e^3+f^3+h^3+k^3+1,
    e^3*g+f^3*g+g,
    e^4+e,
    d*h^3+d*k^3+d,
    d*g,
    d*f,
    d*e,
    d^3+e^3+f^3+1,
    e^2*g^2+d^2*h^2+c,
    f^2*g^2+d^2*k^2+b,
    f^2*h^2+e^2*k^2+a)
    
elapsedTime primaryDecomposition I      -- 4.23382 seconds
elapsedTime primaryDecomposition minimalPresentation I      -- 4.13288 seconds
elapsedTime primaryDecomposition comodule I      -- 2.65694 seconds
elapsedTime primaryDecomposition comodule minimalPresentation I      -- 1.87443 seconds
    
/////////////////////////////////////////////////////////////
//Theo0:  11 Komponenten
//min_ass_prim_l        :   5 sec
//prim_dec_tree_charsets:  10 sec
//newMinAssPrimes       :     sec
//minAssPrimes          :   6 sec
//decomp                :   8 sec/19
/////////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e,f,g]
I = ideal(
    d*f,
    d*e^2,
    d^2*e,
    c*f+d*e,
    c*e,
    c^2*d+d^3*g,
    b*e^2+b*f^2*g,
    b*d*e,
    b^5*d^3*g^2,
    b^5*c^2*g^2,
    a*f+b*e,
    a*e-b*f*g,
    a^2*b*g+b^3*g^2,
    a^2*b^3*d^3,
    a^2*b^3*c^2,
    a^4*d^3*g^2,
    b*f,
    f^2,
    a^4*c^2*g)

elapsedTime primaryDecomposition I      -- 1.51251 seconds
elapsedTime primaryDecomposition comodule I      -- 0.790623 seconds
    
//////////////////////////////////////////////////////////
//Theo1:  4  Komponenten
//min_ass_prim_l        :  403 sec
//prim_dec_tree_charsets: nach 9 Stunden und 7 MB abgebrochen
//newMinAssPrimes       :      sec
//minAssPrimes          :    1 sec
//decomp                :    6 sec/9
//////////////////////////////////////////////////////////
    
R = ZZ/32003[b,c,d,e,f,g,u,v,w,x,y]
//wp(3,2,1,3,2,1,3,2,1,3,2);
I = ideal(
    -2*d*g*y+d*e+4*c*f+b*g,
    -2*d*w*y+d*u+4*c*v+b*w,
    -d*g*x-4*d*f*y-4*c*g*y+2*c*e+2*b*f,
    -d*w*x-4*d*v*y-4*c*w*y+2*c*u+2*b*v,
    -2*d*f*x-2*c*g*x+b*e,
    -2*d*v*x-2*c*w*x+b*u)
    
elapsedTime primaryDecomposition I      -- 1.17009 seconds
elapsedTime associatedPrimes comodule I      -- 69.4223 seconds
elapsedTime primaryDecomposition comodule I      -- +0.116673 seconds
    
//////////////////////////////////////////////////////////
//Theo2:  3  Komponenten
//min_ass_prim_l        :    2 sec
//prim_dec_tree_charsets:    3 sec
//newMinAssPrimes       :      sec
//minAssPrimes          :    0 sec
//decomp                :    1 sec/2
//////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e,f,g]
I = ideal(
    e*a-f*b*g,
    f*a+b*e,
    e*c-f*d*g,
    f*c+d*e)

elapsedTime primaryDecomposition I      -- 0.198419 seconds
elapsedTime primaryDecomposition comodule I      -- 0.24645 seconds
    
//////////////////////////////////////////////////////////
//Theo3:  4  Komponenten
//min_ass_prim_l        :    2 sec
//prim_dec_tree_charsets:    5 sec
//newMinAssPrimes       :      sec
//minAssPrimes          :    1 sec
//decomp                :    2 sec/7
//////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e,f,g]
I = ideal(
    f*e^2+f^3*g,
    f*c+d*e,
    f*a+b*e)

elapsedTime primaryDecomposition I      -- 0.308439 seconds
elapsedTime primaryDecomposition comodule I      -- 0.62386 seconds

//////////////////////////////////////////////////////////
//Theo4:  4  Komponenten
//min_ass_prim_l        :   20 sec
//prim_dec_tree_charsets:   nach einer Stunde abgebrochen
//newMinAssPrimes       :      sec
//minAssPrimes          :    4 sec
//decomp                :    4 sec/8
//////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e,f,g,h]
I = ideal(
    a*f+b*e+c*d-c*f*g,
    a*e+b*d-b*f*g-c*e*g-c*f*h,
    a*d-b*f*h-c*e*h)

elapsedTime primaryDecomposition I      -- 0.359853 seconds
elapsedTime primaryDecomposition comodule I      -- 0.277906 seconds
    
//////////////////////////////////////////////////////////
//Becker-Niermann:  2  Komponenten
//min_ass_prim_l        : abgebrochen nach 1 Stunde und 33 MB
//prim_dec_tree_charsets: abgebrochen nach 1 Stunde und 33 MB 
//newMinAssPrimes       :      sec
//minAssPrimes          :   25 sec
//decomp                :   25 sec/
//////////////////////////////////////////////////////////
    
R = ZZ/32003[x,y,z]
I = ideal(
    x^2+x*y^2*z-2*x*y+y^4+y^2+z^2,
    -x^3*y^2+x*y^2*z+x*y*z^3-2*x*y+y^4,
    -2*x^2*y+x*y^4+y*z^4-3)

elapsedTime primaryDecomposition I;      -- 0.396958 seconds
elapsedTime primaryDecomposition comodule I;      -- 0.378588 seconds
    
//////////////////////////////////////////////////////////
//Vershelde-Cools:  3  Komponenten
//min_ass_prim_l        :    2 sec
//prim_dec_tree_charsets:    2 sec
//newMinAssPrimes       :      sec
//minAssPrimes          :    0 sec
//decomp                :    0 sec/
//////////////////////////////////////////////////////////
    
R = ZZ/32003[x,y,z]
I = ideal(
    17*y^6+49*y^5-9*y^4+41*x^2*z+12*y^3+33*y^2+11*y+73,
    42*y^5+33*y^4+21*x^2*y+17*x^2*z+63*y^3+11*x*y+77*y^2+91*y+1,
    26*y^2+44*x*z+12*y+9)
    
elapsedTime primaryDecomposition I      -- 0.271382 seconds
elapsedTime primaryDecomposition comodule I      -- 0.1308 seconds
    
//////////////////////////////////////////////////////////
//Caprasse4 :  19  Komponenten
//min_ass_prim_l        :    7 sec
//prim_dec_tree_charsets:   38 sec
//newMinAssPrimes       :      sec
//minAssPrimes          :    4 sec
//decomp                :    4 sec/
//////////////////////////////////////////////////////////
//Factorisation over algebraic function field required!
    
R = ZZ/32003[x,y,z,t]
I = ideal(
    y^2*z+2*x*y*t-2*x-z,
    -x^3*z+4*x*y^2*z+4*x^2*y*t+2*y^3*t+4*x^2-10*y^2+4*x*z-10*y*t+2,
    2*y*z*t+x*t^2-x-2*z,
    -x*z^3+4*y*z^2*t+4*x*z*t^2+2*y*t^3+4*x*z+4*z^2-10*y*t-10*t^2+2)
    
elapsedTime primaryDecomposition I      -- 6.13223 seconds
elapsedTime primaryDecomposition comodule I      -- 1.85292 seconds
    
//////////////////////////////////////////////////////////
//Moeller :  3  Komponenten
//min_ass_prim_l        :    1 sec
//prim_dec_tree_charsets:    1 sec
//newMinAssPrimes       :      sec
//minAssPrimes          :    1 sec
//decomp                :    1 sec/
//////////////////////////////////////////////////////////

R = ZZ/32003[x,y,z,t,w]    
I = ideal(
    x^2+2*y^2+2*z^2+2*t^2-x, 
    2*x*y+2*y*z+2*z*t-y, 
    y^2+2*x*z+2*y*t-z, 
    x+2*y+2*z+2*t-1)

elapsedTime primaryDecomposition I      -- 0.263748 seconds
elapsedTime primaryDecomposition comodule I      -- 0.107378 seconds
    
//////////////////////////////////////////////////////////
//Cassou :  2  Komponenten
//min_ass_prim_l        :  163 sec
//prim_dec_tree_charsets:  nach 45 Minuten bei 16 MB abgebrochen
//newMinAssPrimes       :      sec
//minAssPrimes          :    4 sec
//decomp                :    4 sec/
//////////////////////////////////////////////////////////
Factorisation over algebraic function field required!
    
R = ZZ/32003[b,c,d,e,w]
I = ideal(
    15*b^4*c*d^2+6*b^4*c^3+21*b^4*c^2*d-144*b^2*c-8*b^2*c^2*e-28*b^2*c*d*e-648*b^2*d+36*b^2*d^2*e+9*b^4*d^3-120,
    30*c^3*b^4*d-32*d*e^2*c-720*d*b^2*c-24*c^3*b^2*e-432*c^2*b^2+576*e*c-576*d*e+16*c*b^2*d^2*e+16*d^2*e^2+16*e^2*c^2+9*c^4*b^4
    +5184+39*d^2*b^4*c^2+18*d^3*b^4*c-432*d^2*b^2+24*d^3*b^2*e-16*c^2*b^2*d*e-240*c,
    216*d*b^2*c-162*d^2*b^2-81*c^2*b^2+5184+1008*e*c-1008*d*e+15*c^2*b^2*d*e-15*c^3*b^2*e-80*d*e^2*c+40*d^2*e^2+40*e^2*c^2,
    261+4*d*b^2*c-3*d^2*b^2-4*c^2*b^2+22*e*c-22*d*e)
    
elapsedTime primaryDecomposition I      -- 0.210382 seconds
elapsedTime primaryDecomposition comodule I      -- 0.299217 seconds
    
//////////////////////////////////////////////////////////
//mat3-2 :  2  Komponenten
//min_ass_prim_l        :  202 sec
//prim_dec_tree_charsets:  nach 100 Minuten bei 7 MB abgebrochen
//newMinAssPrimes       :      sec
//minAssPrimes          :    1 sec
//decomp                :    6 sec/
//////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e,f,g,h,i]
I = ideal( 
    a^2+b*d+c*g, 
    a*b+b*e+c*h, 
    a*c+b*f+c*i, 
    a*d+d*e+f*g, 
    b*d+e^2+f*h, 
    c*d+e*f+f*i, 
    a*g+d*h+g*i, 
    b*g+e*h+h*i, 
    c*g+f*h+i^2)

(I = clear I; time res coker gens I)           -- 1.32 (pd=9)
(I = clear I; time primaryDecomposition I)     -- (2020: 1.24622 seconds)
(I = clear I; time minimalPrimes I)                -- 481.42 sec
(I = clear I; time radical I)                  -- 
(I = clear I; time topComponents I)                      -- 
(I = clear I; time removeLowestDimension I)    -- 
(I = clear I; time (I : ideal vars R))         --  .75
(I = clear I; time saturate(I, ideal vars R))  -- 1.25 sec

elapsedTime primaryDecomposition comodule I      -- 0.281031 seconds

-- Here is the result of minimalPrimes I above:
-- {ideal | a+e+i fg-di -ce+bf bg+eh+hi ch-bi -eg+dh bd+e2+ei cg+ei+i2 fh-ei cd+ef+fi |}    

//////////////////////////////////////////////////////////
//I8_S/Y :  9  Komponenten
//min_ass_prim_l        :    2 sec
//prim_dec_tree_charsets:   10 sec
//newMinAssPrimes       :      sec
//minAssPrimes          :    1 sec
//decomp                :   39 sec/
//////////////////////////////////////////////////////////
    
R = ZZ/32003[a,b,c,d,e,f,g,h,j,k]
I = ideal( 
    (a-k)^9,
    (a-k)^8*(a-b),
    (a-k)^7*(a-c),
    (a-k)^6*(a-d),
    (a-k)^5*(a-e),
    (a-k)^4*(a-f),
    (a-k)^3*(a-g),
    (a-k)^2*(a-h),
    (a-k)*(a-j))

(I = clear I; time res coker gens I)           --  1.09
(I = clear I; time primaryDecomposition I)     -- 35.11 sec (2020: 0.433618 seconds)
(I = clear I; time minimalPrimes I)                --   .66 sec
(I = clear I; time radical I)                  -- 
(I = clear I; time topComponents I)                      -- 
(I = clear I; time removeLowestDimension I)    -- 
(I = clear I; time (I : ideal vars R))         -- 
(I = clear I; time saturate(I, ideal vars R))  -- 

elapsedTime primaryDecomposition comodule I      -- ?, > 10000 seconds
    
//////////////////////////////////////////////////////////
//parametric curve :  1  Komponenten
//min_ass_prim_l        : 3030 sec
//prim_dec_tree_charsets: nach 2 Stunden mit 100 MB abgebrochen
//newMinAssPrimes       :      sec
//minAssPrimes          :    0 sec
//decomp                :    0 sec/
//////////////////////////////////////////////////////////
    
R = ZZ/32003[x,y,z,t]
I = ideal(
    t^10-x,
    t^31-t^6-t-y,
    t^8-z)

elapsedTime primaryDecomposition I      -- 0.0269395 seconds
elapsedTime primaryDecomposition comodule I      -- 0.0185982 seconds
    
//////////////////////////////////////////////////////////
//Gerdt/Ge_S/Y : 9  Komponenten
//min_ass_prim_l        :  266 sec
//prim_dec_tree_charsets:  272 sec
//newMinAssPrimes       :      sec
//minAssPrimes          :    3 sec
//decomp                :    9 sec/
//////////////////////////////////////////////////////////
    
R = ZZ/32003[t,u,v,w,x,y,z]
I = ideal(
    y*w-1/2*z*w+t*w,
    -2/7*u*w^2+10/7*v*w^2-20/7*w^3+t*u-5*t*v+10*t*w,
    2/7*y*w^2-2/7*z*w^2+6/7*t*w^2-y*t+z*t-3*t^2,
    -2*v^3+4*u*v*w+5*v^2*w-6*u*w^2-7*v*w^2+15*w^3+42*y*v,
    -14*z*v-63*y*w+21*z*w-42*t*w+147*x,
    -9/7*u*w^3+45/7*v*w^3-135/7*w^4+2*z*v^2-2*t*v^2-4*z*u*w+10*t*u*w-2*z*v*w-28*t*v*w+4*z*w^2+86*t*w^2-42*y*z+14*z^2+42*y*t-14*z*t-21*x*u+105*x*v-315*x*w,
    6/7*y*w^3-9/7*z*w^3+36/7*t*w^3-2*x*v^2-4*y*t*w+6*z*t*w-24*t^2*w+4*x*u*w+2*x*v*w-4*x*w^2+56*x*y-35*x*z+84*x*t,
    2*u*v*w-6*v^2*w-u*w^2+13*v*w^2-5*w^3+14*y*w-28*t*w,
    u^2*w-3*u*v*w+5*u*w^2+14*y*w-28*t*w,
    -2*z*u*w-2*t*u*w+4*y*v*w+6*z*v*w-2*t*v*w-16*y*w^2-10*z*w^2+22*t*w^2+42*x*w,
    28/3*y*u*w+8/3*z*u*w-20/3*t*u*w-88/3*y*v*w-8*z*v*w+68/3*t*v*w+52*y*w^2+40/3*z*w^2-44*t*w^2-84*x*w,
    -4*y*z*w+10*y*t*w+8*z*t*w-20*t^2*w+12*x*u*w-30*x*v*w+15*x*w^2,
    -y^2*w+1/2*y*z*w+y*t*w-z*t*w+2*t^2*w-3*x*u*w+6*x*v*w-3*x*w^2,
    8*x*y*w-4*x*z*w+8*x*t*w)
    
elapsedTime primaryDecomposition I      -- 1.45349 seconds
elapsedTime primaryDecomposition minimalPresentation I      -- 1.55215 seconds
elapsedTime associatedPrimes comodule I      -- 7.88573 seconds
elapsedTime associatedPrimes comodule minimalPresentation I      -- 2.16607 seconds
elapsedTime primaryDecomposition comodule I      -- +4.59695 seconds
elapsedTime primaryDecomposition comodule minimalPresentation I      -- +1.3416 seconds

//////////////////////////////////////////////////////////
//Liu : 1  Komponenten
//min_ass_prim_l        :    1 sec
//prim_dec_tree_charsets:  nach 1 Stunde und 6 MB abgebrochen
//newMinAssPrimes       :      sec
//minAssPrimes          :    1 sec
//decomp                :    1 sec/
//////////////////////////////////////////////////////////
    
R = ZZ/32003[t,x,y,z,a]
I = ideal(
    y*(z-t)-x+a,
    z*(t-x)-y+a,
    t*(x-y)-z+a,
    x*(y-z)-t+a)

-- 3 components
elapsedTime primaryDecomposition I      -- 0.236712 seconds
elapsedTime primaryDecomposition minimalPresentation I      -- 0.185428 seconds
elapsedTime primaryDecomposition comodule I      -- 0.126721 seconds
elapsedTime primaryDecomposition comodule minimalPresentation I      -- 0.129961 seconds
-- Note: Singular's primdecGTZ() gives extremely complicated output
    
//////////////////////////////////////////////////////////
//Moeller2 : 27  Komponenten
//min_ass_prim_l        :   14 sec
//prim_dec_tree_charsets:   58 sec
//newMinAssPrimes       :      sec
//minAssPrimes          :    4 sec
//decomp                :   23 sec/
//////////////////////////////////////////////////////////
R = ZZ/32003[a,b,c,d,u,v,w,x]
I = ideal(
    a+b+c+d,
    u+v+w+x,
    3*(a*b+a*c+a*d+b*c+b*d+c*d)+2,
    a*(v+w+x)+b*(u+w+x)+c*(u+v+x)+d*(u+v+w),
    u*(b*c+b*d+c*d)+v*(a*c+a*d+c*d)+w*(a*b+a*d+b*d)+x*(a*b+a*c+b*c),
    a*b*c+a*b*d+a*c*d+b*c*d,
    a*b*c*x+a*b*w*d+a*v*c*d+u*b*c*d)

(I = clear I; time res coker gens I)           -- 
(I = clear I; time primaryDecomposition I)     -- (2020: 7.48868 seconds)
(I = clear I; time minimalPrimes I)                -- 
(I = clear I; time radical I)                  -- 
(I = clear I; time topComponents I)                      -- 
(I = clear I; time removeLowestDimension I)    -- 
(I = clear I; time (I : ideal vars R))         -- 
(I = clear I; time saturate(I, ideal vars R))  -- 

elapsedTime primaryDecomposition minimalPresentation I      -- 6.22361 seconds
elapsedTime primaryDecomposition comodule I      -- 20.8393 seconds
elapsedTime primaryDecomposition comodule minimalPresentation I      -- 5.09512 seconds

//////////////////////////////////////////////////////////
//Hunecke1 :  12  Komponenten
//min_ass_prim_l        :       sec
//prim_dec_tree_charsets:       sec
//newMinAssPrimes       :       sec
//minAssPrimes          :   235 sec
//decomp                :  1722 sec/
//////////////////////////////////////////////////////////
R = ZZ/32003[x,y,u,s,t,MonomialSize=>16]
I = ideal(
    x^15,
    y^15,
    u^15,
    u^5-x*y*(x-y)*(s*x-t*y))
    
(I = clear I; time res coker gens I)           -- 
(I = clear I; time primaryDecomposition I)     -- 
(I = clear I; time minimalPrimes I)                -- 
(I = clear I; time radical I)                  -- 
(I = clear I; time topComponents I)                      -- 
(I = clear I; time removeLowestDimension I)    -- 
(I = clear I; time (I : ideal vars R))         -- 
(I = clear I; time saturate(I, ideal vars R))  -- 

elapsedTime primaryDecomposition I;      -- 17.8788 seconds
elapsedTime associatedPrimes comodule I      -- 2.43017 seconds
elapsedTime primaryDecomposition comodule I;      -- +242.126 seconds
    
//////////////////////////////////////////////////////////
//Huneke2 :  12  Komponenten
//min_ass_prim_l        :       sec
//prim_dec_tree_charsets:       sec
//newMinAssPrimes       :       sec
//minAssPrimes          :       sec
//decomp                :       sec/
//////////////////////////////////////////////////////////
R = ZZ/3[x,y,u,s,t]
I = ideal(
    x^27,
    y^27,
    u^27,
    u^5-x*y*(x-y)*(s*x-t*y))

(I = clear I; time res coker gens I)           -- 14.66
(I = clear I; time primaryDecomposition I)     -- seconds
(I = clear I; time minimalPrimes I)                -- 
(I = clear I; time radical I)                  -- 
(I = clear I; time topComponents I)                      -- 
(I = clear I; time removeLowestDimension I)    -- 
(I = clear I; time (I : ideal vars R))         -- 33.27
(I = clear I; time saturate(I, ideal vars R))  -- 

elapsedTime primaryDecomposition I      -- 1696.92 seconds
elapsedTime associatedPrimes comodule I      -- 760 seconds
elapsedTime primaryDecomposition comodule I      -- +43513.2 seconds

//////////////////////////////////////////////////////////
//Wang1 : 2  Komponenten
//min_ass_prim_l        :    1 sec
//prim_dec_tree_charsets:    1 sec
//newMinAssPrimes       :      sec
//minAssPrimes          :    0 sec
//decomp                :    1 sec/
//////////////////////////////////////////////////////////
//Factorisation over algebraic function field required!
    
R = ZZ/32003[x,y,z,t]
I = ideal(
    t^2+x*t^2-y*t-x*y*t+x*y+3*y,
    x*t+z-x*y,
    z*t-2*y^2-x*y-1)
    
elapsedTime primaryDecomposition I      -- 0.0855847 seconds
elapsedTime primaryDecomposition comodule I      -- 0.0686396 seconds
    
//////////////////////////////////////////////////////////
//Wang3 :   Komponenten
//min_ass_prim_l        :     sec
//prim_dec_tree_charsets:     sec
//newMinAssPrimes       :     sec
//minAssPrimes          :     sec
//decomp                :     sec/
//////////////////////////////////////////////////////////
//Factorisation over algebraic function field required!
    
R = ZZ/32003[y,a,b,c,m,n,o,p,q,s]
    p*o*l*y p^1=(o+n)*(n+m)*(m+o)*p^2*q^2*s^2)
    p*o*l*y p^2=p^1+c*(q^2*m-1)+b*(s^2*o-1)+a*(p^2*n-1))
I = ideal(
    j*a*c*o*b(p^2)+i*d*e*a*l(y^2-p^1))
    
//////////////////////////////////////////////////////////
//Wang4 :   Komponenten
//min_ass_prim_l        :     sec
//prim_dec_tree_charsets:     sec
//newMinAssPrimes       :     sec
//minAssPrimes          :     sec
//decomp                :     sec/
//////////////////////////////////////////////////////////
    
R = ZZ/32003[x,y,z,t]
I = ideal(
    x^2+y^2+z^2-t^2,
    x*y+z^2-1,
    x*y*z-x^2-y^2-z+1)

-- 1 component    
elapsedTime primaryDecomposition I      -- 0.0582853 seconds
elapsedTime primaryDecomposition comodule I      -- 0.0602829 seconds
    
/////////////////////////////////////////////////////////////////////////////
    
    
//test
ring s=32003,(g,w,e,f,u,v,x,y),lp;
I = ideal(
    (x^2)*w^3+(4*u*y^2-4*v*x*y)*w^2+(-4*u^2*y+6*u*v*x)*w+(u^3+8*u*v^2*y-8*v^3*x),
    (x^2)*g^3+(4*e*y^2-4*f*x*y)*g^2+(-4*e^2*y+6*e*f*x)*g+(e^3+8*e*f^2*y-8*f^3*x))
    
    
// theo algemein==============================
    
  int p=32003;  //characteristic
      i*n*t n=3)
      i*n*t i)
      r*i*n*g r=p,(x,e(0..n-1),a(0..n-1),c(0..n-1),b(0..n-2)),l*p)
      p*o*l*y e=e(0)+e(n-1)*x^(n-1))
      p*o*l*y a=a(0)+a(n-1)*x^(n-1))
      p*o*l*y c=c(0)+c(n-1)*x^(n-1))
      p*o*l*y b=b(0))
      f*o*r(i=1)i<=n-2)i++)
      {
         e=e+e(i)*x^i)
         a=a+a(i)*x^i)
         c=c+c(i)*x^i)
         b=b+b(i)*x^i)
      }
      i*d*e*a*l q=x^n+b)
      a*t*t*r*i*b(q,"i*sSB",1))
      p*o*l*y q*a=r*e*d*u*c*e(e*a,q))
      p*o*l*y q*c=r*e*d*u*c*e(e*c,q))
    
      r*i*n*g s=(p,e(0..n-1),a(0..n-1),c(0..n-1),b(0..n-2)),(x),l*p)
      p*o*l*y q*a=i*m*a*p(r,q*a))
      p*o*l*y q*c=i*m*a*p(r,q*c))
      m*a*t*r*i*x m*a=c*o*e*f(q*a,x))
      m*a*t*r*i*x m*c=c*o*e*f(q*c,x))
      i*d*e*a*l i*d)
      i*d*e*a*l j*d)
      f*o*r(i=1)i<=n*c*o*l*s(m*a))i++)
      {
         i*d=i*d,m*a[2,i],m*c[2,i])
         j*d=j*d,m*a[2,i])
        
      }
      i*d=s*i*m*p*l*i*f*y(i*d,2))
      j*d=s*i*m*p*l*i*f*y(j*d,2))
    
      s*e*t*r*i*n*g r)
      i*d*e*a*l i*d=i*m*a*p(s,i*d))
      i*d*e*a*l j*d=i*m*a*p(s,j*d))
    
       LIB "p*r*i*m*d*e*c.l*i*b")
       i*n*t a*a=t*i*m*e*r)
       l*i*s*t p*r= d*e*c*o*m*p(i*d))
       t*i*m*e*r-a*a)
       t*e*s*tPr*i*m*a*r*y( p*r, i*d))
       a*a=t*i*m*e*r)
       l*i*s*t p*r= n*e*wMi*nAs*sPr*i*m*e*s(i*d))
       t*i*m*e*r-a*a)
       a*a=t*i*m*e*r)
       l*i*s*t p*r= m*i*nAs*sPr*i*m*e*s(i*d))
       t*i*m*e*r-a*a)
    
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PrimaryDecomposition.installed "
-- End:
