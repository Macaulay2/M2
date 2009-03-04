-- code to add to IntegralClosure2
-- isS2, isSk, isR1, isNormal
-- S2ification
-- canonicalIdeal (2 versions)
-- ringFromFractions (assumes the fractions form a ring?)
-- parametersInI, vasconcelos, endomorphisms, randomMinors
-- integralClosure Ideal

-- Example Leonard1 -----------------------------------------------
S = QQ[x,y]
I = ideal((y^2-y-x/3)^3-y*x^4*(y^2-y-x/3)-x^11);
R = S/I
time R' = integralClosure2 R
icFractions2 R

-- Example vanHoeij1 ----------------------------------------------
S = QQ[x,y]
F = poly"y10+(-2494x2+474)y8+(84366+2042158x4-660492x2)y6
           +(128361096x4-4790216x2+6697080-761328152x6)y4
	   +(-12024807786x4-506101284x2+15052058268x6+202172841+134266087241x8)y2
	   +34263110700x4-228715574724x6+5431439286x2+201803238-9127158539954x10-3212722859346x8"
R = S/F
time R' = integralClosure2 R
icFractions2 R
-- Example vanHoeij2 ----------------------------------------------
S = QQ[x,y]
F = poly"y20+y13x+x4y5+x3(x+1)2"
R = S/F
time R' = integralClosure2 R

-- Example vanHoeij3 ----------------------------------------------
S = QQ[x,y]
F = poly"y30+y13x+x4y5+x3(x+1)2"
R = S/F
R' = integralClosure2 R

-- Example vanHoeij4 ----------------------------------------------
S = QQ[x,y]
F = poly"y40+y13x+x4y5+x3(x+1)2"
R = S/F
R' = integralClosure2 R

-- Example H ------------------------------------------------------
-- Compute the integral closure of this ring
-- source: this is the second step of the integral closure of the Rees algebra
--  S = k[a,b,c][f1t,f2t,f3t], where the fi are the derivatives of
--  a^3 + random (homog) quartic in 3 variables.
-- In the first step, we computed Hom((a,b,c),(a,b,c)), in S, obtaining one new
-- fraction, here represented by z_(0,0).
-- Original plan: compute the integral closure JJf of the ideal J(f), and see if 
-- J(f) is contained in mm*JJf, where
--   mm = (a,b,c).
--   J(f) = ideal jacobian f
--   f = a^3 + random quartic poly in a,b,c. (or other f which are not
--     quasi-homog).
-- To be quasi-homog: check that sub(JJf:f, all vars to 0) is 0.  If not, then
--     f is quasi-homogeneous.
kk = ZZ/32003
R = kk[z_(0,0), w_0, w_1, w_2, a..c, 
     Degrees => {{1, 5}, 3:{1, 2}, 3:{0, 1}}, 
     --Heft => {-1, 1}, 
     --DegreeRank => 2,
     MonomialOrder => {MonomialSize => 16,
	  GRevLex => {4}, 
	  GRevLex => {3:1}, 
	  GRevLex => {3:1}}
     ]
P = ideal(w_1*a^3+14207*w_1*a^2*b-10581*w_1*a*b^2-2057*w_1*b^3-1979*w_1*a^2*c+12475*w_1*a*b*c-2052*w_1*b^2*c-15832*w_1*a*c^2+14966*w_1*b*c^2+12153*w_1*c^3-14407*w_2*a^3-5956*w_2*a^2*b-4087*w_2*a*b^2+14935*w_2*b^3-14207*w_2*a^2*c-10841*w_2*a*b*c+6171*w_2*b^2*c+9764*w_2*a*c^2+2052*w_2*b*c^2+5679*w_2*c^3,
    w_0*a^2*b+8648*w_0*a*b^2+7004*w_0*b^3+6746*w_0*a^2*c+1160*w_0*a*b*c-8737*w_0*b^2*c+14144*w_0*a*c^2-11118*w_0*b*c^2+9772*w_0*c^3-1614*w_1*a^2*b+3638*w_1*a*b^2-7580*w_1*b^3-1840*w_1*a^2*c-4227*w_1*a*b*c-15236*w_1*b^2*c-534*w_1*a*c^2-3839*w_1*b*c^2-5811*w_1*c^3-11398*w_1*a^2-10362*w_2*a^2*b-13314*w_2*a*b^2-12412*w_2*b^3+1614*w_2*a^2*c-7278*w_2*a*b*c+14092*w_2*b^2*c+11369*w_2*a*c^2+14656*w_2*b*c^2-3435*w_2*c^3+3593*w_2*a^2,
    w_0*a^3-13200*w_0*a*b^2-10558*w_0*b^3+6584*w_0*a^2*c+13900*w_0*a*b*c-15130*w_0*b^2*c-12803*w_0*a*c^2+1584*w_0*b*c^2+10363*w_0*c^3+15950*w_1*a^2*b-221*w_1*a*b^2-1035*w_1*b^3-5571*w_1*a^2*c+15361*w_1*a*b*c-10440*w_1*b^2*c+1827*w_1*a*c^2+7561*w_1*b*c^2-10863*w_1*c^3-3794*w_1*a^2+2921*w_2*a^3-12084*w_2*a^2*b+8312*w_2*a*b^2-11276*w_2*b^3-15953*w_2*a^2*c+442*w_2*a*b*c-15698*w_2*b^2*c+1737*w_2*a*c^2+3490*w_2*b*c^2+12415*w_2*c^3-8125*w_2*a^2,
    z_(0,0)*c+8283*w_0*b^4+13415*w_0*a*b^2*c+15929*w_0*b^3*c+14791*w_0*a^2*c^2-10280*w_0*a*b*c^2+14886*w_0*b^2*c^2+1439*w_0*a*c^3-4175*w_0*b*c^3+15249*w_0*c^4-2282*w_1*a^2*b^2-9576*w_1*a*b^3-9726*w_1*b^4-3179*w_1*a^2*b*c+13861*w_1*a*b^2*c+12885*w_1*b^3*c-14005*w_1*a^2*c^2-11058*w_1*a*b*c^2-11922*w_1*b^2*c^2+2190*w_1*a*c^3+4207*w_1*b*c^3+3883*w_1*c^4-15097*w_1*a^2*b+5432*w_1*a*b^2-11827*w_1*b^3-4440*w_1*a^2*c+9826*w_1*a*b*c+13577*w_1*b^2*c-5941*w_1*a*c^2+12926*w_1*b*c^2-12427*w_1*c^3+9693*w_2*a^2*b^2-12726*w_2*a*b^3+6044*w_2*b^4+3560*w_2*a^2*b*c+6989*w_2*a*b^2*c+15573*w_2*b^3*c-8419*w_2*a^2*c^2+10604*w_2*a*b*c^2-8048*w_2*b^2*c^2-11073*w_2*a*c^3+8490*w_2*b*c^3-13597*w_2*c^4+10091*w_2*a^2*b-11584*w_2*a*b^2+2635*w_2*b^3-6917*w_2*a^2*c-15955*w_2*a*b*c+3478*w_2*b^2*c+2828*w_2*a*c^2-13577*w_2*b*c^2+6359*w_2*c^3,
    z_(0,0)*b+1193*w_0*b^4+904*w_0*a*b^2*c+6294*w_0*b^3*c+8260*w_0*a^2*c^2-5011*w_0*a*b*c^2+7315*w_0*b^2*c^2+653*w_0*a*c^3-3095*w_0*b*c^3-8268*w_0*c^4-6499*w_1*a^2*b^2-1159*w_1*a*b^3-6130*w_1*b^4-6268*w_1*a^2*b*c-8673*w_1*a*b^2*c-11981*w_1*b^3*c+13725*w_1*a^2*c^2+612*w_1*a*b*c^2-11647*w_1*b^2*c^2+6447*w_1*a*c^3-12658*w_1*b*c^3+5390*w_1*c^4+7253*w_1*a^2*b+14881*w_1*a*b^2+410*w_1*b^3+80*w_1*a^2*c+7158*w_1*a*b*c-6421*w_1*b^2*c+8338*w_1*a*c^2-15834*w_1*b*c^2+12529*w_1*c^3-9685*w_2*a^2*b^2-9541*w_2*a*b^3-14762*w_2*b^4-9390*w_2*a^2*b*c+2287*w_2*a*b^2*c+7441*w_2*b^3*c+7660*w_2*a^2*c^2-5806*w_2*a*b*c^2-14634*w_2*b^2*c^2+10158*w_2*a*c^3-2866*w_2*b*c^3-11023*w_2*c^4-4176*w_2*a^2*b-2418*w_2*a*b^2+13717*w_2*b^3-452*w_2*a^2*c+11648*w_2*a*b*c-1230*w_2*b^2*c+7021*w_2*a*c^2+6421*w_2*b*c^2+5278*w_2*c^3,
    z_(0,0)*a-w_0*c^4-15556*w_1*b^2*c^2-4222*w_1*b*c^3-15630*w_1*c^4+15334*w_2*b^2*c^2-11331*w_2*b*c^3-11658*w_2*c^4,
    z_(0,0)^2+8554*w_0^2*b^4*c^2-12442*w_0^2*a*b^2*c^3-6908*w_0^2*b^3*c^3-143*w_0^2*a^2*c^4-13084*w_0^2*a*b*c^4+10513*w_0^2*b^2*c^4+10056*w_0^2*a*c^5+14509*w_0^2*b*c^5-15384*w_0^2*c^6+961*w_0*w_1*b^4*c^2+13443*w_0*w_1*a*b^2*c^3-8521*w_0*w_1*b^3*c^3-8203*w_0*w_1*a^2*c^4+7854*w_0*w_1*a*b*c^4-2099*w_0*w_1*b^2*c^4-11965*w_0*w_1*a*c^5+61*w_0*w_1*b*c^5+13504*w_0*w_1*c^6+11427*w_0*w_1*a*b^2*c^2-447*w_0*w_1*b^3*c^2+14715*w_0*w_1*a^2*c^3-1034*w_0*w_1*a*b*c^3+9352*w_0*w_1*b^2*c^3-12642*w_0*w_1*a*c^4-5262*w_0*w_1*b*c^4-2083*w_0*w_1*c^5-15000*w_1^2*a^2*b^2*c^2+14840*w_1^2*a*b^3*c^2+4845*w_1^2*b^4*c^2+3243*w_1^2*a^2*b*c^3-7715*w_1^2*a*b^2*c^3+4895*w_1^2*b^3*c^3-12055*w_1^2*a^2*c^4+2903*w_1^2*a*b*c^4+12560*w_1^2*b^2*c^4-12775*w_1^2*a*c^5+386*w_1^2*b*c^5+1048*w_1^2*c^6+2343*w_1^2*a^2*b*c^2+8081*w_1^2*a*b^2*c^2+2162*w_1^2*b^3*c^2-13723*w_1^2*a^2*c^3+9743*w_1^2*a*b*c^3-581*w_1^2*b^2*c^3+6989*w_1^2*a*c^4+13019*w_1^2*b*c^4-10297*w_1^2*c^5+4561*w_1^2*a^2*c^2-12874*w_0*w_2*b^4*c^2-11596*w_0*w_2*a*b^2*c^3-15929*w_0*w_2*b^3*c^3+11249*w_0*w_2*a^2*c^4+5143*w_0*w_2*a*b*c^4+2869*w_0*w_2*b^2*c^4-2863*w_0*w_2*a*c^5-2009*w_0*w_2*b*c^5+8893*w_0*w_2*c^6+4378*w_0*w_2*a*b^2*c^2+10855*w_0*w_2*b^3*c^2-11133*w_0*w_2*a^2*c^3+9130*w_0*w_2*a*b*c^3+10927*w_0*w_2*b^2*c^3+10835*w_0*w_2*a*c^4+7091*w_0*w_2*b*c^4+8624*w_0*w_2*c^5-12084*w_1*w_2*a^2*b^2*c^2+7537*w_1*w_2*a*b^3*c^2-8526*w_1*w_2*b^4*c^2-13066*w_1*w_2*a^2*b*c^3-12465*w_1*w_2*a*b^2*c^3-914*w_1*w_2*b^3*c^3-217*w_1*w_2*a^2*c^4-11152*w_1*w_2*a*b*c^4-10769*w_1*w_2*b^2*c^4-3147*w_1*w_2*a*c^5-2251*w_1*w_2*b*c^5-8570*w_1*w_2*c^6+15209*w_1*w_2*a^2*b*c^2+2586*w_1*w_2*a*b^2*c^2+13917*w_1*w_2*b^3*c^2-15551*w_1*w_2*a^2*c^3-7424*w_1*w_2*a*b*c^3-4065*w_1*w_2*b^2*c^3-1397*w_1*w_2*a*c^4-10737*w_1*w_2*b*c^4+13845*w_1*w_2*c^5+15667*w_1*w_2*a^2*c^2+12662*w_2^2*a^2*b^2*c^2+6630*w_2^2*a*b^3*c^2+3422*w_2^2*b^4*c^2-179*w_2^2*a^2*b*c^3+7761*w_2^2*a*b^2*c^3+11797*w_2^2*b^3*c^3-4998*w_2^2*a^2*c^4-9789*w_2^2*a*b*c^4+15782*w_2^2*b^2*c^4-14414*w_2^2*a*c^5+4596*w_2^2*b*c^5-12643*w_2^2*c^6+12811*w_2^2*a^2*b*c^2+14993*w_2^2*a*b^2*c^2+11955*w_2^2*b^3*c^2-14035*w_2^2*a^2*c^3+10179*w_2^2*a*b*c^3-6282*w_2^2*b^2*c^3-8378*w_2^2*a*c^4+15021*w_2^2*b*c^4+4946*w_2^2*c^5+1345*w_2^2*a^2*c^2)

---------------------------------------------------------------------
-- Example H1 -- Huneke
kk = ZZ/32003
S = kk[a,b,c]
F = a^2*b^2*c+a^4+b^4+c^4
J = ideal jacobian ideal F
substitute(J:F, kk) -- check local quasi-homogeneity!

R = reesAlgebra J
R = (flattenRing R)_0
time R' = integralClosure2 R;
---------------------------------------------------------------------
-- playing around with example H:
codim P
singP = P + minors(codim P,jacobian P);
singP = trim singP;
codim oo

-- example vanHoeij1 playing:
-- degree of the exension is 10, so there are at most 10 gens of the ring
--  square-root of the discriminant of F wrt x is y^4*(y^2+117), wrt y is x^4*(29x^2+3)
--  so there can't be too many extensions (at most 5?) 
integralClosure2 R
