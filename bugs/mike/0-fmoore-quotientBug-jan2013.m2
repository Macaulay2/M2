-- from Frank Moore, 31 Jan 2013
-- Bug in quotient?
-- In investigating some problems involving random forms over QQ,
-- I seem to have found a bug, or at least a behavior of quotient that I do not understand.

restart
Q = QQ[x,y,z]
R = Q/(2588916931*x^2*y^2-7389060666*x*y^3-11095418817*y^4-22095023125*x^3*z+6756478377*x^2*y*z+13390783153*x*y^2*z-9247344526*y^3*z-6396897274*x^2*z^2-12278993574*x*y*z^2+11171821459*y^2*z^2+23462113162*x*z^3+997641401*y*z^3+10792781707*z^4,12944584655*x^3*y+9651326537*x*y^3+17227359612*y^4+39800698925*x^3*z-17549631790*x^2*y*z-25517939183*x*y^2*z+6965932014*y^3*z+10450570627*x^2*z^2+28318028808*x*y*z^2-14196748513*y^2*z^2-47019031918*x*z^3-8142522090*y*z^3-18094245084*z^4,5177833862*x^4-6214776374*x*y^3-9418949453*y^4-16844534837*x^3*z+6415533247*x^2*y*z+7075857917*x*y^2*z-9606582348*y^3*z-8906243604*x^2*z^2-5131573188*x*y*z^2+11611425180*y^2*z^2+15935283877*x*z^3-2215476622*y*z^3+13910817518*z^4,75356*y^3*z^2+70250*x^2*z^3-40420*x*y*z^3-122074*y^2*z^3-139198*x*z^4+221658*y*z^4-6165*z^5,75356*x*y^2*z^2-10966*x^2*z^3-57912*x*y*z^3-4022*y^2*z^3+123550*x*z^4-118122*y*z^4-61323*z^5,75356*x^2*y*z^2-17206*x^2*z^3-24072*x*y*z^3-27998*y^2*z^3+7010*x*z^4-9282*y*z^4-61419*z^5,18839*x^3*z^2-4838*x^2*z^3+8847*x*y*z^3+105*y^2*z^3+9515*x*z^4-32271*y*z^4-11088*z^5,37678*y^4*z+42410*x^2*z^3-25662*x*y*z^3-23264*y^2*z^3-59196*x*z^4+49336*y*z^4-3695*z^5,75356*x*y^3*z-96038*x^2*z^3+26660*x*y*z^3+30814*y^2*z^3+179994*x*z^4-216746*y*z^4-153059*z^5);
A = R(monoid[T_1, T_2, T_3, Degrees => {3:VerticalList{1, 1}}, Heft => {1, 0}, MonomialOrder => VerticalList{MonomialSize => 32, GRevLex => {3:1}, Position => Up}, DegreeRank => 2, SkewCommutative => {0, 1, 2}]);
A = R(monoid[T_1, T_2, T_3, 
        Degrees => {3:{1,1}},
        SkewCommutative => {0, 1, 2}]);
f = ((4224139625146430218931/2)*y*z^5-693181320042635095861*z^6)*T_1*T_2+(-(1688099653040444934477/2)*y*z^5+(369422029102333714767/2)*z^6)*T_1*T_3+(-95281854976206688333*y*z^5+112999364311814858919*z^6)*T_2*T_3;
g = z*T_1*T_2-y*T_1*T_3+x*T_2*T_3
assert(f % g == 0)  -- zero remainder
h = f // g;
assert((g*h - f) == 0) -- can't divide?
f - h*g == 0

end
f // g
mf = gens ideal f
mg = gens ideal g
mh = (mf // mg)
h = f // g

mh
h
f
g
(A1,p1) = flattenRing A;
J1 = ideal A1;
p1
F = p1 f
G = p1 g
ANS1 = G * (F // G)
ANS2 = F
ANS1 - ANS2
ANS1
F
G
(gens ideal F) // (gens ideal G)
oo_(0,0)
F

ideal A1
code methods symbol //

B = ring J1
F1 = lift(F,B)
G1 = lift(G,B)
L = ideal(G1) + J1
Z1 = F1 // (gens L)
F1 % (gens L)
(gens L) * Z1- F1 -- this one works...!
gens L;
Z1;
