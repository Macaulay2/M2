restart
needsPackage "NCAlgebra"

kk = ZZ/32003
S = kk[a..d]
I = ideal random(S^1, S^{6:-2})
print toString I
R = kk{a,b,c,d}
I1 = ncIdeal apply(subsets(gens R, 2), x -> - x#0 * x#1 + x#1 * x#0)
homogDual I1
I2 = ncIdeal {a^2+a*b,a*c+b*d,a*d,b^2,c^2,c*d+d^2}
242*a^2-3544*a*b+12046*b^2+4576*a*c-14248*b*c+12419*c^2-6287*a*d-7925*b*d-12263*c*d-13997*d^2,-3251*a^2-15252*a*b+885*b^2-12880*a*c+4906*b*c-659*c^2-1666*a*d-7815*b*d+87*c*d-6387*d^2,15143*a^2-884*a*b-11998*b^2+10213*a*c-1559*b*c+4511*c^2+5026*a*d+7708*b*d+6967*c*d+6794*d^2,4414*a^2+8359*a*b-7102*b^2-3369*a*c-8575*b*c+14054*c^2+1755*a*d-12320*b*d-6437*c*d+14722*d^2,-800*a^2+2456*a*b-1040*b^2-2154*a*c+13641*b*c-1989*c^2+7296*a*d+11098*b*d+12742*c*d-2468*d^2,-6008*a^2+7165*a*b+2469*b^2+12197*a*c+7707*b*c-12052*c^2+14040*a*d+9309*b*d-14826*c*d+12081*d^2
I2 = ncIdeal{242*a^2-3544*a*b+12046*b^2+4576*a*c-14248*b*c+12419*c^2-6287*a*d-7925*b*d-12263*c*d-13997*d^2,-3251*a^2-15252*a*b+885*b^2-12880*a*c+4906*b*c-659*c^2-1666*a*d-7815*b*d+87*c*d-6387*d^2,15143*a^2-884*a*b-11998*b^2+10213*a*c-1559*b*c+4511*c^2+5026*a*d+7708*b*d+6967*c*d+6794*d^2,4414*a^2+8359*a*b-7102*b^2-3369*a*c-8575*b*c+14054*c^2+1755*a*d-12320*b*d-6437*c*d+14722*d^2,-800*a^2+2456*a*b-1040*b^2-2154*a*c+13641*b*c-1989*c^2+7296*a*d+11098*b*d+12742*c*d-2468*d^2,-6008*a^2+7165*a*b+2469*b^2+12197*a*c+7707*b*c-12052*c^2+14040*a*d+9309*b*d-14826*c*d+12081*d^2}
I = I1 + I2
J = homogDual I

J = ncIdeal {b*a+a*b-a^2, c*b+b*c, d*b-c*a+b*d-a*c, d^2-d*c-c*d}
J = {d*a-7245*c^2-15425*c*b-13749*c*a-15425*b*c-191*b^2-13088*b*a+a*d-13749*a*c-13088*a*b-11004*a^2,
      d*b-6433*c^2-4763*c*b-5519*c*a+b*d-4763*b*c-1660*b^2-7391*b*a-5519*a*c-7391*a*b-10634*a^2,
      d*c+c*d-11066*c^2-11489*c*b-9090*c*a-11489*b*c-11143*b^2-2918*b*a-9090*a*c-2918*a*b-14879*a^2,
      d^2+3908*c^2-12722*c*b+9721*c*a-12722*b*c-1690*b^2+10674*b*a+9721*a*c+10674*a*b-4873*a^2}
methods ncGroebnerBasis
options ncGroebnerBasis
JGB = ncGroebnerBasis(J, DegreeLimit=>10);
(gens JGB)/degree//tally

restart
debug needsPackage "AssociativeAlgebras"
kk = ZZ/32003
R = kk{d,c,b,a}
J = ideal {b*a+a*b-a^2, c*b+b*c, d*b-c*a+b*d-a*c, d^2-d*c-c*d}
J = ideal {d*a-7245*c^2-15425*c*b-13749*c*a-15425*b*c-191*b^2-13088*b*a+a*d-13749*a*c-13088*a*b-11004*a^2,
      d*b-6433*c^2-4763*c*b-5519*c*a+b*d-4763*b*c-1660*b^2-7391*b*a-5519*a*c-7391*a*b-10634*a^2,
      d*c+c*d-11066*c^2-11489*c*b-9090*c*a-11489*b*c-11143*b^2-2918*b*a-9090*a*c-2918*a*b-14879*a^2,
      d^2+3908*c^2-12722*c*b+9721*c*a-12722*b*c-1690*b^2+10674*b*a+9721*a*c+10674*a*b-4873*a^2}

gbTrace=2
elapsedTime NCGB(J, 3);
select((ideal oo)_*, f -> degree f == {3})
netList oo

elapsedTime NCGB(J, 10);
elapsedTime NCGB(J, 9);
(ideal oo)_*/degree//tally
F = d^2-d*c-c*d
F*d-d*F - F*c + c*F

J = ideal(d^2-d*c-c*d)
elapsedTime NCGB(J, 3)

J = ideal(d^2-c^2)
elapsedTime NCGB(J, 3)
