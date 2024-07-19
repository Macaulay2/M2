TEST ///
R=ZZ/1073741827[z_1..z_3]
I=ideal(7*z_1*z_2+5*z_2*z_3+z_3^2+z_1+5*z_3+10,8*z_1^2+13*z_1*z_3+10*z_3^2+z_2+z_1)
gB=msolveGB I
lT=monomialIdeal leadTerm gB
degree lT
dim lT
R=QQ[z_1..z_3]
I=ideal(7*z_1*z_2+5*z_2*z_3+z_3^2+z_1+5*z_3+10,8*z_1^2+13*z_1*z_3+10*z_3^2+z_2+z_1)
gB=msolveGB I
(ideal gB)== ideal(groebnerBasis I)
lT=monomialIdeal leadTerm gB
degree lT
dim lT
///
	      
TEST ///
R=QQ[x,y];
n=2;
I = ideal ((x-3)*(x^2+1),y-1);
sols=msolveRealSolutions(I,"output type"=>"float");
assert(sols=={{3.0,1.0}});
///

TEST ///
S = ZZ/1073741827[t12,t13,t14,t23,t24,t34];
I = ideal((t13*t12-t23)*(1-t14)+(t14*t12-t24)*(1-t13) - (t12+1)*(1-t13)*(1-t14), (t23*t12-t13)*(1-t24)+(t24*t12-t14)*(1-t23) - (t12+1)*(1-t23)*(1-t24), (t14*t34-t13)*(1-t24)+(t24*t34-t23)*(1-t14) - (t34+1)*(1-t14)*(1-t24));
sat=(1-t24);
J1= saturate(I,sat);
J2=ideal msolveSaturate(I,sat);
assert(J1==J2);
///

TEST ///
R = QQ[x,a,b,c,d];
f = x^2+a*x+b;
g = x^2+c*x+d;
eM2=eliminate(x,ideal(f,g));
eMsolve=msolveEliminate(x,ideal(f,g));
assert(eM2==sub(eMsolve,ring eM2))
///

end

restart
needsPackage "Msolve"
