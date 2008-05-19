K = ZZ/2;
R = K[x_0..x_5];
IRNC = minors(2,matrix{{x_0..x_(4)},{x_1..x_5}});
F = res IRNC;
A = F.dd_1;
B = F.dd_2;

time scan (3, j -> (
	  I = ideal(A*syz transpose syz(transpose(B * random(K^20,K^1)), DegreeLimit => 1));
	  if I == 0
	  then ((betti res I, codim I, degree I),{})
	  else ((betti res I, codim I, degree I),
	       (sort apply(primaryDecomposition I,i->(
			      codim i, degree i, betti res i)))
	       ))
    )	

-- crashes in singular code...


K = ZZ/2;
R = K[x_0..x_5];
I = ideal(x_1*x_2+x_0*x_3+x_1*x_3+x_2*x_3+x_0*x_4+x_1*x_4,x_1^2+x_0*x_2+x_2^2+x_1*x_3+x_3^2+x_2*x_4,x_1*x_3+x_0*x_4+x_1*x_4+x_0*x_5,x_2^2+x_1*x_3+x_2*x_4+x_1*x_5,x_3*x_4+x_2*x_5,x_1^2+x_0*x_2+x_1*x_2+x_0*x_3+x_4^2+x_3*x_5)
S = K[a..f]
J = sub(I,vars S)
toString J

debug Core
R = ZZ/2[a..f]
I = ideal"bc+ad+bd+cd+ae+be,b2+ac+c2+bd+d2+ce,bd+ae+be+af,c2+bd+ce+bf,de+cf,b2+ac+bc+ad+e2+df"
--primaryDecomposition I  -- causes a crash
irreducibleCharacteristicSeries I
gens gb I
primaryDecomposition ideal oo

fI = generators I;
re = rawIdealReorder raw fI
n = #re;
fI = substitute(fI,apply(n,i -> R_(re#i) => R_i));
toString fI
rawCharSeries raw fI

debug Core
R = ZZ/2[a..f]
fI = matrix"bc+bd+ce+cf+df+ef, c2+be+de+e2+cf+f2, ab+bd+cf+df, de+e2+af+cf, cd+ae, ac+bc+d2+be+ef+f2"
rawCharSeries raw fI

R = ZZ/2[a..f]
fI = matrix"bc+bd+ce+cf+df+ef, c2+be+de+e2+cf+f2, ab+bd+cf+df, de+e2+af+cf, cd+ae, ac+bc+d2+be+ef+f2"
rawCharSeries raw fI
