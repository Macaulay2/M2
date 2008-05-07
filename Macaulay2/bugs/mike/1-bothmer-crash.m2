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
