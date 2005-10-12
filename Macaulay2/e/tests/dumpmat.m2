R = ZZ/101[a..f]
I = apply(flatten entries basis(12,R), m -> first exponents m);
writetomat = (filename, I) -> (
     F = openOut(filename);
     F << #I#0 << " " << #I << endl;
     scan(I, x -> (
	       scan(x, a -> F << a << " ");
	       F << endl;
	       ));
     close F
     );
writetomat("foo4.mat",join(I,I,I,I,I))
	       
     