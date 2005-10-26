writetomat = (filename, I) -> (
     F = openOut(filename);
     F << #I#0 << " " << #I << endl;
     scan(I, x -> (
	       scan(x, a -> F << a << " ");
	       F << endl;
	       ));
     close F
     );
randomize = (L) -> (
     -- take a list L, and take a random permutation of it
     x := random(#L);
     L1 := drop(L,{x,x});
     prepend(x, randomize L1))
end
restart
load "dumpmat.m2"

R = ZZ/101[a..j]
I6 = basis(12,R);
N = numgens source I6
time for i from 0 to N-1 do first exponents I6_(0,i);
time L = flatten entries I6;
time apply(L, m -> first exponents m);
writetomat("foo10-12.mat",oo);
time I = apply(flatten entries I6, m -> first exponents m);
writetomat("foo4.mat",join(I,I,I,I,I))
	       
R = ZZ/101[a..h]
I = apply(flatten entries basis(12,R), m -> first exponents m);
#I     
I1 = apply(50000, i -> I#(random (#I)));
#I1
writetomat("foo5.mat", I1);
