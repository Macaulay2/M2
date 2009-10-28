reductionNumber(Ideal,Ideal) := (i,J) -> (
     --J:=minimalReduction i; -- will be a power of i times the minimal reduction
     --JJJ = J;
     I:=i; -- will be a power of i
     M:= ideal vars ring i; -- we're pretending to be in a local ring
     rN:=0;
     if isHomogeneous J then (
     	  while not ((gens I)%J)==0 do (
	       I = trim (i*I);
	       J= trim(i*J);
     	       rN =rN+1))
     else(
     	  while not ((gens I)%(J+M*I))==0 do (
	       I = trim(i*I);
	       J= i*J;
     	       rN =rN+1));
     rN)

end


load "0-reduction.m2"

kk = ZZ/101;
S = kk[a..c];
m = ideal vars S;
i = (ideal"a,b")*m+ideal"c3"
-- one time the following J came out as the minimal reduction:
J = ideal(-10*c^3+40*a^2+11*a*b-4*b^2-43*a*c+5*b*c,-23*c^3+26*a^2-46*a*b+3*b^2-21*a*c+38*b*c,-15*c^3+19*a^2+46*a*b-7*b^2+5*a*c-39*b*c)
reductionNumber(i,J) -- seems to hang
