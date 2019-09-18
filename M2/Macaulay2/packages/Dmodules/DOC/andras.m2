-* Example to compute singular (resp. intersection) cohomology groups of 
   smooth (resp.non-smooth) affine varieties using D-modules.

*-
restart
loadPackage "Dmodules"
R=QQ[x,y,z];
f=x^2+y^2+z^2;
deRham f --Computes singular/de Rham cohomology of the complement of the hypersurface f=0.
S=QQ[x,y,z,t];
W=makeWeylAlgebra S;
g=t*(x^2+y^2+z^2)-1;
localCohom(1,ideal(g))
M=oo#1;
Dintegration(M,{1,1,1,1}) -- Computes same as above, table flipped.
--Now we compute the intersection cohomology of the hypersurface f=0 which has an isolated singularity at 0.
D=makeWeylAlgebra R;
f=x^2+y^2+z^2;
localCohom(1,ideal(f))
N=Ddual oo#1
-*makeCyclic presentation N; --very unfortunate...for localization
I=oo#AnnG
*-
DlocalizeAll(N,x) -- we localize at one of the minors x of the Jacobian matrix
L=image oo#LocMap  -- we obtained the simple D-module corresponding to the IC sheaf corresponding to Z
M=coker presentation L --here it is already cyclic
Dintegration(M,{1,1,1}) -- Computes singular/de Rham cohomology of the complement of the hypersurface f=0 (table flipped).



