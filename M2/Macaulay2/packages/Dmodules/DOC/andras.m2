-* Example to compute singular (resp. intersection) cohomology groups of 
   smooth (resp.non-smooth) affine varieties using D-modules.
*-
restart
loadPackage "Dmodules"
R=QQ[x,y,z];
f=x^2+y^2+z^2;
deRham f --Computes singular/de Rham cohomology of the complement of the hypersurface f=0.
S=QQ[x,y,z,t];
I=ideal(t*(x^2+y^2+z^2)-1);
d=dim I;
W=makeWeylAlgebra S;
I=ideal(t*(x^2+y^2+z^2)-1);
localCohom(1,I);
M=oo#1;
Dintegration(M,{1,1,1,1}); -- Computes same as above, but table flipped.
Coho=new HashTable from (for i from 0 to d list i=>oo#(d-i))
--Now we compute the intersection cohomology of the hypersurface f=0 which has an isolated singularity at 0.
R=QQ[x,y,z];
I=ideal(x^2+y^2+z^2);d=dim I
D=makeWeylAlgebra R;
I=ideal(x^2+y^2+z^2)
localCohom(1,I)
N=Ddual oo#1
-*makeCyclic presentation N; --very unfortunate...for localization
I=oo#AnnG*-
DlocalizeAll(N,x) -- we localize at one of the minors x of the Jacobian matrix
L=image oo#LocMap  -- we obtained the simple D-module corresponding to the IC sheaf corresponding to Z
M=coker presentation L --here it is already cyclic
Dintegration(M,{1,1,1})
ICoho=new HashTable from (for i from 0 to d list i=>oo#(d-i)) -- Computes intersection cohomology of the hypersurface f=0



