genericElement = (R, deg, firstvar) -> (
     -- R is a poly ring A[xs]
     -- deg is a degree
     -- firstvar is a variable in A
     -- result: (F, nextvar).
     A := ring firstvar;
     m := rsort basis(deg,R);
     n := m * genericMatrix(A, firstvar, numgens source m, 1);
     (n_(0,0), A_(index firstvar + numgens source m)))
     
genericMatrix(Module, Module, RingElement) := Matrix => (F,G,a) -> (
     -- F, G are graded free modules over a ring R = A[x1..xn]
     -- a is a variable in the poly ring A
     -- Assumption: the degrees of A are all 0.
     -- return value: a graded matrix F <--- G
     -- with coefficients of the monomials in x's te different variables in the ring A
     -- starting with a.  If there is not enough, then an error is given.
     if not isFreeModule F or not isFreeModule G then error "genericMatrix: expected free modules";
     R := ring F;
     A := ring a;
     if R =!= ring G then error "modules over different rings";
     if A =!= coefficientRing R then error "expected variable in coefficient ring";
     dF := degrees F;
     dG := degrees G;
     firstvar := a;
     h := null;
     map(F, G, (i,j) -> (
	       d := dG#j - dF#i;
	       (h,firstvar) = genericElement(R, d, firstvar);
	       h
	       ))
     )
end
restart
load "genericMatrix.m2"

A = ZZ/5[a_1..a_50, Degrees => {50:0}]
R = A[x,y,z, Join => false]
genericElement(R, 2, a_1)
genericMatrix(R^2, R^{-2},a_1)
