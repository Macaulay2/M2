-- make generic matrices with powert of a variable as
-- coefficients
-- this is sometimes usefull to avoid using a large number
-- of variables

genericPowerElement = (R, deg, var, i) -> (
     -- R is a poly ring A[xs]
     -- deg is a degree
     -- var is a variable in A
     -- i is the next power of a to be used
     -- result: (F, nextvar).
     --A := ring var;
     m := rsort basis(deg,R);
     n := m * matrix apply(numgens source m, j->{var^(j+i)});
     (n_(0,0), i + numgens source m))
     
genericPowerMatrix = method()

genericPowerMatrix(Module, Module, RingElement) := Matrix => (F,G,a) -> (
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
     e := 1;
     h := null;
     map(F, G, (i,j) -> (
	       d := dG#j - dF#i;
	       (h,e) = genericPowerElement(R, d, a, e);
	       h
	       ))
     )

dimHom = method()
dimHom(Module, Module) := ZZ => (F,G) -> (
     -- F, G are graded free modules over a ring R = A[x1..xn]
     -- Assumption: the degrees of A are all 0.
     -- return value: dim Hom=(F <--- G)
     -- with coefficients of the monomials in x's te different variables in the ring A
     -- starting with a.  If there is not enough, then an error is given.
     if not isFreeModule F or not isFreeModule G then error "dimHom: expected free modules";
     R := ring F;
     if R =!= ring G then error "modules over different rings";
     dF := degrees F;
     dG := degrees G;
     sum apply(dG,g-> (
	       sum apply(dF,f->(
	       		 rank source basis(g - f,R)
	       ))
     ))
)

end
---

restart
load "genericPowerMatrix.m2"

A = ZZ/5[a, Degrees => {0}]
R = A[x,y,z, Join => false]
(g,i) = genericPowerElement(R, 2, a,1)
coefficients(g)
genericPowerMatrix(R^2, R^{-2},a)
