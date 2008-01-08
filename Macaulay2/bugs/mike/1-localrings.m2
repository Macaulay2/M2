-- Minimal presentation of a resolution
makeMutableResolution = (C) -> (
     new MutableList from apply(min C + 1 .. max C, i -> mutableMatrix(substitute(C.dd_i,frac ring C))))

pivot = (D, ell, r, c) -> (
     -- Do the (r,c) pivot at the ell'th spot
     last0 = numColumns D#(ell-1) - 1;
     last1 = numColumns D#(ell) - 1;
     columnSwap(D#(ell-1), r, last0);
     rowSwap(D#ell, r, last0);
     columnSwap(D#ell, c, last1);
     rowSwap(D#(ell+1), c, last1);
     u = D#ell_(last0,last1);
     uinv := 1/u;
     for i from 0 to last1-1 do (
	  e := D#ell_(last0,i);
	  if e != 0 then (
	       peek D;
	       columnAdd(D#ell, i, - uinv * e, last1);
	       rowAdd(D#(ell+1), last1, uinv * e, i); -- these are only here to check the code: they do not need to be performed.
	       ));
     -- For now we replace the matrices
     D#(ell-1) = mutableMatrix submatrix(matrix D#(ell-1), {0..last0-1});
     D#(ell) = mutableMatrix submatrix(matrix D#(ell), {0..last0-1}, {0..last1-1});
     D#(ell+1) = mutableMatrix submatrix(matrix D#(ell+1), {0..last1-1},);
     peek D
     )

-- minimal gens in local case
minbase = method()
minbase Ideal := (I) -> (
     g1 := gb I;
     g2 := gb ((ideal vars ring I)*I);
     m1 := monomialIdeal leadTerm g1;
     m2 := monomialIdeal leadTerm g2;
     m1 - m2
     )
end

restart
load "1-localrings.m2"
kk = ZZ/101
kk = QQ
A = kk[a..d]
F = ideal(a^2-a^3-a*d*c-b^4-c^2*d^2-a^7-c^5-d^6)
JF = F + ideal jacobian F
C = res JF
AK = frac A;

C.dd

B = kk[a..d,MonomialOrder=>Weights=>4:-1,Global=>false]
P = substitute(JF,B)
P1 = syz gens P
C1 = mutableMatrix(C.dd_1)
C2 = mutableMatrix(C.dd_2)
C3 = mutableMatrix(C.dd_3)
C4 = mutableMatrix(C.dd_4)

printWidth=220
(C1,C2,C3,C4)

D = makeMutableResolution C
checkD = (D) -> (for i from 1 to #D-1 do assert(matrix D#(i-1) * matrix D#i == 0))
checkD D
peek D
pivot(D, 1, 4,8)
pivot(D,3,6,2)
pivot(D,2,3,2)
pivot(D,2,10,10)
pivot(D,2,0,8)
pivot(D,3,3,2)
pivot(D,1,3,10)
pivot(D,2,9,6)
checkD D

columnMult(D#3,0,-24*c*d^2+5)
columnMult(D#3,1,-24*c*d^2+5)
lift(matrix D#3,A)

columnAdd(C2,10,b^3,8)
columnAdd(C2,11,-d^3,8)
columnAdd(C2,12,-2*d^2,8)

columnAdd(C2,3,a,8)
columnAdd(C2,4,c,8)

rowAdd(C3,8,-b^3,10)
rowAdd(C3,8,d^3,11)
rowAdd(C3,8,2*d^2,12)
rowAdd(C3,8,-a,3)
rowAdd(C3,8,-c,4)

matrix C2 * matrix C3

