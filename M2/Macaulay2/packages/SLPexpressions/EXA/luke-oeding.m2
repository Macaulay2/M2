-- the polynomial is the 3x3x3 degree 4 Strassen Invariant
restart
needsPackage "SLPexpressions"

xx = 5
aa = 2
bb = 2
cc = 2

scan((0,0)..(aa,xx), i->a_i = inputGate A_i)
scan((0,0)..(bb,xx), i->b_i = inputGate B_i)
scan((0,0)..(cc,xx), i->c_i = inputGate C_i)
scan((0,0,0)..(aa,bb,cc), i->x_i = inputGate X_i)

-- the following matrices should have their column indices be determined by the
--  columns of some triple of tableaux, and ideally there should be a function that makes
--  them dynamically
A1= matrix apply(3,i -> apply({0,1,2}, j-> a_(i,j) ))
A2= matrix apply(3,i -> apply({3,4,5}, j-> a_(i,j) ))


B1= matrix apply(3,i -> apply({0,2,4}, j-> b_(i,j) ))
B2= matrix apply(3,i -> apply({1,3,5}, j-> b_(i,j) ))

C1= matrix apply(3,i -> apply({0,1,3}, j-> c_(i,j) ))
C2= matrix apply(3,i -> apply({2,4,5}, j-> c_(i,j) ))


det GateMatrix := o -> M -> (
    assert (numrows M == 3 and numcols M == 3);
    sum apply({{0, 1, 2}, {0, 2, 1}, {1, 0, 2}, {1, 2, 0}, {2, 0, 1}, {2, 1, 0}},
    	{1,-1,-1,1,1,-1}, (p,s)->s*product(#p,i->M_(i,p#i))
    	)
    )

-- this should be held as a product and not expanded for larger problems
time T = det(A1)*det(A2)*det(B1)*det(B2)*det(C1)*det(C2);
F = T;
time for d from 0 to xx do(
    time F = sum(cc+1,k-> sum(bb+1, j-> sum(aa+1,i->x_(i,j,k)*diff(a_(i,d),diff(b_(j,d),diff(c_(k,d),F))))));
    time CF = compress F;
    print(support F - set support CF);
    F = CF;
    )
toString F