-- Debugging of binomial and toric GB's

R = ZZ/101[a..f]

I = ideal(a^2-b*c, a*b-c*d)
gens gb(I)

I = ideal(a^2-b*c, a*b-c*d)
gb(I, Algorithm=>Toric)
gens oo

assocModel = (m,n) -> (
     -- m by n assoc model.
     -- returns the "A" matrix for it
     R := ZZ[y_(1,1)..y_(m,n)];
     L1 := toList apply(1..m, i -> sum toList apply(1..n, j -> y_(i,j)));
     L2 := toList apply(1..n, i -> sum toList apply(1..m, j -> y_(j,i)));
     L3 := {sum flatten toList apply(1..m, i -> toList apply(1..n, j -> i*j*y_(i,j)))};
     substitute(transpose jacobian matrix {join(L1,L2,L3)}, ZZ))

LLL syz assocModel(4,4)
