-- 19 points in P2
kk = ZZ/32003
R = kk[x,y,z]
L = apply(entries random(kk^19, kk^3), v -> minors(2, matrix"x,y,z" || matrix{v}))

L_18^2
P = intersect(append(L_{0..17},L_18^2))
S = kk[a..g]
F = map(R,S,P_*)
I = trim ker F
betti I
Q = intersect(join(
	  apply(toList(0..11), i -> L_i^3),
	  apply(toList(12..13), i -> L_i^2),
	  apply(toList(14..17), i -> L_i^4),
	  {L_18^4}))
betti Q
Qa = sum(0..7, i -> Q_i)
R1 = R/Qa
G = map(R1,S,(sub(P,R1))_*)
ker G
J = oo
betti J
J = trim J
betti J
degree J
genus J
betti J
N = Hom(J, S^1/J);
res J
codim J
m1 = syz gens J
m1 = transpose m1;
S1 = S/J
m1 = sub(m1,S1);
ker m1;
