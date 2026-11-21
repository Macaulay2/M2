-- The documented results of version 1.0 stay (lead agent, session 1).
-- The interface: the four functions of 1.0 and the new ones are exported.
scan({"cyclotomicField", "cyclotomicPoly", "findRootPower", "joinCyclotomic", "cyclotomicRoot",
        "rootOfUnityGenerator", "rootOfUnityExponent", "rootOfUnityOrder", "discreteLog",
        "cyclotomicInclusion", "cyclotomicExtension", "galoisAutomorphism"},
    s -> assert isGlobalSymbol s);
-- cyclotomicField: one ring per order, generator ww_n with relation Phi_n
S = cyclotomicField 5;
assert(S === cyclotomicField 5 and isField S);
r = S_0;
assert(r^5 == 1 and r != 1 and r^4 + r^3 + r^2 + r + 1 == 0);
assert(toString r == "ww_5");
S6 = cyclotomicField 6;
assert(toString S6_0 == "ww_6" and S6_0^2 - S6_0 + 1 == 0 and S6 =!= cyclotomicField 3);
-- cyclotomicPoly as in 1.0
R = QQ[w];
assert(cyclotomicPoly(6, w) == w^2 - w + 1);
assert(cyclotomicPoly(1, w) == w - 1);
assert(cyclotomicPoly(12, w) == w^4 - w^2 + 1);
assert(first degree cyclotomicPoly(105, w) == 48);
-- findRootPower: order of the generator, or 2
assert(findRootPower((cyclotomicField 5)[getSymbol "x"]) == 5);
assert(findRootPower(QQ[getSymbol "x"]) == 2);
assert(findRootPower((cyclotomicField 2)[getSymbol "x"]) == 2);
assert(findRootPower((cyclotomicField 4)[getSymbol "x"]) == 4);
-- joinCyclotomic: the documented example, the field of order lcm(3, 4) = 12
F = cyclotomicField 3; G = cyclotomicField 4;
A = F[getSymbol "t"]; B = G[getSymbol "t"];
L = joinCyclotomic {ideal(A_0 - F_0^2), ideal(B_0^2 - G_0)};
T = ring L#0;
assert(ring L#1 === T and coefficientRing T === cyclotomicField 12);
z = (coefficientRing T)_0;
assert(L#0 == ideal(T_0 - z^8) and L#1 == ideal(T_0^2 - z^3));
-- 1.0 keeps the order of the lcm: a join over cyclotomicField 6 stays over it
A6 = S6[getSymbol "s"];
assert(coefficientRing ring first joinCyclotomic {ideal(A6_0 - S6_0)} === S6);
