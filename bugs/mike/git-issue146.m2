trimGens = method()
trimGens Matrix := (M) -> gens trimGens image M -- what about more general matrices/modules?
trimGens Ideal := (I) -> ideal trimGens module I
trimGens Module := (I) -> (
    R := ring I;
    if R === ZZ or isField R then return trim I;
    kk := ultimate(coefficientRing, R);
    if kk =!= ZZ and isHomogeneous I 
    then trim I
    else (
        -- get ultimate coeff ring too
        -- Assume for now that I is in a poly ring over ZZ, or a quotient of such.
        --   Allow skew commutative case too
        Z := entries gens gb syz gens I;
        G := if kk === ZZ then (
               matrix for r in Z list for s in r list if s == 1 or s == -1 then lift(s,kk) else 0
               )
             else (
               matrix for r in Z list for s in r list if # support s == 0 then lift(s,kk) else 0
               );
        << "--- G: ----" << endl;
        << "Z = " << matrix Z << endl;
        << "before G = " << G;
        G = gens gb G;
        << "  after G = " << G << endl;        
        iden := id_(kk^(numColumns gens I));
        p := toList select(0..numColumns gens I - 1, i -> iden_{i} % G != 0);
        image (gens I)_p
        )
    )

end
restart
load "~/src/M2/bugs/mike/git-issue146.m2"
printWidth=150
P = ZZ[a, c1, c2, d]
J1 = ideal(3*a*c1^2+2*a^2*c2-6*c1^2-11*a*c2-6*c2^2+6*a+39*c2-38,a*c1^2*d+a^2*c2*d-2*c1^2*d-4*a*c2*d-2*c2^2*d+2*a*d+15*c2*d-17*d-1,a^2*c2^2+3*a*c1^2+2*a*c2^2+3*c1^2-9*a*c2+3*c2^2+6*a-13*c2+6,a^3*c2+5*a*c2-13*a-13,a^2*c1^2-a*c1^2-3*a^2*c2-2*a*c2^2+2*a^2-2*c1^2+6*a*c2-2*c2^2-2*a+13*c2-4,a^3*c1^2+2*a^3+a^2*c2+10*c1^2+27*a*c2+10*c2^2-39*a-52*c2+7 )
J2 = trim J1
J3 = ideal first entries gens J2
gens J1 % J2
gens J2 % J3
gens J1 % J3
a^2*c2*d % J1 == a^2*c2*d % J3
2*c2*d % J1 == 2*c2*d % J3
leadTerm gens gb J1
leadTerm gens gb J3


J1 = ideal J1_*
L = ideal gens gb J1
syz gens L

trimGens ideal gens gb J1
matrix for r in oo list for s in r list if s == 1 or s == -1 then s else 0
syz gens J1;
Z1 = trimGens image oo;
Z2 = syz Z1;
Z2 = trimGens image Z2;

restart
debug needsPackage "HyperplaneArrangements"

R = ZZ[x,y,z];
A3 = arrangement({x,y,z,x-y,x-z,y-z},R)
j = orlikSolomon A3
errorDepth=0
modT = (ring j)^1/(j*(ring j^1));
R1 = ZZ[X_1..X_6]
F = res(prune modT, LengthLimit=>3);
g = transpose F.dd_2;
G = res(coker g,LengthLimit=>4);
FA = coker symExt(G.dd_4, R1);
d = first flatten degrees cover FA;
M = FA**(ring FA)^{d};  -- GD: I want this to be generated in degree 0

res M

F1 = relations modT
F2 = trimGens syz F1
g = transpose F2
g2 = trimGens syz g
g3 = trimGens syz g2
g4 = trimGens syz g3

P1 = QQ[a, c1, c2, d]
J1 = ideal(3*a*c1^2+2*a^2*c2-6*c1^2-11*a*c2-6*c2^2+6*a+39*c2-38,a*c1^2*d+a^2*c2*d-2*c1^2*d-4*a*c2*d-2*c2^2*d+2*a*d+15*c2*d-17*d-1,a^2*c2^2+3*a*c1^2+2*a*c2^2+3*c1^2-9*a*c2+3*c2^2+6*a-13*c2+6,a^3*c2+5*a*c2-13*a-13,a^2*c1^2-a*c1^2-3*a^2*c2-2*a*c2^2+2*a^2-2*c1^2+6*a*c2-2*c2^2-2*a+13*c2-4,a^3*c1^2+2*a^3+a^2*c2+10*c1^2+27*a*c2+10*c2^2-39*a-52*c2+7 )
L1 = trimGens gens J1

needsPackage "HyperplaneArrangements"
R = ZZ[x,y,z];
A3 = arrangement({x,y,z,x-y,x-z,y-z},R)
assert(pdim EPY A3 == 3)

-- Examples in git issue 146:
-- Example 1.
P = ZZ[a, c1, c2, d]
J = ideal(3_a_c1^2+2_a^2_c2-6_c1^2-11_a_c2-6_c2^2+6_a+39_c2-38,a_c1^2_d+a^2_c2_d-2_c1^2_d-4_a_c2_d-2_c2^2_d+2_a_d+15_c2_d-17_d-1,a^2_c2^2+3_a_c1^2+2_a_c2^2+3_c1^2-9_a_c2+3_c2^2+6_a-13_c2+6,a^3_c2+5_a_c2-13_a-13,a^2_c1^2-a_c1^2-3_a^2_c2-2_a_c2^2+2_a^2-2_c1^2+6_a_c2-2_c2^2-2_a+13_c2-4,a^3_c1^2+2_a^3+a^2_c2+10_c1^2+27_a_c2+10_c2^2-39_a-52*c2+7 ); J = promote(J,P);
Jmin = trim J
assert (Jmin == J)
assert (ideal first entries gens Jmin == J)
-- Example 2.
P = ZZ[a, c1, c2, d]
J1 = ideal(3*a*c1^2+2*a^2*c2-6*c1^2-11*a*c2-6*c2^2+6*a+39*c2-38,a*c1^2*d+a^2*c2*d-2*c1^2*d-4*a*c2*d-2*c2^2*d+2*a*d+15*c2*d-17*d-1,a^2*c2^2+3*a*c1^2+2*a*c2^2+3*c1^2-9*a*c2+3*c2^2+6*a-13*c2+6,a^3*c2+5*a*c2-13*a-13,a^2*c1^2-a*c1^2-3*a^2*c2-2*a*c2^2+2*a^2-2*c1^2+6*a*c2-2*c2^2-2*a+13*c2-4,a^3*c1^2+2*a^3+a^2*c2+10*c1^2+27*a*c2+10*c2^2-39*a-52*c2+7 )
J2 = trim J1
J3 = ideal first entries gens J2
gens J1 % J2
gens J2 % J3
gens J1 % J3
a^2*c2*d % J1 == a^2*c2*d % J3
2*c2*d % J1 == 2*c2*d % J3
leadTerm gens gb J1
leadTerm gens gb J3
assert(J1 == J2)
assert(J1 == J3)
assert(J2 == J3)
