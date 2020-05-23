-- arising from one computation in ~/local/people/van-Hoeij-Mark/examples/example3.m2
debug Core
testGCD = (F1,F2) -> (
     G1 := rawGCD(F1,F2);
     (G,U,V) := rawExtendedGCD(F1,F2);
     assert(G == U*F1 + V*F2);
     assert(G == G1);
     (G,U,V)
     )

R1 = rawTowerRing(2, 1:"a")
R1 = rawTowerRing(2, ("a","y"))
a = rawRingVar(R1,0)
y = rawRingVar(R1,1)
F1 = poly"a112+a109+a108+a102+a101+a100+a90+a89+a86+a84+a83+a80+a78+a77+a76+a75+a74+a73+a70+a69+a68+a67+a64+a63+a62+a61+a59+a58+a54+a52+a50+a49+a47+a46+a45+a42+a40+a39+a35+a34+a32+a31+a30+a26+a25+a24+a23+a21+a17+a16+a15+a13+a12+a11+a8+a6+a4+a2+1"

R2 = rawTowerQuotientRing(R1, (F1,0_R1))
a = rawRingVar(R2,0)
y = rawRingVar(R2,1)
G = poly"y21+ay20+(a3+a+1)y18+(a3+1)y17+(a4+a)y16+(a7+a6+a3+a+1)y15+a7y14+(a8+a7+a6+a4+a3+1)y13+(a9+a8+a4+1)y12+(a11+a9+a8+a5+a4+a3+a2)y11+(a12+a9+a8+a7+a5+a3+a+1)y10+(a14+a13+a10+a9+a8+a7+a6+a3+a2+1)y9+(a13+a9+a8+a6+a4+a3+a)y8+(a16+a15+a13+a12+a11+a7+a3+a)y7+(a17+a16+a13+a9+a8+a)y6+(a17+a16+a12+a7+a5+a2+a+1)y5+(a19+a16+a15+a12+a6+a5+a3+1)y4+(a18+a15+a12+a10+a9+a7+a4+a)y3+(a22+a21+a20+a18+a13+a12+a9+a8+a7+a5+a4+a3)y2+(a23+a22+a20+a17+a15+a14+a12+a9)y+a25+a23+a19+a17+a15+a13+a11+a5"
H = poly"y20+(a2+1)y18+a2y17+y16+(a6+a2+1)y15+a6y14+(a6+a2)y13+a8y12+(a10+a8+a4+a2)y11+(a8+a6+a4+a2+1)y10+(a12+a8+a6+a2)y9+(a12+a8+a2+1)y8+(a14+a12+a10+a6+a2+1)y7+(a16+a12+a8+1)y6+(a16+a6+a4+1)y5+(a18+a14+a4+a2)y4+(a14+a8+a6+1)y3+(a20+a12+a8+a6+a4+a2)y2+(a22+a16+a14+a8)y+a24+a22+a18+a16+a14+a12+a10+a4"
testGCD(G,H)
time g = rawGCD(G,H)
(G // g) * g == G
G1 = G // g
H % g
H1 = H // g
rawGCD(G1,H1)

-- Let's factor G, H.