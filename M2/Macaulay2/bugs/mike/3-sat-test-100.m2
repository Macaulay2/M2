satideal0 = (I,F) -> (
    R := ring I;
    m := transpose generators ideal(F);
    while (
	S := (ring I)/I;
	m = m ** S;
	I = ideal syz gb(m, Syzygies => true);
	I != 0
        ) do ();
    I
    --ideal (presentation ring I ** R)
    )

end

load "/Users/mike/M2/Macaulay2/bugs/mike/3-sat-test-100.m2"
-- line 863 in /Users/mike/local/src/macaulay2/M2/Macaulay2/packages/Macaulay2/doc9.m2

R = ZZ/101[x,y,z,a,b,c,d]
--R = ZZ/101[a,b,c,d,x,y,z]  This order is VERY BAD!!
--R = ZZ/101[x,y,z,a,b,c,d,MonomialOrder=>ProductOrder{3,4}]
S = ZZ/101[x,y,z]
row2 = substitute(random(S^1, S^{-3,-3,-3,-3}), R)
row1 = matrix{{a,b,c,d}}
J = minors(2,row1 || row2)
-- gbTrace = 3
F = row2_(0,0)
  -- For this example, just saturate w.r.t. F.
  -- best time: 21.76 seconds
quotient(J, F, Strategy=>Iterate);
time saturate(J, F)
time saturate(J, F, Strategy=>Bayer)  -- 21.76
time saturate(J, F, Strategy=>Eliminate) -- 26.08

satideal0(J,F)


R = ZZ/101[x,y,z,a,b,c,d]
S = ZZ/101[x,y,z]
row2 = substitute(random(S^1, S^{-3,-3,-3,-3}), R)
row1 = matrix{{a,b,c,d}}
J = minors(2,row1 || row2)
F = matrix{{row2_(0,0)}}
gbTrace = 3
S1 = R/J
F1 = matrix{{F ** S1}}
I1 = ideal syz gb(F1, Syzygies=>true)
S2 = S1/I1
peek S2
F2 = matrix{{F ** S2}}
I2 = ideal syz gb(F2, Syzygies=>true)
