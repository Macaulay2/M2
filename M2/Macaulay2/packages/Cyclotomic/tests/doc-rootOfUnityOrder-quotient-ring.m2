-- doc-rootOfUnityOrder-quotient-ring.m2
-- Documentation of rootOfUnityOrder: input "x, a ring element, in QQ, a
-- number field QQ[a]/(f), ZZ/p or a Galois field".  The number field may be
-- given as the quotient ring QQ[a]/(f) without toField.
A = QQ[a]/(a^2+1);
assert(rootOfUnityOrder a == 4);
B = QQ[b]/(b^2-2);
assert(rootOfUnityOrder b === null);
assert(rootOfUnityOrder(-1_B) == 2);
