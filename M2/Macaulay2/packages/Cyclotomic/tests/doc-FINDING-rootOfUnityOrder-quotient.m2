-- doc-FINDING-rootOfUnityOrder-quotient.m2 -- a finding of the independent tester.
-- Documentation of rootOfUnityOrder: input "x, a ring element, in QQ, a
-- number field QQ[a]/(f), ZZ/p or a Galois field".  The example builds the
-- number field with toField(QQ[a]/(a^2+1)).  For the quotient ring
-- QQ[a]/(a^2+1) without toField, which is the number field QQ[a]/(f)
-- literally, the function raises "rootOfUnityOrder: expected an element of
-- QQ, of a number field QQ[a]/(f), or of a finite field".
-- (Unsure: the documentation may mean only rings marked as fields by
-- toField; then it should say so.)
A = QQ[a]/(a^2+1);
assert(rootOfUnityOrder a == 4);
B = QQ[b]/(b^2-2);
assert(rootOfUnityOrder b === null);
assert(rootOfUnityOrder(-1_B) == 2);
