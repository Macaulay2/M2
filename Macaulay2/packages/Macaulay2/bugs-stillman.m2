-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"

restart
R = ZZ[a..d,MonomialOrder=>Lex]
sorted = sort{a*d, b^100, c^3*d}
assert(sorted === {c^3*d, b^100, a*d}) -- fails
assert(c^3*d <= b^100)
assert(b^100 <= a*d)
assert(c^3*d <= a*d)
sorted2 = rsort{a*d, b^100, c^3*d} -- same as sort??

-- files to watch out for:
ann'-doc.m2 (egrep screwed up)
ann-doc.m2 -- already exists??
contract'-doc.m2
diff'-doc
identity-doc.m2 -- already exists??
file functions/normalPrompts-doc.m2 already exists -- not modifying it
file functions/numeric-doc.m2 already exists -- not modifying it
writing functions/peek'-doc.m2
writing functions/quotient'-doc.m2
writing functions/remainder'-doc.m2

-- design flaw:
-- forceGB shouild be
--    forceGB(f, GroebnerBasis=>g, ChangeMatrix=>c, SyzygyMatrix=>s) ?
-- forceGB(f, ChangeMatrix=>m)
--    where does this go to?  The minimal matrix?

-- 'monomials' applied to a matrix with more than one row is coming out messed up
-- see the test in functions/monomials-doc.m2
