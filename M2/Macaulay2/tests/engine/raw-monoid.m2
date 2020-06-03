--status: this old test depends on internal things and probably should be deleted


---------------------------------------------------
-- Test of engine monomial order and monoid code --
---------------------------------------------------

needs "raw-util.m2"
errorDepth = 0

-- Test of the following routines:
--   rawMonomialOrdering
--     Lex, 
--     GRevLex, 
--     RevLex
--     Weights
--     GroupLex, GroupRevLex
--     NCLex
--     Position=>Up, Position=>Down
--     tensor products?
--   rawNumberOfVariables
--   rawNumberOfInvertibleVariables
--   rawNonTermOrderVariables
--   rawMonoid
--   toString, hash for RawMonoid, RawMonomialOrdering
--
--   rawCompareMonomial
--   rawMonomialDivide   -- REMOVE??
--   rawMonomialDivides  -- WRONG??

mo = rawMonomialOrdering { Weights => {1,2}, Lex => 4, GroupLex => 2}
assert(rawNumberOfVariables mo === 6)
assert(rawNumberOfInvertibleVariables mo == 2)
print "ERROR: rawNonTermOrderVariables not connected or tested yet"
--rawNonTermOrderVariables mo

M = rawMonoid(mo, ("a","b","c","d","e","f"), degring 1, (1,1,1,1,1,1),{1})
a = rawVarMonomial(0,1)
b = rawVarMonomial(1,1)
c = rawVarMonomial(2,1)
e = rawVarMonomial(4,1)
assert (rawCompareMonomial(M,a,b) === symbol<)
assert (rawCompareMonomial(M,b,a) === symbol>)
assert (rawCompareMonomial(M,a,a) === symbol==)
assert (rawCompareMonomial(M,a*e, b^3*e) === symbol<)
assert rawMonomialDivides(M, a, a*b)
assert not rawMonomialDivides(M, a*b, a)
assert rawMonomialDivides(M, a, a*e)
print "ERROR: a*e should divide a here, since e is invertible?"
--assert rawMonomialDivides(M, a*e, a)

print "WARNING: rawMonomialDivide not connected or tested: probably remove it"
--rawMonomialDivide

assert instance(hash M,ZZ)
assert instance(toString M,String)

-- Local Variables:
-- compile-command: "M2 -e errorDepth=0 --stop -e 'load \"raw-monoid.m2\"' -e 'exit 0' "
-- End:
