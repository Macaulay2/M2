-- Here are the errors we have in the test directory
-- and what they are.

ISSAC-97.m2 -- runs now, but takes quite a bit of memory
LU.m2             -- MES: mutable matrix multiplication
WeylAlgebra.m2    -- DRG: specifying Weyl algebra variables
clearOutput.m2    -- DRG: peculiar front end symbol bug?
engine-div.m2     -- MES: division and remainders
galois.m2         -- DRG: simple one at begin, MES: galois ringmap bug
inverse.m2
matrixpromote.m2  -- I'm to look at this
minPres_test.m2
monoid.m2         -- not really a bug: this code should give an error
normal_test.m2
order.m2          -- DRG: order of factors is not unique...
overflow.m2       -- MES: monomial overflow bug
reduce.m2         -- MES+DRG: division and remainder by 0 should be OK
res11.m2          -- MES: reordering of module generators
ring2.m2          -- MES or DRG: certain kernel of ringmaps not implemented
skewmonideal.m2   -- MES: squares appear
sortcols.m2       -- MES: what should sorting be for ZZ?  Should we always look at coeff for sorting?
subst3.m2         -- MES: galois ringmap bug

engine/LU.m2
engine/raw-decompose.m2
engine/raw.m2

-- New errors since this afternoon:
inhom.m2

-- I placed these on hold:
gb3.m2            -- GB bug?
gbZZbug4.m2       -- GB bug?
intersect.m2      -- GB bug?
localgb.m2

-- make check
