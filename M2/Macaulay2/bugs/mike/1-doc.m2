-- Doc for July 2-3, 2006:

code(minimalPrimes,MonomialIdeal) -- this code should perhaps call instead 'dual'...
code(independentSets, MonomialIdeal)
code(independentSets, Ideal) -- check: is this correct over quotient rings?
code methods (associatedPrimes) -- uses one algorithm.  Others?
code methods adjoint
code methods adjoint1  -- reshape too...
code methods LU
code methods solve -- what rings is this restricted to?
  rawFFLU??

end

DONE    method has no documentation: Macaulay2 :: minimalPrimes(MonomialIdeal), key: (minimalPrimes, MonomialIdeal)
DONE    symbol has no documentation: Macaulay2 :: LU
DONE    method has no documentation: Macaulay2 :: LU(MutableMatrix), key: (LU, MutableMatrix)
DONE    symbol has no documentation: Macaulay2 :: solve
DONE    method has no documentation: Macaulay2 :: solve(Matrix,Matrix), key: (solve, Matrix, Matrix)
DONE    method has no documentation: Macaulay2 :: solve(MutableMatrix,MutableMatrix), key: (solve, MutableMatrix, MutableMatrix)
DONE    symbol has no documentation: Macaulay2 :: independentSets
DONE    method has no documentation: Macaulay2 :: independentSets(Ideal), key: (independentSets, Ideal)
DONE    method has no documentation: Macaulay2 :: independentSets(MonomialIdeal), key: (independentSets, MonomialIdeal)
    symbol has no documentation: Macaulay2 :: adjoint
    symbol has no documentation: Macaulay2 :: adjoint1
DONE    symbol has no documentation: Macaulay2 :: associatedPrimes

missing reference to documentation as subnode: Macaulay2 :: preimage of an ideal
missing reference to documentation as subnode: Macaulay2 :: algebraic varieties
missing reference to documentation as subnode: Macaulay2 :: Groebner bases in local rings
missing reference to documentation as subnode: Macaulay2 :: partial computation of a Groebner basis
missing reference to documentation as subnode: Macaulay2 :: Hilbert driven Groebner basis
missing reference to documentation as subnode: Macaulay2 :: assignment
missing reference to documentation as subnode: Macaulay2 :: a sample package: Quaternions

GONE? missing reference to documentation as subnode: Macaulay2 :: examples of monomial orders
GONE? missing reference to documentation as subnode: Macaulay2 :: general monomial orders
GONE? missing reference to documentation as subnode: Macaulay2 :: term orders
GONE? missing reference to documentation as subnode: Macaulay2 :: local orders
GONE? missing reference to documentation as subnode: Macaulay2 :: negative exponents

DONE symbol has no documentation: Macaulay2 :: schreyerOrder
DONE method has no documentation: Macaulay2 :: schreyerOrder(Module), key: 

Also: notes on problems with the doc:
generators-doc.m2: gens(GeneralOrderedMonoid), remove it.
  gens too in synopsis
  TO each type...
  remove see also: Monoid
  