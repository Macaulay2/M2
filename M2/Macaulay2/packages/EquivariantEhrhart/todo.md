# Current TODOs

2. ** Ollie ** Check Author / Contributor details from workshop participants
   - [Current Status] I've sent around emails to everyone to see who wants / will remain
   on the Authors => {..} lists for the packages. Added other workshop participants to
   contributors.
   So far: other than OC, VC, VR:
   > Sean Grate replied -> author
   > Karolyn So replied -> moves to contributor
   > Mike Cummings replied -> author
   > Alex Milner replied -> moved to contributor
   > Vincenzo Reda -> added website
   > Benoît Guerville-Ballé -> confirmed contributor only

6. ** Vic ** Add M2Version minimum requirement because of the link to the Permutations package (EE)
   and the Normaliz fix (RP)
    
7. ** Vic ** Protect against non-cycle group inputs in `equivariantEhrhartSeries`
    
8. ** Vic ** In doc for `ehrhartSeries`, the return type is not in `frac(QQ[t])`, it's of class
   `Divide` if `R` is unspecified

11. ** Vin **
    Get the tests (hStarVector, ehrhartQP) from Vincenzo incorporated into the package

12. ** Vin ** Make sure the tests cover enough cases and fill in any missing doc nodes

13. ** Everyone ** Read the docs of all packages

14. **Everyone** Check for other TODOs in the files and fix/report them:
    > (EE) fixed `alarm` TODO in `generateGroup`
    > Clean up the end of (RP) file - collect and present examples nicely and remove code we don't need

    
    

# Long Term / Future TODOs

1. Implement `equivariantEhrhartSeries` for non-cyclic groups. For this, it would be more
   fruitful / reasonable to interface to existing Group software E.g. GAP, this might need an
   interfacing package of its own.
    
2. Add hypersimplex and permutohedron into Polyhedra package as standard constructions 


Here's how the code for hypersimplex and permutohedron might be implemented.
```macaulay2
-- could add these two to Polyhedra

hypersimplex
hypersimplex = method()
hypersimplex(ZZ, ZZ) := (n, k) -> (
    convexHull transpose matrix for s in subsets(n, k) list (
    for i from 0 to n-1 list if member(i, s) then 1 else 0
    )
)

permutohedron = method()
permutohedron ZZ := n -> (
    convexHull transpose matrix permutations n
)
```
4. Resolve generateGroup problem

   We have a function called `generateGroup` in `EquivariantEhrhart` that takes a list of
   matrices and outputs the group (as a list of elements) that they generate (under
   multiplication). Currently, we only use this for cycle groups. Note that for permutation
   groups, we never work with the full group, only conjugacy class representatives and a
   minimal (2-element) generating set.

   It serves minimal use now but could be used more in the future - but maybe we should
   not be 'reinventing the wheel' and just call GAP (if possible) for getting character
   tables of general finite groups.

5. Resolve the future todos from `RationalPolytopes`:

```
----------------------------------
-- Plans for future development --
----------------------------------

-- To-do list --

-- check exported functions work with easy examples
-- that can be computed by hand


-- implement a method for internalQuasiPolynomial that implements the following procedure:
-- 1) check the cache for a stored list of polynomials
-- 2) if there is no list, use the coefficients matrix to produce a list of polynomials and cache them
-- 3) take the input i modulo the period to obtain j, and return polynomial number j evaluated at i


-- decide what should be done if we try to create a quasi polynomial of period 1.
-- it's just a polynomial! So should we return a genuine polynomial or not?


-- if a quasi polynomial is made from a polytope, then store a reference to that polytope in the cache of
-- the quasi polynomial


-- cache the quasi-polynomial in the polyhedron and avoid recomputing the quasi-polynomial if it is already cached
-- note that the Polyhedron type is just a hashtable with a single entry: cache


-- check the definition of hStarPolynomial polynomial in literature and check whether the denominator of the Ehrhart series is:
-- (1 - t^(denominator P))^(dim P)  or  (1 - t^(period P))^(dim P)
-- Answer:
--   in the literature, authors typically do not define a denominator / hstar polynomial for rational polytopes
--


-- simplify the names of functions: E.g. ehrhartQP -> Ehrhart (overriding the one in Polyhedra)
-- or, if we don't want to override, then choose a name without abbreviations: e.g. ehrhartQuasiPolynomial
-- function names and variables should start with lower case
-- periodQP -> period (may need to change the key in the QuasiPolynomial type)


-- think about how a user might interact with the package and what would make life easier for them.
-- E.g. A user comes along with a polytope in mind: either they know the vertices or a half-space description
--      the user want to compute the Ehrhart quasi-polynomial, Ehrhart series, hStarPolynomial poly, delta-vector (coefficients of hStarPolynomial poly)


-- Whenever we perform a computation, e.g. computing the ehrhartQP, store the result in the cache
-- and before performing computations, check if we have already computed it by checking the cache
-- a useful piece of code is:
C = new CacheTable from {1 => "hi"}
C#?1 -- 1 is a key of the hash table
C#?2 -- but 2 is not
```


# DONE


1. Fix ReturnDenominator bug (fix tests 6 and 8) in RationalPolytopes [Done]

3. Remove the Normaliz patch [Done]

9. ** Ollie **
    [Ollie: move all the working files to MY FORK and give push access to Victoria and Vincenzo]
    Tidy up unnecessary files. An example / demo file is good to have at the base level of
    the repo. But the test files and todos can be moved into another folder. We should also
    move the `RationalPolytopes.m2` up to the base level.

    To incorpate the packages into M2:

    Create Fork -> Add packages to the Fork -> Pull Request

    Always be checking the examples and other files for things that we may want in the final packages
    and move them over to the included files.

10. Get Vincenzo and Alex into M2 org to be able to push changes OR use PullRequests
    [consequence of 9.]

5. ** Ollie **
   (EE) We should add a test to make sure that the cycleRepresentatives and SymmetricGroupTable
   from BettiCharacters has compatible ordering AND make a note of it in the docs

   - Added test for character table of S_5.
     This test will fail if something changes in the code that causes the order of the partitions
     to change.
   - Added some caveats to the documentation nodes for `cycleTypeRepresentatives` and
     `representationRing`. For code maintenance, we require that the function `partitions` must
     be deterministic. If this fails in the future, then we will need to sort the partitions and
     possibly modify the character table code so that it uses the sorted partitions. 


# Old TODOs 

things to update:

  1. change how isSymmetric Matrix works (use sets)
  2. update partitionToPermutation
  3. `hStarPolynomial(Polytope,Matrix) = H_P^*` where $H_P^*$ comes from the equivariant Ehrhart series $EES_P = \frac{H_P^*(t)}{\det(I - t \rho)}$ with $\rho$ the natural representation of group generated by the matrix $g$.
  4. Need a way to construct the _entire_ group generated by a list of generators. In particular, the cyclic group generated by $g$.
  5. Compute $P_{g^i}$, the polytope fixed by $g^i$, as well as its Ehrhart series.
  6. Look at $\mathfrak{S}_n$ case to put these Ehrhart series together to obtain $H_P^*$.

