## Project: Monomial Lookup tables in the engine

1. There have been a disturbing number of irreproducible bugs in
    monideal.cpp, I think mostly in the sat function.

2. There are several pieces of code that are very close, but not exactly the same:
    monideal, f4/f4-monlookup, schreyer-resolution/res-f4-monlookup.

3. We should also try using mathic(gb): StaticMonoMap, MonoLookup
    (which use actual tables from mathic)

4. monideal has several issues:
    - while removal is possible, we want to remove all alements that satisfy:
      the monomial is divisible by a given one.
    - note that removal is not possible for the f4 monlookup tables.
    - cannot have 2 monomials in, one which divides the other?
        This makes the use of this over ZZ not possible.
    - bug: inserting elements of high degree has a built in inefficiency.
      these routines should do a stable sort on simple degree, then insert one by one.
    - stash: these are no longer used at all.
    - intarray: it would be great to remove these.  Can we use an Arena for
      the monomials in the table? How about for the mi_node's?
    - memory allocation?  Remove all use of GC here?
    - if a monideal is contained in another object, that object needs to be finalized,
      or it should also always get its destructor run.
    - perhaps a MonomialIdealPool: a bunch of monomial ideals which use the same
      Arena's...
    - remove use of Queue.
    - remove use of intarray.

5. create a new monideal class that has the same functionality, but
    uses mathic DS's.

6. Instead of returning a vector of all matches, call a function with each?

7. There should be a top level type that includes a monideal in it.
    This encapsulated object is what is sent to the fron end (and is
    finalized).

8. Use of Index.  Instead, create an Iterator type inside
    MonomialIdeal (or MonomialTable, or whatever we end up calling
    it).

9. MonomialLookup (new name for MonomialIdeal?) will not have hash
    codes.  But MonomialIdeal will.  Make the hash code use all of the
    monomials, not just the first 5!
                
So perhaps the code in monideal should be separated into 3 parts:

    a. the table itself
    b. functions such as sat, erase, etc  These can be global functions.
    c. MonomialIdeal code for the frontend. In e/monomial-ideal.{hpp,cpp}

SparseMonomial: replace code in varpower, f4/varpower-monomial, schreyer-resolution/res-varpower-monomial
    - do not use intarray
    - each SparseMonomial has its first int a length field
    - can construct a range.
    - iterator type
    - two types? Only differences: multiplication, error checking?
    - display: ostream, give a list of strings for the names of the variables.
    - functions: isOne, isEqual, compare?, mult, quotient, lcm, divides, weight (default: all 1's), erase?
    -  also iSWellFormed.
    -  some more from varpower.hpp: radical, is_pure_power, power, to_ntuple/from_ntuple? one, var.
    
    AllocatedSparseMonomial: give a length, or other SparseMonomial.  When it goes out of scope, the
      space is deallocated.  Maybe this should just be any integer vector?  Maybe just use a std::vector?
    
types to add in?
SparseMonomial:
    pointer to an array of int's, [len, v1, e1, v2, e2, ..., vr, er], len = 2r+1
      if commutative, then v1 < v2 < ... < vr.
      if noncommutative, this does not hold.  But in any case v_i, v_(i+1) are not equal.
    SparseMonomial::size()
    SparseMonomial::range() -> SparseMonomialRange