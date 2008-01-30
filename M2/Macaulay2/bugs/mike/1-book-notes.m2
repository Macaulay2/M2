-- Notes on going through the book chapter outputs in version 1.0.9test
-- 29 Jan 2008

-------------
-- preface --
-------------

OK.  The output seems good.

---------------
-- varieties --
---------------

OK.  The output seems good.

Notes:
1. oo / print @@ print
   Another good way to view it: netList oo

2. MonomialSize=>16.  The default is currently 16 or 32.

--------------
-- geometry --
--------------

Output: problem, although it looks correct.
FIX THIS or UNDERSTAND IT!!

Notes:
1. 'top' has been replaced with 'topComponents'
2. decompose works over QQ

-----------------
-- programming --
-----------------

OK.  The output seems good.

Notes:
1. Real numbers can also be input as, e.g. 2e100
     Real numbers can have arbitrary precision now.
2. Function is now called FunctionClosure
3. toExternalString shoul be used instead of toString to obtain 
     machine readable input.
4. Comments can also be multi-line, enclosed by {* and *}
5. Another format for strings: /// ... ///
6. Other newer language features?

-------------
-- schemes --
-------------

Output: The det computation on line i28 takes quite a long time
Otherwise the output seems good.

Notes:
1. Section 5: One can now use local rings in Macaulay2.  For example:
  S = QQ{x,y,z}
  I = ideal(x^5+y^3+z^3, x^3+y^5+z^3, x^3+y^3+z^5);
  degree I
2. Section 8: The definition of blowup given here is special to this stuation.
   The output on line o70 in the book contains a non-minimal generator (the last one).
   The check for non-singularity in line i71 in the book is not correct.  One needs to
   take the 2 x 2 minors of the Jacobian (since codim J == 2):
   radical(J + minors(2,jacobian J)) == ideal gens ring J

--------------------
-- monomialIdeals --
--------------------

Output: is OK, although we need to make one additional change, as in note (3).

Notes:
1. Many of the algorithms mentioned here are now implemented directly in Macaulay2.
2. The 'dual' and primaryDecomposition functions displayed on lines o26 and o27 
   have been rewritten.
3. primaryDecomposition now returns an irredundant primary decomposition.  
   Use irreducibleDecomposition instead of primaryDecomposition:
   -- line i32:
   apply(2..6, i -> #irreducibleDecomposition treeIdeal i)
4. erase symbol x
   instead, use:
   x = symbol x
5. For the discussion between i108, i109: the first entry of a degree vector does not
   need to be positive.  However, a Heft vector is required in the ring in that case.   
   
