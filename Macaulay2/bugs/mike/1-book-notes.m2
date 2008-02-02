-- Notes on going through the book chapter outputs in version 1.0.9test
-- 29 Jan 2008

-------------
-- preface --
-------------

OK.  The output seems good.

Output placed in test.out.expected

---------------
-- varieties --
---------------

OK.  The output seems good.

Notes:
1. oo / print @@ print
   Another good way to view it: netList oo

2. MonomialSize=>16.  The default is currently 16 or 32.

Output placed in test.out.expected

--------------
-- geometry --
--------------

Output: problem, although it looks correct.
FIX THIS or UNDERSTAND IT!!
The problem is on lines o82-o85 (or even up to o90).  It is probably correct, but
  it would be nicer if Hom would produce nicer generating sets? (As it seems to have done
  in 0.9.2)

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

Output placed in test.out.expected

-------------
-- schemes --
-------------

Output: The det computation on line i28 takes quite a long time 
(fixed by using Strategy=>Cofactor).
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
   saturate(J + minors(2,jacobian J)) == 1
   (this test placed into the test.m2 file).

Output placed in test.out.expected

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

Output placed in test.out.expected
   
----------------------------------------------------
-- from enumerative geometry to solving equations --
----------------------------------------------------

Output: is fine, except the random test as in note 1. gives a different answer.
Fixed by adding in a 'random 10', before the call that fails...

Notes: 
1. line i65, the test fails.  Rerunning it often makes it succeed.  The reason the test fails
   is that Macaulay 2's random number generator has changed since the book was written.

Output placed in test.out.expected

--------------------------------
-- complete intersections ------
--------------------------------

Output: o32: gives FALSE, not TRUE...
The rest of the output seems ok.

Notes:
o48: degrees are correct, they are no longer displayed.
i67: the code has changed slightly, mostly involving Adjust and Repair (having been changed
     to use Heft).
i136: the time seems to be slower now... (well, faster, but only twice as fast)
i146: used 7.6 sec, book: .13 sec
i148: used 3.4 sec, book: .15 sec
i149: used 3.4 sec, book: .18 sec
i152: used 7.6 sec, book: .39 sec
i153: used 3.5 sec, book: .12 sec
i155: used 7.8 sec, book: .97 sec
i156: used 3.1 sec, book: 1.73 sec

--------------------------------
-- toric Hilbert schemes -------
--------------------------------

Output:

Notes:
1. One does not need to load the file LLL.m2 (in fact, there is no such file!?)
2. polarCone.m2 is now a real package: FourierMotzkin.m2, but the name of the function is fourierMotzkin,
     not polarCone.
3. The computation on line i53 returns a different value.  This is because the choice of variables 
     to remove has changed, probably due to a difference in sorting order of polynomials.
4. The input for lines i55,i56,i57,i58 should now read:
  CX = QQ[a..e, z_1,z_5,z_6,z_11, Weights =>
      {9,3,5,0,0,0,0,0,0}];
  F = map(CX, ring J, matrix{{a,b,c,d,e}} | 
            substitute(G.matrix,CX))
  J1 = F J
  substitute(ideal(z_5^2),CX) + J1
     NOTE TO us: perhaps changing minPres.m2 would be preferable, if that is possible?

5. The output on line o63 is different, as the output of Groebner bases (and
      consequently the output from leadTerm) is now sorted by lead term.
6. The output on line o66 is different as in note 4 above.  The primary decomposition of the
     ideal on line o66 can be found by:
  primaryDecomposition value o66

Output placed in test.out.expected

--------------------------
-- sheaf algorithms ------
--------------------------

Output: it is now OK.

Notes:
1. Dan fixed the one problem in here (line i73 is changed, as the presentation
of FHM comes out in a different order...

Output placed in test.out.expected

---------------------------
-- constructions ----------
---------------------------

Output: Now is fine, after changing the DegreeRank of the tensor products...

Notes:
1. I need to check the other routines, since they aren't actually checked here.

Output placed in test.out.expected

--------------------------
-- d-modules -------------
--------------------------

Output:
o30: the Boperator is DIFFERENT.  Is this a problem?  The other parts of the hash table on o30 and o31 seem
to be fine.
o41: LocModule is different.
o45: output differs, but is probably fine.  In fact, the new one seems better...
o88: The output is different.  Is this OK?

Notes:
1. On line o14:
  Note that some variables are given as '$s' in the book, but as 's' in the
  current M2. If one does 'use ring o14', then s can be used as a variable.
2. gb no longer displays its output.  After line o25, do
  gens oo
  to see the Groebner basis from the previous line.
3. The Groebner basis on line o45 is not reduced, where in the current version it is.

Emailed Anton about this.

Output placed in test.out.expected.  This file contains these suspect differences)