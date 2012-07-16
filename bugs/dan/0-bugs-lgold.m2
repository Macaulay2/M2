-- -*- coding: utf-8 -*-
-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"

Never mind the earlier bug with
hilbertSeries/projectiveHilbertPolynomial. I figured out what was
going on.

-------------------------------------------------------------------------

The main page for "poincare" said:

"(cokernel f).cache.poincare = p -- inform the system that the Poincare
polynomial of the cokernel of f is p. This can speed the computation
of a Gröbner basis of f. For details, see computing Gröbner bases."

Mike said this ought to be changed as well as mentioned on the gb
page, but it needs to be checked out or updated first so it is
currently commented out of the documentation.


-------------------------------------------------------------------------

hilbertFunction applied to a CoherentSheaf doesn't work as desired. It
needs to be fixed. At the moment the relevant page is commented out of
the documentation.

Also hilbertSeries applied to a CoherentSheaf doesn't work as desired. It
needs to be fixed. At the moment the relevant page is commented out of
the documentation.

-------------------------------------------------------------------------

It would be nice to have a good example of using reduceHilbert for the
documentation page. 

-------------------------------------------------------------------------

Check that the saturation is being used for hilbertSeries of a
ProjectiveVariety.

-------------------------------------------------------------------------

Would like better example for PoincareN where there are different
steps of the resolution which have the same degree.

-------------------------------------------------------------------------

It would be really nice if the coloration could not see the dashes in
a resolution or the printout of a rational function as a comment.

-------------------------------------------------------------------------

There is a problem calculating the hilbertFunction in the example for
hilbertPolynomial of a Module. At the moment the relevant code is
commented out. When the bug gets fixed, the comments need to be removed.

-------------------------------------------------------------------------

Note: how to use Macaulay 2 in emacs needs to be extremely prominent on
the main Macaulay 2 page.

This description must include information on how to comment and
uncomment an entire section of code.


-------------------------------------------------------------------------
