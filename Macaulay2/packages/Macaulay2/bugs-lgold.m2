-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"

Never mind the earlier bug with
hilbertSeries/projectiveHilbertPolynomial. I figured out what was
going on.

-------------------------------------------------------------------------

The main page for "poincare" says:

"(cokernel f).cache.poincare = p -- inform the system that the Poincare
polynomial of the cokernel of f is p. This can speed the computation
of a Groebner basis of f. For details, see computing Groebner bases."

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

-------------------------------------------------------------------------

