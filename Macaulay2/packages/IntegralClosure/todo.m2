To do for our package:

1. quick tests for being integrally closed
2. conductor in all cases (needs pushForward)
3. examples that are not S2, 
  make things S2 right away:
    1. have Noether normalization
    2. test for S2 (easy to check in graded or local case, via cutting by linear forms)
         inhomog case?
    3. possible strategies:
       1. take whole Jacobian ideal
       2. take a component of the radical of that
       3. remove components that are of codim >= 2.
       4. can we produce other integrally closed ideals in the conductor ideal?
       5. reduce new jacobian matrix mod radical of the old one. (and then radical again)
          maybe good to test for integral closure?
       6. adding random minors of the jacobian matrix in -- why is this not improving things?
       7. quotient ring operations: don't recompute GB's.
       8. use Trager or vanHoiej in the higher dimensional case by taking a Noether Norma
           and working over a function field.
	   How does the result of this relate to the actual integral closure?  It should be almost right...
       9. put all of these in as strategies to the algorithm
       10. curve:  do it where it sits, or project to a plane curve.  Which is better?
4. Test suite.
  Run this on several machines and algorithms and systems
  
