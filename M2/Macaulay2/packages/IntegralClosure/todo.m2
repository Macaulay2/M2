------------------------------
todo MES, 8-19-09

- change names of variables of the output

- remove use of frac R, in the case when we don't have a Noether normalization?
-  or change frac code to not do automatic simplification in this case?
-  perhaps the code should only reduce if the given polynomials have a common factor
-  over the actual poly ring?

- write and rewrite doc
- use good fractions
- choice of NNL ideal to start
- maybe ALWAYS use fractional ideal code?  Even in the non-Noether normal case

-------------------------------

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
       11. reduce first birationally to a hypersurface (option).
4. Test suite.
  Run this on several machines and algorithms and systems
  
understand:
S2-ification: is this worth doing?
why does Hom(I^-1,I^-1) seem so hard to compute?

talking 4/30/09 before I leave MSRI:

a. ringFromFractions(H,fs,F,mus) could use fractions not expressed over a common denominator.
  This might produce higher quality fractions
  
  What others ways can we do produce better fractions?
  e.g. gens gb syz matrix{{h,f}}.

b. Don't take entire Jacobian ideal.  Lots of choices here.
  n at the beginning
  add some new ones in each step
  discriminant
  
c. rewrite ringFromFractions to do the computation in the base somehow. (as in GLS?)
