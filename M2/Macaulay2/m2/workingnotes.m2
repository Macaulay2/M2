Mike could fix: 

     test/ISSAC-97.m2	  				    -- too big now, improve the algorithm
     test/engine-div.m2	    				    -- raw % and // need to be defined, or something
     test/gbZZbug.m2	      				    -- gb problem with bit integers
     test/galois.m2					    -- GF substitution
     test/subst3.m2					    -- GF substitution
     test/euclid.errors	    				    -- % and // in ZZ[x,Inverses=>true,MonomialOrder=>RevLex]
     test/engine/LU.m2	   				    -- in debug version, I get many smashed blocks!  Memory allocator problem?
     test/order.m2					    -- sorting of polynomials needs a total ordering
     test/slow/completeintersections.m2	    	      	    -- too slow or hung up???

Dan could fix:

=============================================================================

	  a
	  b
    o23 = c

    i24 : z#0

    o24 = a	     	  ??? put z#0 on the line?  will this affect my code?

=============================================================================

TEST's in the Macaulay2 documentation might not be getting run!


on branch : 
change 1
change 2
change 3
