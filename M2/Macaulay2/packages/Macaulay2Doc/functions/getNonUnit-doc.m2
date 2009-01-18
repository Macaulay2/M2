-- -*- coding: utf-8 -*-
--- status: Draft
--- author(s): MES
--- notes: This function is not functional yet

document {
     Key =>  getNonUnit,
     Headline => "retrieve a previously discovered non-unit",
     Usage => "getNonUnit R",
     Inputs => {
	  "R" => Ring => "in which division by a non-unit may have been attempted"
	  },
     Outputs => {
	  RingElement => {"the non-unit, if any, or ", TO null}
	  },
     "If a ring has been declared to be a field, using ", TO toField, " or ", TO frac, ", but a nonzero element is
     found to not be a unit, this routine will return that element, otherwise ", TO null, " is returned.",
     EXAMPLE lines ///
     	  A = ZZ/101[a]/(a^2-1);
	  toField A
	  1//(a-1)
	  getNonUnit A
          ///,
     "Warning: this function does not work yet for divisions attempted in the course
     of computing a Gröbner basis or resolution.",
     SeeAlso => { "toField", "frac" }
     }

