--- status: Draft
--- author(s): MES
--- notes: This function is not functional yet

document {
     Key => {(getNonUnit,Ring) getNonUnit},
     Headline => "retrieve a previously discovered non-unit",
     Usage => "getNonUnit R",
     Inputs => {
	  "R" => "a ring in which division by a non-unit has been attempted"
	  },
     Outputs => {
	  "the non-unit"
	  },
     "If a ring has been delcared to be a field, using ", TO toField, ", but a nonzero element is
     found to not be a unit, this routine will return that element.",
     EXAMPLE lines ///
     	  A = ZZ/101[a]/(a^2-1)
	  toField A
	  1/(a-1)
          ///,
     "Warning: this function does not work yet for divisions attempted in the course
     of computing a Groebner basis or resolution.",
     SeeAlso => { "toField" }
     }

