----------------------------------------------
--hilbertkunz2
  -- An example used when trying to understand Hilbert-Kunz multiplicity
  -- Suggested by Ragnar Buchweitz or Paul Monsky
  R1 = ZZ/5[a..d,MonomialSize=>16];
  ideal{a^3-2*a^2*b-a*b^2-2*b^3+a*b*c-2*b^2*c+2*a*c^2-2*b*c^2-c^3+2*a*b*d
	      -2*b^2*d-a*c*d-2*b*c*d-2*c^2*d+a*d^2+c*d^2-d^3, a^125, b^125,
	      c^125, d^125}
