--		Copyright 2011 by Daniel R. Grayson

use common;
use util;

header "

#include <M2mem.h>
#include <flint/arith.h>
#include <flint/fmpz_factor.h>

util_arrayZZ flint_factor(gmp_ZZ x) {
     int i;
     fmpz_t n;
     fmpz_set_mpz(n, x);
     fmpz_factor_t factor;
     fmpz_factor_init(factor);
     fmpz_factor(factor,n);
     int len = factor->num;
     util_arrayZZ result = getmemarraytype(util_arrayZZ,2*len+1);
     result->len = 2*len+1;
     for (i=0; i<result->len; i++) {
	  result->array[i] = (__mpz_struct *)getmem(sizeof(__mpz_struct));
     	  mpz_init(result->array[i]);
	  }
     mpz_set_si(result->array[0], factor->sign);
     for (i=0; i<len; i++) {
	  fmpz_get_mpz(result->array[2*i+1],factor->p + i);
	  fmpz_get_mpz(result->array[2*i+2],(fmpz *)(factor->exp + i));
	  }
     fmpz_factor_clear(factor);
     return result;
     }
";

import factor(x:ZZ):arrayZZ;

factorint(e:Expr):Expr := (
     when e
     is x:ZZcell do toExpr(factor(x.v))
     else WrongArgZZ());
setupfun("Flint$factorint",factorint);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d flint.o "
-- End:
