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
     __mpz_struct *tmp;
     tmp = (void *)getmem(sizeof(__mpz_struct));
     mpz_init(tmp);
     mpz_set_si(tmp, factor->sign);
     result->array[0] = tmp;
     for (i=0; i<len; i++) {
	  tmp = (void *)getmem(sizeof(__mpz_struct));
     	  mpz_init(tmp);
	  fmpz_get_mpz(tmp,factor->p + i);
	  result->array[2*i+1] = tmp;
	  tmp = (void *)getmem(sizeof(__mpz_struct));
     	  mpz_init(tmp);
	  fmpz_get_mpz(tmp,(fmpz *)(factor->exp + i));
	  result->array[2*i+2] = tmp;
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
