/*
Copyright © 2014 Andreas Enge <andreas.enge@inria.fr>

This file is part of pari-gnump.

Pari-gnump is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

Pari-gnump is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Pari-gnump.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
 * Downloaded (25/05/2016) from:
 * http://www.multiprecision.org/index.php?prog=pari-gnump
 * and modified from its original version.
 */

#include <stdio.h>
#include <limits.h>
#include <assert.h>
#include <gmp.h>
#include <mpfr.h>
#include <mpc.h>
#include <pari/pari.h>

//void mpz_set_GEN (mpz_ptr z, GEN x);
GEN mpz_get_GEN (mpz_srcptr z);

//void mpq_set_GEN (mpq_ptr q, GEN x);
GEN mpq_get_GEN (mpq_srcptr q);

//void pari_mpfr_init2 (mpfr_ptr f, mpfr_prec_t prec);
void pari_mpfr_init_set_GEN (mpfr_ptr f, GEN x, mpfr_prec_t default_prec);
//int mpfr_set_GEN (mpfr_ptr f, GEN x, mpfr_rnd_t rnd);
GEN mpfr_get_GEN (mpfr_srcptr f);

//void pari_mpc_init2 (mpc_ptr c, mpfr_prec_t prec);
//void pari_mpc_init3 (mpc_ptr c, mpfr_prec_t prec_re, mpfr_prec_t prec_im);
void pari_mpc_init_set_GEN (mpc_ptr c, GEN x, mpfr_prec_t default_prec);
//int mpc_set_GEN (mpc_ptr c, GEN x, mpc_rnd_t rnd);
GEN mpc_get_GEN (mpc_srcptr c);
