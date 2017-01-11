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

#include "pari-gnump.h"

/****************************************************************************/
/*                                                                          */
/* Functions converting between pari and mpz                                */
/*                                                                          */
/****************************************************************************/

void mpz_set_GEN (mpz_ptr z, GEN x)
   /* Sets z to x, which needs to be of type t_INT. */

{
   const long lx = lgefint (x) - 2;
   const long sign = signe (x);
   int i;

   assert (sizeof (long) == sizeof (mp_limb_t));

   if (typ (x) != t_INT)
      pari_err_TYPE ("mpz_set_GEN", x);

   if (sign == 0)
      mpz_set_ui (z, 0);
   else {
      mpz_realloc2 (z, lx * BITS_IN_LONG);
      z->_mp_size = sign * lx;
      for (i = 0; i < lx; i++)
         (z->_mp_d) [i] = *int_W (x, i);
   }
}

/****************************************************************************/

GEN mpz_get_GEN (mpz_srcptr z)
   /* Returns the GEN of type t_INT corresponding to z. */

{
   const long lz = z->_mp_size;
   const long lx = labs (lz);
   const long lx2 = lx + 2;
   int i;
   GEN x = cgeti (lx2);

   assert (sizeof (long) == sizeof (mp_limb_t));

   x [1] = evalsigne ((lz > 0 ? 1 : (lz < 0 ? -1 : 0))) | evallgefint (lx2);
   for (i = 0; i < lx; i++)
      *int_W (x, i) = (z->_mp_d) [i];

   return x;
}

/****************************************************************************/
/*                                                                          */
/* Functions converting between pari and mpq                                */
/*                                                                          */
/****************************************************************************/

void mpq_set_GEN (mpq_ptr q, GEN x)
   /* Sets q to x, which needs to be of type t_FRAC or t_INT. */

{
   if (typ (x) == t_FRAC) {
      mpz_set_GEN (&(q->_mp_num), gel (x, 1));
      mpz_set_GEN (&(q->_mp_den), gel (x, 2));
   }
   else if (typ (x) == t_INT) {
      mpz_set_GEN (&(q->_mp_num), x);
      mpz_set_GEN (&(q->_mp_den), gen_1);
   }
   else
      pari_err_TYPE ("mpq_set_GEN", x);
}

/****************************************************************************/

GEN mpq_get_GEN (mpq_srcptr q)
   /* Returns the GEN of type t_FRAC corresponding to q. */

{
   GEN x = cgetg (3, t_FRAC);

   gel (x, 1) = mpz_get_GEN (mpq_numref (q));
   gel (x, 2) = mpz_get_GEN (mpq_denref (q));

   return x;
}

/****************************************************************************/
/*                                                                          */
/* Functions converting between pari and mpfr                               */
/*                                                                          */
/****************************************************************************/

static void xmpn_mirrorcopy (mp_limb_t *z, mp_limb_t *x, long n)
   /* Copied from kernel/gmp/mp.c; used to revert the mantissa between
      mpfr and pari representations. */
{
  long i;
  for (i = 0; i < n; i++)
    z [i]= x [n-1-i];
}

/****************************************************************************/

void pari_mpfr_init2 (mpfr_ptr f, mpfr_prec_t prec)
   /* Works in the same way as mpfr_init2, except that the space for the
      mantissa is allocated on the pari stack. */
{
   long l;
   mp_limb_t * mant;

   assert (sizeof (long) == sizeof (mp_limb_t));

   l = nbits2nlong (mpfr_custom_get_size (prec) * 8);
   mant = new_chunk (l);
   mpfr_custom_init_set (f, MPFR_NAN_KIND, 0, prec, mant);
}

/****************************************************************************/

static void pari_mpfr_init_set_REAL (mpfr_ptr f, GEN x)
   /* Creates space for f on the pari stack and sets it to x, which is
      assumed to be a t_REAL.
      Unlike with the mpfr_init_set functions, the used precision is not the
      mpfr default precision, but that of x (also if x is 0). */
{
   assert (sizeof (long) == sizeof (mp_limb_t));

   if (signe (x) == 0) {
      long e = expo (x);
      mp_prec_t prec = (e > -2 ? 2 : -e);
      pari_mpfr_init2 (f, prec);
      mpfr_set_zero (f, 1);
   }
   else {
      long l = realprec (x) - 2;
      mp_limb_t * rc = new_chunk (l);

      xmpn_mirrorcopy (rc, x+2, l);
      mpfr_custom_init_set (f, signe (x) * MPFR_REGULAR_KIND,
                           expo (x) + 1, bit_prec (x), rc);
   }
}

/****************************************************************************/

static int mpfr_set_REAL (mpfr_ptr f, GEN x, mpfr_rnd_t rnd)
   /* Sets f to x, which is assumed to be of type t_REAL. */

{
   pari_sp av = avma;
   int inex;
   mpfr_t tmp;

   /* Copy x without loss. TODO: Share mantissa of x once it need not
      be reverted any more. */
   pari_mpfr_init_set_REAL (tmp, x);
   inex = mpfr_set (f, tmp, rnd);

   avma = av;

   return (inex);
}

/****************************************************************************/

static int mpfr_set_INT (mpfr_ptr f, GEN x, mpfr_rnd_t rnd)
   /* Sets f to x, which is assumed to be of type t_INT. */

{
   int inex;
   mpz_t tmp;

   mpz_init (tmp);
   mpz_set_GEN (tmp, x);
   inex = mpfr_set_z (f, tmp, rnd);
   mpz_clear (tmp);

   return (inex);
}

/****************************************************************************/

static void pari_mpfr_init_set_INT (mpfr_ptr f, GEN x, mpfr_prec_t prec)
   /* Creates space for f of mpfr precision prec on the pari stack and sets
      it to x, which is assumed to be a t_INT. */
{
   pari_mpfr_init2 (f, prec);
   mpfr_set_INT (f, x, MPFR_RNDN);
}

/****************************************************************************/

static int mpfr_set_FRAC (mpfr_ptr f, GEN x, mpfr_rnd_t rnd)
   /* Sets f to x, which is assumed to be of type t_FRAC. */

{
   int inex;
   mpq_t tmp;

   mpq_init (tmp);
   mpq_set_GEN (tmp, x);
   inex = mpfr_set_q (f, tmp, rnd);
   mpq_clear (tmp);

   return (inex);
}

/****************************************************************************/

static void pari_mpfr_init_set_FRAC (mpfr_ptr f, GEN x, mpfr_prec_t prec)
   /* Creates space for f of mpfr precision prec on the pari stack and sets
      it to x, which is assumed to be a t_FRAC. */
{
   pari_mpfr_init2 (f, prec);
   mpfr_set_FRAC (f, x, MPFR_RNDN);
}

/****************************************************************************/

int mpfr_set_GEN (mpfr_ptr f, GEN x, mpfr_rnd_t rnd)
   /* Sets f to x. */

{
   switch (typ (x)) {
      case t_REAL:
         return mpfr_set_REAL (f, x, rnd);
      case t_INT:
         return mpfr_set_INT (f, x, rnd);
      case t_FRAC:
         return mpfr_set_FRAC (f, x, rnd);
      default:
         pari_err_TYPE ("mpfr_set_GEN", x);
   }
}

/****************************************************************************/

void pari_mpfr_init_set_GEN (mpfr_ptr f, GEN x, mpfr_prec_t default_prec)
   /* Creates space for f on the pari stack and sets it to x.
      If x is of type t_REAL, its own precision is used, otherwise,
      default_prec is used for f. */

{
   switch (typ (x)) {
      case t_REAL:
         pari_mpfr_init_set_REAL (f, x);
         return;
      case t_INT:
         pari_mpfr_init_set_INT (f, x, default_prec);
         return;
      case t_FRAC:
         pari_mpfr_init_set_FRAC (f, x, default_prec);
         return;
      default:
         pari_err_TYPE ("pari_mpfr_init_set_GEN", x);
   }
}

/****************************************************************************/

GEN mpfr_get_GEN (mpfr_srcptr f)
   /* returns f as a t_REAL with the minimal precision required to represent
      it */
{
   if (!mpfr_number_p (f))
      pari_err_OVERFLOW ("mpfr_get_GEN");
   else if (mpfr_zero_p (f))
      return real_0_bit (-mpfr_get_prec (f));
   else {
      long l = nbits2nlong (mpfr_get_prec (f));
      GEN r = cgetr (l + 2);
      xmpn_mirrorcopy (r+2, mpfr_custom_get_significand (f), l);
      setsigne (r, (mpfr_signbit (f) ? -1 : 1));
      setexpo (r, mpfr_get_exp (f) - 1);
      return r;
   }
}

/****************************************************************************/
/*                                                                          */
/* Functions converting between pari and mpc                                */
/*                                                                          */
/****************************************************************************/

void pari_mpc_init2 (mpc_ptr c, mpfr_prec_t prec)
   /* Works in the same way as mpc_init2, except that the space for the
      mantissae is allocated on the pari stack. */
{
   pari_mpfr_init2 (mpc_realref (c), prec);
   pari_mpfr_init2 (mpc_imagref (c), prec);
}

/****************************************************************************/

void pari_mpc_init3 (mpc_ptr c, mpfr_prec_t prec_re, mpfr_prec_t prec_im)
   /* Works in the same way as mpc_init3, except that the space for the
      mantissae is allocated on the pari stack. */
{
   pari_mpfr_init2 (mpc_realref (c), prec_re);
   pari_mpfr_init2 (mpc_imagref (c), prec_im);
}

/****************************************************************************/

int mpc_set_GEN (mpc_ptr c, GEN x, mpc_rnd_t rnd)
   /* Sets c to x. */

{
   int inex_re, inex_im;

   switch (typ (x)) {
      case t_COMPLEX:
         inex_re = mpfr_set_GEN (mpc_realref (c), greal (x),
                                 MPC_RND_RE (rnd));
         inex_im = mpfr_set_GEN (mpc_imagref (c), gimag (x),
                                 MPC_RND_IM (rnd));
         break;
      case t_REAL:
      case t_INT:
      case t_FRAC:
         inex_re = mpfr_set_GEN (mpc_realref (c), x, MPC_RND_RE (rnd));
         inex_im = mpfr_set_GEN (mpc_imagref (c), gen_0, MPC_RND_IM (rnd));
         break;
      default:
         pari_err_TYPE ("mpc_set_GEN", x);
   }

   return MPC_INEX (inex_re, inex_im);
}

/****************************************************************************/

void pari_mpc_init_set_GEN (mpc_ptr c, GEN x, mpfr_prec_t default_prec)
   /* Creates space for c on the pari stack and sets it to x.
      Components of type t_REAL get their own precision, while others
      are created with default_prec. */

{
   switch (typ (x)) {
      case t_COMPLEX:
         pari_mpfr_init_set_GEN (mpc_realref (c), greal (x), default_prec);
         pari_mpfr_init_set_GEN (mpc_imagref (c), gimag (x), default_prec);
         return;
      case t_REAL:
      case t_INT:
      case t_FRAC:
         pari_mpfr_init_set_GEN (mpc_realref (c), greal (x), default_prec);
         pari_mpfr_init_set_GEN (mpc_imagref (c), gen_0, default_prec);
         return;
      default:
         pari_err_TYPE ("mpc_init_set_GEN", x);
   }
}

/****************************************************************************/

GEN mpc_get_GEN (mpc_srcptr c)
   /* returns c as a t_COMPLEX with the minimal precision required to
      represent it */
{
   GEN x = cgetg (3, t_COMPLEX);

   gel (x, 1) = mpfr_get_GEN (mpc_realref (c));
   gel (x, 2) = mpfr_get_GEN (mpc_imagref (c));

   return x;
}

/****************************************************************************/
