/* mpc.h -- Include file for mpc.
 *
 * Copyright (C) 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010, 2011, 2012, 2014, 2015 INRIA
 *
 * This file is part of GNU MPC.
 *
 * GNU MPC is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 3 of the License, or (at your
 * option) any later version.
 *
 * GNU MPC is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program. If not, see http://www.gnu.org/licenses/ .
 * */

#ifndef __MPC_H
#define __MPC_H

#include "mpfr.h"

/* Return values */

/* Transform negative to 2, positive to 1, leave 0 unchanged */
#define MPC_INEX_POS(inex) (((inex) < 0) ? 2 : ((inex) == 0) ? 0 : 1)
/* Transform 2 to negative, 1 to positive, leave 0 unchanged */
#define MPC_INEX_NEG(inex) (((inex) == 2) ? -1 : ((inex) == 0) ? 0 : 1)

/* The global inexact flag is made of (real flag) + 4 * (imaginary flag), where
 * each of the real and imaginary inexact flag are:
 * 0 when the result is exact (no rounding error)
 * 1 when the result is larger than the exact value
 * 2 when the result is smaller than the exact value */
#define MPC_INEX(inex_re, inex_im) \
          (MPC_INEX_POS(inex_re) | (MPC_INEX_POS(inex_im) << 2))
#define MPC_INEX_RE(inex) MPC_INEX_NEG((inex) & 3)
#define MPC_INEX_IM(inex) MPC_INEX_NEG((inex) >> 2)

/* For functions computing two results, the return value is
 * inexact1+16*inexact2, which is 0 iif both results are exact. */
#define MPC_INEX12(inex1, inex2) (inex1 | (inex2 << 4))
#define MPC_INEX1(inex) (inex & 15)
#define MPC_INEX2(inex) (inex >> 4)

/* Definition of rounding modes */

/* a complex rounding mode is just a pair of two real rounding modes
 * we reserve four bits for a real rounding mode.  */
typedef int mpc_rnd_t;

#define MPC_RND(r1,r2) (((int)(r1)) + ((int)(r2) << 4))
#define MPC_RND_RE(x) ((mpfr_rnd_t)((x) & 0x0F))
#define MPC_RND_IM(x) ((mpfr_rnd_t)((x) >> 4))

/* Definitions of types and their semantics */

typedef struct {
    mpfr_t re;
    mpfr_t im;
}
__mpc_struct;

typedef __mpc_struct mpc_t[1];
typedef __mpc_struct *mpc_ptr;
typedef const __mpc_struct *mpc_srcptr;

#define mpc_realref(x) ((x)->re)
#define mpc_imagref(x) ((x)->im)

#endif /* ifndef __MPC_H */

