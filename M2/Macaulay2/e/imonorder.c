
#include "config.h"
#include <stdio.h>
#if HAVE_STDINT_H
#include <stdint.h>
#elif HAVE_INTTYPES_H
#include <inttypes.h>
#else
#error integer type definitions not available
#endif

#include "imonorder.h"
#include "../d/M2mem.h"
/* TODO:
   -- negative exponent versions need to be included (at least for MO_LEX)
   -- non-commutative blocks should be added in
*/

static int ntmpexp = 0;
static int *tmpexp = 0; /* Set to an array 0..ntmpexp-1 of ints */

static void mo_block_revlex(struct mo_block *b, int nvars)
{
  b->typ = MO_REVLEX;
  b->nvars = nvars;
  b->nslots = nvars;
  b->first_exp = 0; /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_grevlex(struct mo_block *b, int nvars)
{
  b->typ = MO_GREVLEX;
  b->nvars = nvars;
  b->nslots = nvars;
  b->first_exp = 0; /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_grevlex2(struct mo_block *b, int nvars)
{
  b->typ = MO_GREVLEX2;
  b->nvars = nvars;
  b->nslots = (nvars+1)/2; /* 2 per word */
  b->first_exp = 0; /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_grevlex4(struct mo_block *b, int nvars)
{
  b->typ = MO_GREVLEX4;
  b->nvars = nvars;
  b->nslots = (nvars + 3)/4;  /* 4 per word */
  b->first_exp = 0; /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_grevlex_wts(struct mo_block *b, int nvars)
{
  b->typ = MO_GREVLEX_WTS;
  b->nvars = nvars;
  b->nslots = nvars;
  b->first_exp = 0; /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = nvars;
  b->weights = 0; /* will be set later */
}

static void mo_block_grevlex2_wts(struct mo_block *b, int nvars)
{
  b->typ = MO_GREVLEX2_WTS;
  b->nvars = nvars;
  b->nslots = (nvars+1)/2; /* 2 per word */
  b->first_exp = 0; /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = nvars;
  b->weights = 0; /* will be set later */
}

static void mo_block_grevlex4_wts(struct mo_block *b, int nvars)
{
  b->typ = MO_GREVLEX4_WTS;
  b->nvars = nvars;
  b->nslots = (nvars + 3)/4;  /* 4 per word */
  b->first_exp = 0; /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = nvars;
  b->weights = 0; /* will be set later */
}

static void mo_block_lex(struct mo_block *b, int nvars)
{
  b->typ = MO_LEX;
  b->nvars = nvars;
  b->nslots = nvars;
  b->first_exp = 0; /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_lex2(struct mo_block *b, int nvars)
{
  b->typ = MO_LEX2;
  b->nvars = nvars;
  b->nslots = (nvars+1)/2; /* 2 per word */
  b->first_exp = 0; /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_lex4(struct mo_block *b, int nvars)
{
  b->typ = MO_LEX4;
  b->nvars = nvars;
  b->nslots = (nvars + 3)/4;  /* 4 per word */
  b->first_exp = 0; /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_group_lex(struct mo_block *b, int nvars)
{
  b->typ = MO_LAURENT;
  b->nvars = nvars;
  b->nslots = nvars;
  b->first_exp = 0; /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_group_revlex(struct mo_block *b, int nvars)
{
  b->typ = MO_LAURENT_REVLEX;
  b->nvars = nvars;
  b->nslots = nvars;
  b->first_exp = 0; /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_wt_function(struct mo_block *b, int nvars, double *wts)
{
  b->typ = MO_WEIGHTS;
  b->nvars = 0;
  b->nslots = 1;
  b->first_exp = 0;
  b->first_slot = 0; /* will be set later */
  b->nweights = nvars;
  b->weights = wts;
}

MonomialOrder *monomialOrderMake(const MonomialOrdering *mo)
{
  MonomialOrder *result;
  int i,j,nv,this_block;
  double *wts = NULL;
  /* Determine the number of variables, the number of blocks, and the location
     of the component */
  int nblocks = 0;
  int nvars = 0;
  int hascomponent = 0;
  int isnoncomm = 0;
  for (i=0; i<mo->len; i++)
    {
      struct mon_part_rec_ *t = mo->array[i];
      nblocks++;
      if (t->type == MO_POSITION_DOWN || t->type == MO_POSITION_UP)
	hascomponent++;
      else if (t->type == MO_NC_LEX)
	isnoncomm = 1;
      if (t->type != MO_WEIGHTS)
	nvars += t->nvars;
    }
  nblocks -= hascomponent;

  /* Now create the blocks, and fill them in. Also fill in the deg vector */
  result = (MonomialOrder *) getmem(sizeof(MonomialOrder));
  result->nvars = nvars;
  result->nslots = 0;
  result->nblocks = nblocks;
  result->blocks = (struct mo_block *) getmem(nblocks * sizeof(struct mo_block));
  result->degs = (double *) getmem_atomic(nvars * sizeof(double));
  if (hascomponent == 0)
    result->nblocks_before_component = nblocks;

  this_block = 0;
  nvars = 0;
  for (i=0; i<mo->len; i++)
    {
      struct mon_part_rec_ *t = mo->array[i];
      if (t->type != MO_WEIGHTS)
	{
	  if (t->wts == 0)
	    for (j=0; j<t->nvars; j++)
	      result->degs[nvars++] = 1;
	  else
	    for (j=0; j<t->nvars; j++)
	      result->degs[nvars++] = t->wts[j];
	}
      else
	{
	  wts = (double *) getmem_atomic(t->nvars * sizeof(double));
	  for (j=0; j<t->nvars; j++)
	    wts[j] = t->wts[j];
	}
      switch (t->type) {
      case MO_REVLEX:
	mo_block_revlex(result->blocks + this_block++, t->nvars);
	break;
      case MO_GREVLEX:
	mo_block_grevlex(result->blocks + this_block++, t->nvars);
	break;
      case MO_GREVLEX2:
	mo_block_grevlex2(result->blocks + this_block++, t->nvars);
	break;
      case MO_GREVLEX4:
	mo_block_grevlex4(result->blocks + this_block++, t->nvars);
	break;
      case MO_GREVLEX_WTS:
	mo_block_grevlex_wts(result->blocks + this_block++, t->nvars);
	break;
      case MO_GREVLEX2_WTS:
	mo_block_grevlex2_wts(result->blocks + this_block++, t->nvars);
	break;
      case MO_GREVLEX4_WTS:
	mo_block_grevlex4_wts(result->blocks + this_block++, t->nvars);
	break;
      case MO_LEX:
	mo_block_lex(result->blocks + this_block++, t->nvars);
	break;
      case MO_LEX2:
	mo_block_lex2(result->blocks + this_block++, t->nvars);
	break;
      case MO_LEX4:
	mo_block_lex4(result->blocks + this_block++, t->nvars);
	break;
      case MO_WEIGHTS:
	mo_block_wt_function(result->blocks + this_block++, t->nvars, wts);
	break;
      case MO_LAURENT:
	mo_block_group_lex(result->blocks + this_block++, t->nvars);
	break;
      case MO_LAURENT_REVLEX:
	mo_block_group_revlex(result->blocks + this_block++, t->nvars);
	break;
      case MO_NC_LEX:
	/* MES */
	break;
      case MO_POSITION_UP:
	if (--hascomponent == 0)
	  {
	    // Set the information about the component
	    result->component_up = 1;
	    result->nblocks_before_component = this_block;
	  }
	//  mo_block_position_up(result->blocks + this_block);
	break;
      case MO_POSITION_DOWN:
	if (--hascomponent == 0)
	  {
	    // Set the information about the component
	    result->component_up = 0;
	    result->nblocks_before_component = this_block;
	  }
	//  mo_block_position_down(result->blocks + this_block);
	break;
      }
    }

  /* Go back and fill in the 'slots' information */
  /* Now fix the first_exp, first_slot values, and also result->{nslots,nvars}; */
  nv = 0;
  result->nslots = 0;
  for (i=0; i<nblocks; i++)
    {
      enum MonomialOrdering_type typ = result->blocks[i].typ;


      result->blocks[i].first_exp = nv;
      result->blocks[i].first_slot = result->nslots;
      nv += result->blocks[i].nvars;
      result->nslots += result->blocks[i].nslots;

      if (typ == MO_WEIGHTS)
	{
	  result->blocks[i].first_exp = 0;

	  /* divide the wt vector by the degree vector */
	  for (j=0; j<result->blocks[i].nvars; j++)
	    result->blocks[i].weights[j] /= result->degs[j];
	}
      else if (typ == MO_GREVLEX_WTS || typ == MO_GREVLEX2_WTS || typ == MO_GREVLEX4_WTS)
	{
	  result->blocks[i].weights = result->degs + result->blocks[i].first_exp;
	}

      if (i == result->nblocks_before_component-1)
	{
	  result->nslots_before_component = result->nslots;
	}
    }

  /* Set is_laurent */
  result->is_laurent = (int *) getmem_atomic(result->nvars * sizeof(int));
  for (i=0; i<result->nvars; i++) result->is_laurent[i] = 0;

  for (i=0; i<result->nblocks; i++)
    if (result->blocks[i].typ == MO_LAURENT
	|| result->blocks[i].typ == MO_LAURENT_REVLEX)
      {
	for (j=0; j < result->blocks[i].nvars; j++)
	  result->is_laurent[result->blocks[i].first_exp + j] = 1;
      }

  /* Set tmpexp */
  if (result->nvars >= ntmpexp)
    {
      ntmpexp = result->nvars+1;
      tmpexp = (int *) getmem_atomic(ntmpexp *sizeof(int));
    }
  return result;
}

extern void monomialOrderFree(MonomialOrder *mo)
{
}

#ifdef HAVE_STDDEF_H
/* we include this because on suns it includes <sys/isa_defs.h> */
#include <stddef.h>
#endif

#ifdef HAVE_ENDIAN_H
#include <endian.h>
#endif

#if defined(__BYTE_ORDER) && defined(__LITTLE_ENDIAN) && __BYTE_ORDER == __LITTLE_ENDIAN || defined(_LITTLE_ENDIAN)

union pack4 {
  int32_t i;
  struct {
    uint8_t a;
    uint8_t b;
    uint8_t c;
    uint8_t d;
  } ch;
};

union pack2 {
  int32_t i;
  struct {
    uint16_t a;
    uint16_t b;
  } ch;
};
#else
union pack4 {
  int32_t i;
  struct {
    uint8_t d;
    uint8_t c;
    uint8_t b;
    uint8_t a;
  } ch;
};

union pack2 {
  int32_t i;
  struct {
    uint16_t b;
    uint16_t a;
  } ch;
};
#endif
	
#if 0  
static bool MO_pack4(int nvars, const int *expon, int *slots)
// return false if any of the non-negative exponents are >= 128.
{
  int x;
  union pack4 w;
  while (nvars > 0)
    {
      w.i = 0;
      if (--nvars >= 0) {
	x = *expon++;
	if (x >= 128) return false;
	w.ch.d = x;
	if (--nvars >= 0) {
	  x = *expon++;
	  if (x >= 128) return false;
	  w.ch.c = x;
	  if (--nvars >= 0) {
	    x = *expon++;
	    if (x >= 128) return false;
	    w.ch.b = x;
	    if (--nvars >= 0) {
	      x = *expon++;
	      if (x >= 128) return false;
	      w.ch.a = x;
	    }
	  }}}
      *slots++ = w.i;
    }
  return true;
}

static bool MO_pack2(int nvars, const int *expon, int *slots)
// return false if any of the non-negative exponents are >= (1<<15) == 32768
{
  int x;
  union pack2 w;
  while (nvars > 0)
    {
      w.i = 0;
      if (--nvars >= 0) {
	x = *expon++;
	if (x >= 32768) return false;
	w.ch.b = x;
	if (--nvars >= 0) {
	  x = *expon++;
	  if (x >= 32768) return false;
	  w.ch.a = *expon++;
	}
      }
      *slots++ = w.i;
    }
  return true;
}
#endif

//static int MO_checksize(int n, const int *a, int leftmask)
//{
//  for ( ; n > 0; --n)
//    if (*a++ & leftmask) return 0;
//  return 1;
//}
//static long MO_packit4(int nvars, const int *a)
//{
//  long result = ((*a++) & rightmask);
//  result = (result << 8) | ((*a++) & 0x07f);
//  result = (result << 8) | ((*a++) & 0x07f);
//  result = (result << 8) | ((*a++) & 0x07f);
//  return result;
//}

static void MO_pack4(int nvars, const int *expon, int *slots)
{
  union pack4 w;
  while (nvars > 0)
    {
      w.i = 0;
      if (--nvars >= 0) {
	w.ch.d = *expon++;
	if (--nvars >= 0) {
	  w.ch.c = *expon++;
	  if (--nvars >= 0) {
	    w.ch.b = *expon++;
	    if (--nvars >= 0) w.ch.a = *expon++;
	  }}}
      *slots++ = w.i;
    }
}

static void MO_pack2(int nvars, const int *expon, int *slots)
{
  union pack2 w;
  while (nvars > 0)
    {
      w.i = 0;
      if (--nvars >= 0) {
	w.ch.b = *expon++;
	if (--nvars >= 0) w.ch.a = *expon++;
      }
      *slots++ = w.i;
    }
}

static void MO_unpack4(int nvars, const int *slots, int *expon)
{
  union pack4 w;
  while (nvars > 0)
    {
      w.i = *slots++;
      if (--nvars >= 0) {
	*expon++ = w.ch.d;
	if (--nvars >= 0) {
	  *expon++ = w.ch.c;
	  if (--nvars >= 0) {
	    *expon++ = w.ch.b;
	    if (--nvars >= 0) *expon++ = w.ch.a;
	  }}}
    }
}

static void MO_unpack2(int nvars, const int *slots, int *expon)
{
  union pack2 w;
  while (nvars > 0)
    {
      w.i = *slots++;
      if (--nvars >= 0) {
	*expon++ = w.ch.b;
	if (--nvars >= 0) *expon++ = w.ch.a;
      }
    }
}

void monomialOrderEncode(const MonomialOrder *mo, 
			    const_exponents expon, 
			    monomial result_psums)
     /* Given 'expon', compute the encoded partial sums value */
{
  if (mo == 0) return;
  int i,j,nvars,s;
  int *p1;
  struct mo_block *b = mo->blocks;
  int nblocks = mo->nblocks;
  const int *e = expon;
  int *p = result_psums;
  for (i=nblocks; i>0; --i, b++)
    switch (b->typ) {
    case MO_LEX:
    case MO_LAURENT:
      nvars = b->nvars;
      for (j=0; j<nvars; j++)
	*p++ = *e++;
      break;
    case MO_REVLEX:
    case MO_LAURENT_REVLEX:
      nvars = b->nvars;
      for (j=0; j<nvars; j++)
	*p++ = - *e++;
      break;
    case MO_GREVLEX:
    case MO_GREVLEX_WTS:
      nvars = b->nvars;
      p += b->nslots;
      p1 = p;
      *--p1 = *e++;
      for (j=1; j<nvars; j++)
	{
	  --p1;
	  *p1 = *e++ + p1[1];              // check overflow here?
	}
      break;
    case MO_GREVLEX4:
    case MO_GREVLEX4_WTS:
      nvars = b->nvars;
      p1 = tmpexp + b->nvars;
      *--p1 = *e++;
      for (j=1; j<nvars; j++)
	{
	  --p1;
	  *p1 = *e++ + p1[1];
	}
      MO_pack4(nvars,p1,p);
      p += b->nslots;
      break;
    case MO_GREVLEX2:
    case MO_GREVLEX2_WTS:
      nvars = b->nvars;
      p1 = tmpexp + b->nvars;
      *--p1 = *e++;
      for (j=1; j<nvars; j++)
	{
	  --p1;
	  *p1 = *e++ + p1[1];
	}
      MO_pack2(nvars,p1,p);             // check overflow here?
      p += b->nslots;
      break;
    case MO_LEX4:
      nvars = b->nvars;
      MO_pack4(nvars,e,p);             // check overflow here?
      p += b->nslots;
      e += nvars;
      break;
    case MO_LEX2:
      nvars = b->nvars;
      MO_pack2(nvars,e,p);             // check overflow here?
      p += b->nslots;
      e += nvars;
      break;
    case MO_WEIGHTS:
      s = 0;
      for (j=0; j<b->nweights; j++)
	s += b->weights[j] * expon[j];             // check overflow here?
      *p++ = s;
      break;
    case MO_POSITION_UP:
    case MO_POSITION_DOWN:
      /* nothing to do here */
      break;
    case MO_NC_LEX:
      /* nothing to do here */
      break;
    }
}

int monomialOrderFromActualExponents(const MonomialOrder *mo, 
				      const_exponents expon, 
				      exponents result_exp)
/* Sets result_exp[i] = expon[i] * deg[i], i=0..nvars-1.
   If a variable is negative, and shouldn't be, 0 is returned.
   Otherwise 1 is returned. */
{
  if (mo == 0) return 1;
  int i;
  int result = 1;
  for (i=0; i<mo->nvars; i++)
    {
      result_exp[i] = expon[i] * mo->degs[i];
      if (expon[i] < 0)
	{
	  if (!mo->is_laurent[i])
	    result = 0;
	}
    }
  return result;
}

int monomialOrderToActualExponents(const MonomialOrder *mo, 
				      const_exponents expon, 
				      exponents result_exp)
{
  if (mo == 0) return 1;
  int i;
  for (i=0; i<mo->nvars; i++)
    {
      result_exp[i] = expon[i] / mo->degs[i];
    }
  return 1;
}

void monomialOrderDecode(const MonomialOrder *mo, const_monomial psums, exponents expon)
{
  if (mo == 0) return;
  int i,j,nvars;
  struct mo_block *b = mo->blocks;
  int nblocks = mo->nblocks;
  int *e = expon;
  const int *p = psums;
  for (i=nblocks; i>0; --i, b++)
    switch (b->typ) {
    case MO_LEX:
    case MO_LAURENT:
      nvars = b->nvars;
      p = psums + b->first_slot;
      e = expon + b->first_exp;
      for (j=0; j<nvars; j++)
	*e++ = *p++;
      break;
    case MO_REVLEX:
    case MO_LAURENT_REVLEX:
      nvars = b->nvars;
      p = psums + b->first_slot;
      e = expon + b->first_exp;
      for (j=0; j<nvars; j++)
	*e++ = - *p++;
      break;
    case MO_GREVLEX:
    case MO_GREVLEX_WTS:
      nvars = b->nvars;
      p = psums + b->first_slot + nvars - 1;
      e = expon + b->first_exp;
      *e++ = *p--;
      for (j=nvars-1; j>=1; --j, --p)
	*e++ = *p - p[1];
      break;
    case MO_GREVLEX4:
    case MO_GREVLEX4_WTS:
      nvars = b->nvars;
      MO_unpack4(nvars, psums + b->first_slot, tmpexp);
      p = tmpexp + nvars - 1;
      e = expon + b->first_exp;
      *e++ = *p--;
      for (j=nvars-1; j>=1; --j, --p)
	*e++ = *p - p[1];
      break;
    case MO_GREVLEX2:
    case MO_GREVLEX2_WTS:
      nvars = b->nvars;
      MO_unpack2(nvars, psums + b->first_slot, tmpexp);
      p = tmpexp + nvars - 1;
      e = expon + b->first_exp;
      *e++ = *p--;
      for (j=nvars-1; j>=1; --j, --p)
	*e++ = *p - p[1];
      break;
    case MO_LEX4:
      nvars = b->nvars;
      e = expon + b->first_exp;
      MO_unpack4(nvars, psums + b->first_slot, e);
      break;
    case MO_LEX2:
      nvars = b->nvars;
      e = expon + b->first_exp;
      MO_unpack2(nvars, psums + b->first_slot, e);
      break;
    case MO_WEIGHTS:
      break;
    case MO_POSITION_UP:
    case MO_POSITION_DOWN:
      /* should not occur, but do nothing in any case */
      break;
    case MO_NC_LEX:
      /* nothing to do here */
      break;
    }
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
*/
