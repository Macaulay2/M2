// Copyright 2009 Michael E. Stillman

#include "imonorder.hpp"

#include "engine-includes.hpp"

#ifdef HAVE_ALLOCA_H
#  include <alloca.h>
#else
#  include <malloc.h>
#endif

#include "overflow.hpp"

/* TODO:
   -- negative exponent versions need to be included (at least for MO_LEX)
   -- non-commutative blocks should be added in
*/

static void mo_block_revlex(struct mo_block *b, int nvars)
{
  b->typ = MO_REVLEX;
  b->nvars = nvars;
  b->nslots = nvars;
  b->first_exp = 0;  /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_grevlex(struct mo_block *b, int nvars)
{
  b->typ = MO_GREVLEX;
  b->nvars = nvars;
  b->nslots = nvars;
  b->first_exp = 0;  /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_grevlex2(struct mo_block *b, int nvars)
{
  b->typ = MO_GREVLEX2;
  b->nvars = nvars;
  b->nslots = (nvars + 1) / 2; /* 2 per word */
  b->first_exp = 0;            /* will be set later */
  b->first_slot = 0;           /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_grevlex4(struct mo_block *b, int nvars)
{
  b->typ = MO_GREVLEX4;
  b->nvars = nvars;
  b->nslots = (nvars + 3) / 4; /* 4 per word */
  b->first_exp = 0;            /* will be set later */
  b->first_slot = 0;           /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_grevlex_wts(struct mo_block *b, int nvars)
{
  b->typ = MO_GREVLEX_WTS;
  b->nvars = nvars;
  b->nslots = nvars;
  b->first_exp = 0;  /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = nvars;
  b->weights = 0; /* will be set later */
}

static void mo_block_grevlex2_wts(struct mo_block *b, int nvars)
{
  b->typ = MO_GREVLEX2_WTS;
  b->nvars = nvars;
  b->nslots = (nvars + 1) / 2; /* 2 per word */
  b->first_exp = 0;            /* will be set later */
  b->first_slot = 0;           /* will be set later */
  b->nweights = nvars;
  b->weights = 0; /* will be set later */
}

static void mo_block_grevlex4_wts(struct mo_block *b, int nvars)
{
  b->typ = MO_GREVLEX4_WTS;
  b->nvars = nvars;
  b->nslots = (nvars + 3) / 4; /* 4 per word */
  b->first_exp = 0;            /* will be set later */
  b->first_slot = 0;           /* will be set later */
  b->nweights = nvars;
  b->weights = 0; /* will be set later */
}

static void mo_block_lex(struct mo_block *b, int nvars)
{
  b->typ = MO_LEX;
  b->nvars = nvars;
  b->nslots = nvars;
  b->first_exp = 0;  /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_lex2(struct mo_block *b, int nvars)
{
  b->typ = MO_LEX2;
  b->nvars = nvars;
  b->nslots = (nvars + 1) / 2; /* 2 per word */
  b->first_exp = 0;            /* will be set later */
  b->first_slot = 0;           /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_lex4(struct mo_block *b, int nvars)
{
  b->typ = MO_LEX4;
  b->nvars = nvars;
  b->nslots = (nvars + 3) / 4; /* 4 per word */
  b->first_exp = 0;            /* will be set later */
  b->first_slot = 0;           /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_group_lex(struct mo_block *b, int nvars)
{
  b->typ = MO_LAURENT;
  b->nvars = nvars;
  b->nslots = nvars;
  b->first_exp = 0;  /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_group_revlex(struct mo_block *b, int nvars)
{
  b->typ = MO_LAURENT_REVLEX;
  b->nvars = nvars;
  b->nslots = nvars;
  b->first_exp = 0;  /* will be set later */
  b->first_slot = 0; /* will be set later */
  b->nweights = 0;
  b->weights = 0;
}

static void mo_block_wt_function(struct mo_block *b, int nvars, deg_t *wts)
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
  int i, j, nv, this_block;
  deg_t *wts = NULL;
  /* Determine the number of variables, the number of blocks, and the location
     of the component */
  int nblocks = 0;
  int nvars = 0;
  int hascomponent = 0;
  for (i = 0; i < mo->len; i++)
    {
      struct mon_part_rec_ *t = mo->array[i];
      nblocks++;
      if (t->type == MO_POSITION_DOWN || t->type == MO_POSITION_UP)
        hascomponent++;
      else if (t->type == MO_NC_LEX)
        {
          // Currently, do nothing.
        }
      if (t->type != MO_WEIGHTS) nvars += t->nvars;
    }
  nblocks -= hascomponent;

  /* Now create the blocks, and fill them in. Also fill in the deg vector */
  result = getmemstructtype(MonomialOrder *);
  result->nvars = nvars;
  result->nslots = 0;
  result->nblocks = nblocks;
  result->blocks =
      (struct mo_block *)getmem(nblocks * sizeof(result->blocks[0]));
  result->degs = (deg_t *)getmem_atomic(nvars * sizeof(result->degs[0]));
  if (hascomponent == 0) result->nblocks_before_component = nblocks;

  this_block = 0;
  nvars = 0;
  for (i = 0; i < mo->len; i++)
    {
      struct mon_part_rec_ *t = mo->array[i];
      if (t->type != MO_WEIGHTS)
        {
          if (t->wts == 0)
            for (j = 0; j < t->nvars; j++) result->degs[nvars++] = 1;
          else
            for (j = 0; j < t->nvars; j++) result->degs[nvars++] = t->wts[j];
        }
      else
        {
          wts = (deg_t *)getmem_atomic(t->nvars * sizeof(wts[0]));
          for (j = 0; j < t->nvars; j++) wts[j] = t->wts[j];
        }
      switch (t->type)
        {
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
            // if extra weight values are given (more than "nvars", ignore the
            // rest.
            mo_block_wt_function(
                result->blocks + this_block++,
                (t->nvars <= result->nvars ? t->nvars : result->nvars),
                wts);
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
  /* Now fix the first_exp, first_slot values, and also result->{nslots,nvars};
   */
  nv = 0;
  result->nslots = 0;
  result->nslots_before_component = 0;
  for (i = 0; i < nblocks; i++)
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
          for (j = 0; j < result->blocks[i].nvars; j++)
            safe::div_by(result->blocks[i].weights[j], result->degs[j]);
          ;
        }
      else if (typ == MO_GREVLEX_WTS || typ == MO_GREVLEX2_WTS ||
               typ == MO_GREVLEX4_WTS)
        {
          result->blocks[i].weights =
              result->degs + result->blocks[i].first_exp;
        }

      if (i == result->nblocks_before_component - 1)
        {
          result->nslots_before_component = result->nslots;
        }
    }

  /* Set is_laurent */
  result->is_laurent = (int *)getmem_atomic(result->nvars * sizeof(int));
  for (i = 0; i < result->nvars; i++) result->is_laurent[i] = 0;

  for (i = 0; i < result->nblocks; i++)
    if (result->blocks[i].typ == MO_LAURENT ||
        result->blocks[i].typ == MO_LAURENT_REVLEX)
      {
        for (j = 0; j < result->blocks[i].nvars; j++)
          result->is_laurent[result->blocks[i].first_exp + j] = 1;
      }

  return result;
}

extern void monomialOrderFree(MonomialOrder *mo) {}
static void MO_pack4(int nvars, const int *expon, int *slots)
{
  int32_t i;
  if (nvars == 0) return;
  while (1)
    {
      i = safe::fits_7(*expon++) << 24;
      if (--nvars == 0) break;
      i |= safe::fits_7(*expon++) << 16;
      if (--nvars == 0) break;
      i |= safe::fits_7(*expon++) << 8;
      if (--nvars == 0) break;
      i |= safe::fits_7(*expon++);
      if (--nvars == 0) break;
      *slots++ = i;
    }
  *slots++ = i;
}

static void MO_pack2(int nvars, const int *expon, int *slots)
{
  int32_t i;
  if (nvars == 0) return;
  while (1)
    {
      i = safe::fits_15(*expon++) << 16;
      if (--nvars == 0) break;
      i |= safe::fits_15(*expon++);
      if (--nvars == 0) break;
      *slots++ = i;
    }
  *slots++ = i;
}

static void MO_unpack4(int nvars, const int *slots, int *expon)
{
  int32_t i;
  if (nvars == 0) return;
  while (1)
    {
      i = *slots++;
      *expon++ = (i >> 24);
      if (--nvars == 0) break;
      *expon++ = (i >> 16) & 0x7f;
      if (--nvars == 0) break;
      *expon++ = (i >> 8) & 0x7f;
      if (--nvars == 0) break;
      *expon++ = i & 0x7f;
      if (--nvars == 0) break;
    }
}

static void MO_unpack2(int nvars, const int *slots, int *expon)
{
  int32_t i;
  if (nvars == 0) return;
  while (1)
    {
      i = *slots++;
      *expon++ = i >> 16;
      if (--nvars == 0) break;
      *expon++ = i & 0x7fff;
      if (--nvars == 0) break;
    }
}

void monomialOrderEncodeFromActualExponents(const MonomialOrder *mo,
                                            const_exponents expon,
                                            monomial result_psums)
/* Given 'expon', compute the encoded partial sums value */
{
  if (mo == 0) return;
  int *tmpexp = static_cast<int *>(alloca((mo->nvars + 1) * sizeof(int)));
  int i, j, nvars, s;
  int *p1;
  deg_t *degs;
  struct mo_block *b = mo->blocks;
  int nblocks = mo->nblocks;
  const int *e = expon;
  int *p = result_psums;
  for (i = nblocks; i > 0; --i, b++) switch (b->typ)
      {
        case MO_LEX:
        case MO_LAURENT:
          nvars = b->nvars;
          for (j = 0; j < nvars; j++) *p++ = *e++;
          break;
        case MO_REVLEX:
        case MO_LAURENT_REVLEX:
          nvars = b->nvars;
          for (j = 0; j < nvars; j++) *p++ = safe::minus(*e++);
          break;
        case MO_GREVLEX:
          nvars = b->nvars;
          p += b->nslots;
          p1 = p;
          *--p1 = *e++;
          for (j = 1; j < nvars; j++)
            {
              --p1;
              *p1 = safe::add(*e++, p1[1]);
            }
          break;
        case MO_GREVLEX_WTS:
          nvars = b->nvars;
          degs = mo->degs + b->first_exp;
          p += b->nslots;
          p1 = p;
          *--p1 = safe::mult(*e++, *degs++);
          for (j = 1; j < nvars; j++)
            {
              --p1;
              int tmp = safe::mult(*e++, *degs++);
              *p1 = safe::add(tmp, p1[1]);
            }
          break;
        case MO_GREVLEX4:
          nvars = b->nvars;
          p1 = tmpexp + b->nvars;
          *--p1 = *e++;
          for (j = 1; j < nvars; j++)
            {
              --p1;
              *p1 = safe::add(*e++, p1[1]);
            }
          MO_pack4(nvars, p1, p);
          p += b->nslots;
          break;
        case MO_GREVLEX4_WTS:
          nvars = b->nvars;
          degs = mo->degs + b->first_exp;
          p1 = tmpexp + b->nvars;
          *--p1 = safe::mult(*e++, *degs++);
          for (j = 1; j < nvars; j++)
            {
              --p1;
              int tmp = safe::mult(*e++, *degs++);
              *p1 = safe::add(tmp, p1[1]);
            }
          MO_pack4(nvars, p1, p);
          p += b->nslots;
          break;
        case MO_GREVLEX2:
          nvars = b->nvars;
          p1 = tmpexp + b->nvars;
          *--p1 = *e++;
          for (j = 1; j < nvars; j++)
            {
              --p1;
              *p1 = safe::add(*e++, p1[1]);
            }
          MO_pack2(nvars, p1, p);
          p += b->nslots;
          break;
        case MO_GREVLEX2_WTS:
          nvars = b->nvars;
          degs = mo->degs + b->first_exp;
          p1 = tmpexp + b->nvars;
          *--p1 = safe::mult(*e++, *degs++);
          for (j = 1; j < nvars; j++)
            {
              --p1;
              int tmp = safe::mult(*e++, *degs++);
              *p1 = safe::add(tmp, p1[1]);
            }
          MO_pack2(nvars, p1, p);
          p += b->nslots;
          break;
        case MO_LEX4:
          nvars = b->nvars;
          MO_pack4(nvars, e, p);
          p += b->nslots;
          e += nvars;
          break;
        case MO_LEX2:
          nvars = b->nvars;
          MO_pack2(nvars, e, p);
          p += b->nslots;
          e += nvars;
          break;
        case MO_WEIGHTS:
          if (b->nweights == 0)
            {
              s = 0;
            }
          else
            {
              s = safe::mult(b->weights[0], expon[0]);
              for (j = 1; j < b->nweights; j++)
                s = safe::add(s, safe::mult(b->weights[j], expon[j]));
            }
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

void monomialOrderDecodeToActualExponents(const MonomialOrder *mo,
                                          const_monomial psums,
                                          exponents expon)
{
  if (mo == 0) return;
  int *tmpexp = static_cast<int *>(alloca((mo->nvars + 1) * sizeof(int)));
  int i, j, nvars;
  deg_t *degs = mo->degs;
  deg_t *d;
  struct mo_block *b = mo->blocks;
  int nblocks = mo->nblocks;
  int *e = expon;
  const int *p = psums;
  for (i = nblocks; i > 0; --i, b++) switch (b->typ)
      {
        case MO_LEX:
        case MO_LAURENT:
          nvars = b->nvars;
          p = psums + b->first_slot;
          e = expon + b->first_exp;
          for (j = 0; j < nvars; j++) *e++ = *p++;
          break;
        case MO_REVLEX:
        case MO_LAURENT_REVLEX:
          nvars = b->nvars;
          p = psums + b->first_slot;
          e = expon + b->first_exp;
          for (j = 0; j < nvars; j++) *e++ = safe::minus(*p++);
          break;
        case MO_GREVLEX:
          nvars = b->nvars;
          p = psums + b->first_slot + nvars - 1;
          e = expon + b->first_exp;
          *e++ = *p--;
          for (j = nvars - 1; j >= 1; --j, --p) *e++ = safe::sub(*p, p[1]);
          break;
        case MO_GREVLEX_WTS:
          nvars = b->nvars;
          d = degs + b->first_exp;
          p = psums + b->first_slot + nvars - 1;
          e = expon + b->first_exp;
          *e++ = *p-- / *d++;
          for (j = nvars - 1; j >= 1; --j, --p)
            *e++ = safe::sub(*p, p[1]) / *d++;
          break;
        case MO_GREVLEX4:
          nvars = b->nvars;
          MO_unpack4(nvars, psums + b->first_slot, tmpexp);
          p = tmpexp + nvars - 1;
          e = expon + b->first_exp;
          *e++ = *p--;
          for (j = nvars - 1; j >= 1; --j, --p) *e++ = safe::sub(*p, p[1]);
          break;
        case MO_GREVLEX4_WTS:
          nvars = b->nvars;
          d = degs + b->first_exp;
          MO_unpack4(nvars, psums + b->first_slot, tmpexp);
          p = tmpexp + nvars - 1;
          e = expon + b->first_exp;
          *e++ = *p-- / *d++;
          for (j = nvars - 1; j >= 1; --j, --p)
            *e++ = safe::sub(*p, p[1]) / *d++;
          break;
        case MO_GREVLEX2:
          nvars = b->nvars;
          MO_unpack2(nvars, psums + b->first_slot, tmpexp);
          p = tmpexp + nvars - 1;
          e = expon + b->first_exp;
          *e++ = *p--;
          for (j = nvars - 1; j >= 1; --j, --p) *e++ = safe::sub(*p, p[1]);
          break;
        case MO_GREVLEX2_WTS:
          nvars = b->nvars;
          d = degs + b->first_exp;
          MO_unpack2(nvars, psums + b->first_slot, tmpexp);
          p = tmpexp + nvars - 1;
          e = expon + b->first_exp;
          *e++ = *p-- / *d++;
          for (j = nvars - 1; j >= 1; --j, --p)
            *e++ = safe::sub(*p, p[1]) / *d++;
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
// indent-tabs-mode: nil
// End:
*/
