#include "interface/monomial-ordering.h"

// TODO: remove this when to_string methods are moved together
#include <stdio.h>

#include "error.h"

#if defined(__cplusplus)
extern "C" {
#endif

static struct mon_part_rec_ *mo_make(enum MonomialOrdering_type type,
                                     int nvars,
                                     int *wts)
{
  mon_part result;
  result = getmemstructtype(mon_part);
  result->type = type;
  result->nvars = nvars;
  if (wts != 0)
    {
      int i;
      result->wts = getmematomicvectortype(int, nvars);
      for (i = 0; i < nvars; i++) result->wts[i] = wts[i];
    }
  else
    result->wts = 0;
  return result;
}

static MonomialOrdering *make_mon_order(int n)
{
  static unsigned int next_hash = 23023421;
  MonomialOrdering *z = getmemarraytype(MonomialOrdering *, n);
  z->len = n;
  z->_hash = next_hash++;
  int i;
  for (i = 0; i < n; i++) z->array[i] = NULL;
  return z;
}

int moIsLex(const MonomialOrdering *mo)
{
  // The monomial order is lex if what?
  // one lex block, no grevlex blocks, no weightvector blocks.
  // only: lex block and position blocks are allowed.
  int nlex = 0;
  int i;
  for (i = 0; i < mo->len; i++)
    {
      enum MonomialOrdering_type typ = mo->array[i]->type;
      switch (typ)
        {
          case MO_LEX:
          case MO_LEX2:
          case MO_LEX4:
            nlex++;
            break;
          case MO_POSITION_UP:
          case MO_POSITION_DOWN:
            break;
          default:
            return 0;
        }
    }
  return (nlex == 1);
}

int moIsGRevLex(const MonomialOrdering *mo)
{
  // The monomial order is lex if what?
  // one lex block, no grevlex blocks, no weightvector blocks.
  // only: lex block and position blocks are allowed.
  int ngrevlex = 0;
  int i;
  for (i = 0; i < mo->len; i++)
    {
      enum MonomialOrdering_type typ = mo->array[i]->type;
      switch (typ)
        {
          case MO_GREVLEX:
          case MO_GREVLEX2:
          case MO_GREVLEX4:
          case MO_GREVLEX_WTS:
          case MO_GREVLEX2_WTS:
          case MO_GREVLEX4_WTS:
            ngrevlex++;
            break;
          case MO_POSITION_UP:
          case MO_POSITION_DOWN:
            break;
          default:
            return 0;
        }
    }
  return (ngrevlex == 1);
}

int rawNumberOfVariables(const MonomialOrdering *mo)
{
  int i, sum = 0;
  for (i = 0; i < mo->len; i++)
    if (mo->array[i]->type != MO_WEIGHTS) sum += mo->array[i]->nvars;
  return sum;
}

M2_arrayint moGetWeightValues(const MonomialOrdering *mo)
{
  int nvars = rawNumberOfVariables(mo);
  // grab the first weight vector
  if (mo->len == 0) return 0;
  if (mo->array[0]->type == MO_WEIGHTS)
    {
      int i;
      M2_arrayint result = M2_makearrayint(nvars);
      int *wts = mo->array[0]->wts;
      for (i = 0; i < mo->array[0]->nvars; i++) result->array[i] = wts[i];
      for (; i < nvars; i++) result->array[i] = 0;
      return result;
    }
  return 0;
}

int rawNumberOfInvertibleVariables(const MonomialOrdering *mo)
{
  int i, sum = 0;
  for (i = 0; i < mo->len; i++)
    if (mo->array[i]->type == MO_LAURENT ||
        mo->array[i]->type == MO_LAURENT_REVLEX)
      sum += mo->array[i]->nvars;
  return sum;
}

M2_arrayint rawNonTermOrderVariables(const MonomialOrdering *mo)
// returns a list of the indices of those variables which are less than 1 in
// the given monomial order.
{
  int i, j, sum, nextvar;
  int nvars = rawNumberOfVariables(mo);
  int *gt = getmematomicvectortype(int, nvars);
  for (i = 0; i < nvars; i++)
    gt[i] =
        0;  // 0 means undecided, -1 means non term order, 1 means term order
  // Now we loop through the parts of the monomial order
  nextvar = 0;
  for (i = 0; i < mo->len; i++)
    {
      mon_part p = mo->array[i];
      switch (p->type)
        {
          case MO_LEX:
          case MO_LEX2:
          case MO_LEX4:
          case MO_GREVLEX:
          case MO_GREVLEX2:
          case MO_GREVLEX4:
          case MO_GREVLEX_WTS:
          case MO_GREVLEX2_WTS:
          case MO_GREVLEX4_WTS:
          case MO_LAURENT:
          case MO_NC_LEX:
            for (j = 0; j < p->nvars; j++, nextvar++)
              if (gt[nextvar] == 0) gt[nextvar] = 1;
            break;
          case MO_LAURENT_REVLEX:
          case MO_REVLEX:
            for (j = 0; j < p->nvars; j++, nextvar++)
              if (gt[nextvar] == 0) gt[nextvar] = -1;
            break;
          case MO_WEIGHTS:
            for (j = nextvar; j < p->nvars; j++)
              if (gt[j] == 0)
                {
                  if (p->wts[j] > 0)
                    gt[j] = 1;
                  else if (p->wts[j] < 0)
                    gt[j] = -1;
                }
            break;
          case MO_POSITION_UP:
          case MO_POSITION_DOWN:
            break;
        }
    }
  // At this point every variables' gt should be 1 or -1.
  sum = 0;
  for (i = 0; i < nvars; i++)
    {
      if (gt[i] == 0) INTERNAL_ERROR("gt[i] should not be 0");
      if (gt[i] < 0) sum++;
    }

  // Make an array of this length.
  M2_arrayint result = M2_makearrayint(sum);
  nextvar = 0;
  for (i = 0; i < nvars; i++)
    if (gt[i] < 0) result->array[nextvar++] = i;
  return result;
}

MonomialOrdering *rawLexMonomialOrdering(int nvars, int packing)
{
  MonomialOrdering *result;
  mon_part p;
  enum MonomialOrdering_type typ;

  if (packing == 2)
    typ = MO_LEX2;
  else if (packing == 4)
    typ = MO_LEX4;
  else
    typ = MO_LEX;

  p = mo_make(typ, nvars, NULL);
  result = make_mon_order(1);
  result->array[0] = p;
  return result;
}

MonomialOrdering /* or null */ *rawGRevLexMonomialOrdering(M2_arrayint degs,
                                                           int packing)
{
  MonomialOrdering *result;
  mon_part p;
  enum MonomialOrdering_type typ;
  int *wts;
  int all_one = 1;
  unsigned int i;
  for (i = 0; i < degs->len; i++)
    if (degs->array[i] <= 0)
      {
        ERROR("grevlex: expected all degrees to be positive");
        return NULL;
      }
    else if (degs->array[i] > 1)
      all_one = 0;

  if (all_one)
    {
      if (packing == 2)
        typ = MO_GREVLEX2;
      else if (packing == 4)
        typ = MO_GREVLEX4;
      else
        typ = MO_GREVLEX;
      wts = 0;
    }
  else
    {
      if (packing == 2)
        typ = MO_GREVLEX2_WTS;
      else if (packing == 4)
        typ = MO_GREVLEX4_WTS;
      else
        typ = MO_GREVLEX_WTS;
      wts = degs->array;
    }

  p = mo_make(typ, degs->len, wts);
  result = make_mon_order(1);
  result->array[0] = p;
  return result;
}

MonomialOrdering *rawRevLexMonomialOrdering(int nvars)
{
  mon_part p = mo_make(MO_REVLEX, nvars, NULL);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}

MonomialOrdering *rawWeightsMonomialOrdering(M2_arrayint wts)
{
  mon_part p = mo_make(MO_WEIGHTS, wts->len, wts->array);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}
MonomialOrdering *rawGroupLexMonomialOrdering(int nvars)
{
  mon_part p = mo_make(MO_LAURENT, nvars, 0);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}
MonomialOrdering *rawGroupRevLexMonomialOrdering(int nvars)
{
  mon_part p = mo_make(MO_LAURENT_REVLEX, nvars, 0);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}
MonomialOrdering *rawNClexMonomialOrdering(int nvars)
{
  mon_part p = mo_make(MO_NC_LEX, nvars, 0);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}
MonomialOrdering *rawPositionMonomialOrdering(M2_bool up_or_down)
{
  mon_part p =
      mo_make((up_or_down ? MO_POSITION_UP : MO_POSITION_DOWN), 0, NULL);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}

static MonomialOrdering *M2_mo_offset(const MonomialOrdering *mo, int offset)
{
  int i, j;
  MonomialOrdering *result = make_mon_order(mo->len);
  for (i = 0; i < mo->len; i++)
    {
      mon_part p = mo->array[i];
      if (p->type != MO_WEIGHTS)
        result->array[i] = mo_make(p->type, p->nvars, p->wts);
      else
        {
          mon_part q = mo_make(MO_WEIGHTS, offset + p->nvars, NULL);
          q->wts = getmemvectortype(int, q->nvars);
          for (j = 0; j < offset; j++) q->wts[j] = 0;
          for (; j < q->nvars; j++) q->wts[j] = p->wts[j - offset];
        }
    }
  return result;
}

static int is_good(mon_part p)
{
  switch (p->type)
    {
      case MO_LEX:
      case MO_LEX2:
      case MO_LEX4:
      case MO_GREVLEX:
      case MO_GREVLEX2:
      case MO_GREVLEX4:
      case MO_GREVLEX_WTS:
      case MO_GREVLEX2_WTS:
      case MO_GREVLEX4_WTS:
      case MO_LAURENT:
      case MO_NC_LEX:
      case MO_LAURENT_REVLEX:
      case MO_REVLEX:
      case MO_WEIGHTS:
        return (p->nvars > 0);
      case MO_POSITION_UP:
      case MO_POSITION_DOWN:
        return 1;
    }
  return 0;
}

MonomialOrdering *rawJoinMonomialOrdering(engine_RawMonomialOrderingArray M)
{
  MonomialOrdering *result;
  const MonomialOrdering *mo;
  int i, j, sum, next;
  int nvars_so_far = 0;

  //  sum = 0;
  //  for (i=0; i<M->len; i++)
  //    sum += M->array[i]->len;

  sum = 0;
  for (i = 0; i < M->len; i++)
    {
      mo = M->array[i];
      for (j = 0; j < mo->len; j++)
        if (is_good(mo->array[j])) sum++;
    }

  result = make_mon_order(sum);
  next = 0;
  for (i = 0; i < M->len; i++)
    {
      mo = M->array[i];
      for (j = 0; j < mo->len; j++)
        {
          mon_part p = mo->array[j];
          if (!is_good(p)) continue;
          if (p->type != MO_WEIGHTS)
            nvars_so_far += p->nvars;
          else
            {
              /* Shift the weights over by nvars_so_far */
              mon_part q = mo_make(MO_WEIGHTS, nvars_so_far + p->nvars, NULL);
              q->wts = getmemvectortype(int, q->nvars);
              for (j = 0; j < nvars_so_far; j++) q->wts[j] = 0;
              for (; j < q->nvars; j++) q->wts[j] = p->wts[j - nvars_so_far];
              p = q;
            }
          result->array[next++] = p;
        }
    }
  return result;
}

MonomialOrdering *rawProductMonomialOrdering(engine_RawMonomialOrderingArray M)
{
  MonomialOrdering *result;
  int i, j, sum, next, offset;
  sum = 0;
  for (i = 0; i < M->len; i++) sum += M->array[i]->len;

  result = make_mon_order(sum);
  next = 0;
  offset = 0;
  for (i = 0; i < M->len; i++)
    {
      int nvars = rawNumberOfVariables(M->array[i]);
      MonomialOrdering *mo = M2_mo_offset(M->array[i], offset);
      for (j = 0; j < mo->len; j++) result->array[next++] = mo->array[j];
      offset += nvars;
    }
  return result;
}

M2_string intarray_to_string(int len, int *p)
{
  int i;
  char s[200];
  M2_string result = M2_tostring("{");
  for (i = 0; i < len; i++)
    {
      if (i > 0) result = M2_join(result, M2_tostring(","));
      sprintf(s, "%d", p[i]);
      result = M2_join(result, M2_tostring(s));
    }
  result = M2_join(result, M2_tostring("}"));
  return result;
}

M2_string ones_to_string(int len)
{
  int i;
  char s[200];
  M2_string one;
  M2_string result = M2_tostring("{");
  sprintf(s, "1");
  one = M2_tostring(s);
  for (i = 0; i < len; i++)
    {
      if (i > 0) result = M2_join(result, M2_tostring(","));
      result = M2_join(result, one);
    }
  result = M2_join(result, M2_tostring("}"));
  return result;
}

unsigned int rawMonomialOrderingHash(const MonomialOrdering *mo)
{
  return mo->_hash;
}

M2_string IM2_MonomialOrdering_to_string(const MonomialOrdering *mo)
{
  int i;
  char s[200];
  int p_ones = 0;
  M2_string result = M2_tostring("MonomialOrder => {");
  for (i = 0; i < mo->len; i++)
    {
      mon_part p = mo->array[i];
      p_ones = 0;
      if (i == 0)
        result = M2_join(result, M2_tostring("\n    "));
      else
        result = M2_join(result, M2_tostring(",\n    "));
      switch (p->type)
        {
          case MO_LEX:
            sprintf(s, "Lex => %d", p->nvars);
            break;
          case MO_LEX2:
            sprintf(s, "LexSmall => %d", p->nvars);
            break;
          case MO_LEX4:
            sprintf(s, "LexTiny => %d", p->nvars);
            break;
          case MO_GREVLEX:
            sprintf(s, "GRevLex => ");
            p_ones = 1;
            break;
          case MO_GREVLEX2:
            sprintf(s, "GRevLexSmall => ");
            p_ones = 1;
            break;
          case MO_GREVLEX4:
            sprintf(s, "GRevLexTiny => ");
            p_ones = 1;
            break;
          case MO_GREVLEX_WTS:
            sprintf(s, "GRevLex => ");
            break;
          case MO_GREVLEX2_WTS:
            sprintf(s, "GRevLexSmall => ");
            break;
          case MO_GREVLEX4_WTS:
            sprintf(s, "GRevLexTiny => ");
            break;
          case MO_REVLEX:
            sprintf(s, "RevLex => %d", p->nvars);
            break;
          case MO_WEIGHTS:
            sprintf(s, "Weights => ");
            break;
          case MO_LAURENT:
            sprintf(s, "GroupLex => %d", p->nvars);
            break;
          case MO_LAURENT_REVLEX:
            sprintf(s, "GroupRevLex => %d", p->nvars);
            break;
          case MO_NC_LEX:
            sprintf(s, "NCLex => %d", p->nvars);
            break;
          case MO_POSITION_UP:
            sprintf(s, "Position => Up");
            break;
          case MO_POSITION_DOWN:
            sprintf(s, "Position => Down");
            break;
          default:
            sprintf(s, "UNKNOWN");
            break;
        }
      result = M2_join(result, M2_tostring(s));
      if (p->wts != NULL)
        result = M2_join(result, intarray_to_string(p->nvars, p->wts));
      else if (p_ones)
        result = M2_join(result, ones_to_string(p->nvars));
    }
  result = M2_join(result, M2_tostring("\n    }"));
  return result;
}

#if defined(__cplusplus)
}
#endif
