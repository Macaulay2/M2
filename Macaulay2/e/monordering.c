#include "engine.h"
#include "error.h"
#include "monordering.h"
#include <stdio.h>

static struct mon_part_rec_ *mo_make(int type, int nvars, int *wts)
{
  mon_part result;
  result = (mon_part) getmem(sizeof(*result));
  result->type = type;
  result->nvars = nvars;
  if (wts != 0)
    {
      int i;
      result->wts = (int *) getmem_atomic(nvars * sizeof(int));
      for (i=0; i<nvars; i++)
	result->wts[i] = wts[i];
    }
  else
    result->wts = 0;
  return result;
}

static MonomialOrdering *make_mon_order(int n)
{
  static unsigned long next_hash = 2300230023U;
  MonomialOrdering *z = (MonomialOrdering *)getmem_atomic(sizeofarray(z,n));
  z->len = n;
  z->_hash = next_hash++;
  return z; /* Note that getmem_atomic returns zeroed memory */
}

int rawNumberOfVariables(const MonomialOrdering *mo)
{
  int i, sum = 0;
  for (i=0; i<mo->len; i++)
    if (mo->array[i]->type != MO_WEIGHTS)
      sum += mo->array[i]->nvars;
  return sum;
}

int rawNumberOfInvertibleVariables(const MonomialOrdering *mo)
{
  int i, sum = 0;
  for (i=0; i<mo->len; i++)
    if (mo->array[i]->type == MO_LAURENT)
      sum += mo->array[i]->nvars;
  return sum;
}

MonomialOrdering *rawLexMonomialOrdering(int nvars, int packing)
{
  MonomialOrdering *result;
  mon_part p;
  enum MonomialOrdering_type typ;

  if (packing == 2) typ = MO_LEX2;
  else if (packing == 4) typ = MO_LEX4;
  else typ = MO_LEX;

  p = mo_make(typ, nvars, NULL);
  result = make_mon_order(1);
  result->array[0] = p;
  return result;
}

MonomialOrdering *rawGRevLexMonomialOrdering(M2_arrayint degs, int packing)
{
  MonomialOrdering *result;
  mon_part p;
  enum MonomialOrdering_type typ;
  int *wts;
  int all_one = 1;
  unsigned int i;
  for (i=0; i<degs->len; i++)
    if (degs->array[i] <= 0)
      INTERNAL_ERROR("grevlex: expected all degrees to be positive");
    else if (degs->array[i] > 1)
      all_one = 0;

  if (all_one)
    {
      if (packing == 2) typ = MO_GREVLEX2;
      else if (packing == 4) typ = MO_GREVLEX4;
      else typ = MO_GREVLEX;
      wts = 0;
    }
  else
    {
      if (packing == 2) typ = MO_GREVLEX2_WTS;
      else if (packing == 4) typ = MO_GREVLEX4_WTS;
      else typ = MO_GREVLEX_WTS;
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
MonomialOrdering *rawNClexMonomialOrdering(int nvars)
{
  mon_part p = mo_make(MO_NC_LEX, nvars, 0);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}
MonomialOrdering *rawPositionMonomialOrdering(M2_bool up_or_down)
{
  mon_part p = mo_make((up_or_down ? MO_POSITION_UP : MO_POSITION_DOWN), 0, NULL);
  MonomialOrdering *result = make_mon_order(1);
  result->array[0] = p;
  return result;
}

static MonomialOrdering *M2_mo_offset(MonomialOrdering *mo, int offset)
{
  int i,j;
  MonomialOrdering *result = make_mon_order(mo->len);
  for (i=0; i<mo->len; i++)
    {
      mon_part p = mo->array[i];
      if (p->type != MO_WEIGHTS)
	result->array[i] = mo_make(p->type, p->nvars, p->wts);
      else
	{
	  mon_part q = mo_make(MO_WEIGHTS, offset + p->nvars, NULL);
	  q->wts = (int *) getmem_atomic(q->nvars * sizeof(int));
	  for (j=0; j<offset; j++)
	    q->wts[j] = 0;
	  for ( ; j<q->nvars; j++)
	    q->wts[j] = p->wts[j-offset];
	}
    }
  return result;
}


MonomialOrdering *rawJoinMonomialOrdering(MonomialOrdering_array M)
{
  MonomialOrdering *result;
  int i,j,sum,next;
  sum = 0;
  for (i=0; i<M->len; i++)
    sum += M->array[i]->len;

  result = make_mon_order(sum);
  next = 0;
  for (i=0; i<M->len; i++)
    {
      MonomialOrdering *mo = M->array[i];
      for (j=0; j<mo->len; j++)
	result->array[next++] = mo->array[j];
    }
  return result;
}

MonomialOrdering *rawProductMonomialOrdering(MonomialOrdering_array M)
{
  MonomialOrdering *result;
  int i,j,sum,next,offset;
  sum = 0;
  for (i=0; i<M->len; i++)
    sum += M->array[i]->len;

  result = make_mon_order(sum);
  next = 0;
  offset = 0;
  for (i=0; i<M->len; i++)
    {
      int nvars = rawNumberOfVariables(M->array[i]);
      MonomialOrdering *mo = M2_mo_offset(M->array[i], offset);
      for (j=0; j<mo->len; j++)
	result->array[next++] = mo->array[j];
      offset += nvars;
    }
  return result;
}


#if 0

extern MonomialOrdering IM2_MonomialOrdering_read_xml(FILE *f)
{
  MonomialOrdering result = 0;
  MonomialOrdering mo;
  int *wts;
  int nv;
  char str[1000];
  
  for (;;)
    {
      fscanf(f, "%s", str);
      if (strcmp("lex",str) == 0)
	{
	  fscanf(f, "%d", &nv);
	  mo = rawLexMonomialOrdering(nv);
	}
      else if (strcmp("lex2",str) == 0)
	{
	  fscanf(f, "%d", &nv);
	  mo = rawLexMonomialOrdering2(nv);
	}
      else if (strcmp("lex4",str) == 0)
	{
	  fscanf(f, "%d", &nv);
	  mo = rawLexMonomialOrdering4(nv);
	}
      else if (strcmp("grevlex",str) == 0)
	{
	  wts = intarray_read(f, &nv);
	  mo = rawGRevLexMonomialOrdering(nv,wts);
	  FREE(wts);
	}
      else if (strcmp("grevlex2",str) == 0)
	{
	  wts = intarray_read(f, &nv);
	  mo = rawGRevLexMonomialOrdering2(nv,wts);
	  FREE(wts);
	}
      else if (strcmp("grevlex4",str) == 0)
	{
	  wts = intarray_read(f, &nv);
	  mo = rawGRevLexMonomialOrdering4(nv,wts);
	  FREE(wts);
	}
      else if (strcmp("revlex",str) == 0)
	{
	  fscanf(f, "%d", &nv);
	  mo = rawRevLexMonomialOrdering(nv);
	}
      else if (strcmp("weights",str) == 0)
	{
	  wts = intarray_read(f, &nv);
	  mo = rawWeightsMonomialOrdering(nv,wts);
	  FREE(wts);
	}
      else if (strcmp("laurent",str) == 0)
	{
	  fscanf(f, "%d", &nv);
	  mo = rawGroupLexMonomialOrdering(nv);
	}
      else if (strcmp("NClex",str) == 0)
	{
	  fscanf(f, "%d", &nv);
	  mo = rawNClexMonomialOrdering(nv);
	}
      else if (strcmp("component",str) == 0)
	{
	  mo = IM2_MonomialOrdering_component();
	}
      else if (strcmp("</order>", str) == 0)
	{
	  break;
	}
      else
	{
	  fprintf(stderr, "unknown block type \"%s\" for monomial order", str);
	  exit(1);
	}

      mo_append(&result,mo);
    }

  return result;
}
#endif

M2_string intarray_to_string(int len, int *p)
{
  int i;
  char s[200];
  M2_string result = tostring("{");
  for (i=0; i<len; i++)
    {
      if (i > 0) result = strings_join(result, tostring(","));
      sprintf(s, "%d", p[i]);
      result = strings_join(result, tostring(s));
    }
  result = strings_join(result,tostring("}"));
  return result;
}

M2_string ones_to_string(int len)
{
  int i;
  char s[200];
  M2_string one;
  M2_string result = tostring("{");
  sprintf(s, "1");
  one = tostring(s);
  for (i=0; i<len; i++)
    {
      if (i > 0) result = strings_join(result, tostring(","));
      result = strings_join(result, one);
    }
  result = strings_join(result,tostring("}"));
  return result;
}

unsigned long IM2_MonomialOrdering_hash(MonomialOrdering *mo)
{
  return mo->_hash;
}

M2_string IM2_MonomialOrdering_to_string(const MonomialOrdering *mo)
{
  int i;
  char s[200];
  int p_ones = 0;
  M2_string result = tostring("MonomialOrdering => {");
  for (i=0; i<mo->len; i++)
    {
      mon_part p = mo->array[i];
      p_ones = 0;
      if (i == 0)
	result = strings_join(result, tostring("\n    "));
      else
	result = strings_join(result, tostring(",\n    "));
      switch (p->type) {
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
      result = strings_join(result, tostring(s));
      if (p->wts != NULL)
	result = strings_join(result, intarray_to_string(p->nvars, p->wts));
      else if (p_ones)
	result = strings_join(result, ones_to_string(p->nvars));
    }
  result = strings_join(result, tostring("\n    }"));
  return result;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
*/
