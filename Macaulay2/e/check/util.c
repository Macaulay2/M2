#include "util.h"
#include "string.h"
/*******************************************************************/
/* Stuff needed from M2lib.c, scclib.c in order to link */
int system_interrupted = 0;

void outofmem(void)
{
  printf("Out of memory\n");
  exit(1);
}
/***********************************************************/


M2_Integer make_integer(long int a)
{
  M2_Integer result = (M2_Integer) getmem(sizeof(__mpz_struct));
  mpz_init_set_si(result,a);
  return result;
}

M2_arrayint arrayint(int len, ...)
{
  va_list ap;
  int i;
  M2_arrayint result;
  va_start(ap, len);
  result = makearrayint(len);
  for (i=0; i<len; i++)
    result->array[i] = va_arg(ap, int);
  va_end(ap);
  return result;
}

const Monomial *monom(int len, ...)
{
  va_list ap;
  int i;
  M2_arrayint result;
  va_start(ap, len);
  result = makearrayint(len);
  for (i=0; i<len; i++)
    result->array[i] = va_arg(ap, int);
  va_end(ap);
  return IM2_Monomial_make(result);
}

static MonomialOrdering_array make_mon_order_array(int n)
{
  MonomialOrdering_array z = (MonomialOrdering_array)getmem_atomic(sizeofarray(z,n));
  z->len = n;
  return z; /* Note that getmem_atomic returns zeroed memory */
}

MonomialOrdering * monorder(int len, ...)
{
  va_list ap;
  int i;
  MonomialOrdering_array result;
  va_start(ap, len);
  result = (MonomialOrdering_array) make_mon_order_array(len);
  for (i=0; i<len; i++)
    result->array[i] = va_arg(ap, MonomialOrdering *);
  va_end(ap);
  return IM2_MonomialOrdering_join(result);
}

M2_stringarray make_names(char *s, int n)
{
  int i, slen;
  char str[100];
  M2_stringarray a;
  a = (M2_stringarray) getmem (sizeofarray(a,n));
  a->len = n;
  strcpy(str,s);
  slen = strlen(s);
  for (i=0; i<n; i++)
    {
      sprintf(str+slen,"%d",i);
      a->array[i] = tostring(str);
    }
  return a;
}

Monoid *make_degree_monoid(int ndegs)
{
  if (ndegs == 0) 
    return IM2_Monoid_trivial();

  return IM2_Monoid_make(IM2_MonomialOrdering_laurent(ndegs),
			 make_names("t",ndegs),
			 IM2_Monoid_trivial(),
			 arrayint(0));
}

const Ring *make_poly_ring(int charac, int nvars)
{
  const Ring *K;
  M2_arrayint degs;
  int i;

  if (charac == 0)
    K = IM2_Ring_ZZ();
  else
    K = IM2_Ring_ZZp(charac);

  degs = makearrayint(nvars);
  for (i=0; i<nvars; i++)
    degs->array[i] = 1;

  return IM2_Ring_polyring(K,
			   IM2_Monoid_make(IM2_MonomialOrdering_grevlex(degs,1),
					   make_names("x",nvars),
					   make_degree_monoid(1),
					   degs));
}

const RingElement_array *make_ringelem_array(int len, ...)
{
  va_list ap;
  int i;
  RingElement_array *result;
  va_start(ap, len);
  result = (RingElement_array *) getmem (sizeofarray(result,len));
  result->len = len;

  for (i=0; i<len; i++)
    result->array[i] = va_arg(ap, const RingElement *);
  va_end(ap);
  return result;
}


const Vector *make_vector(const FreeModule *F, ...)
{
  va_list ap;
  int i,n;
  RingElement_array *result;
  va_start(ap, F);
  n = IM2_FreeModule_rank(F);
  result = (RingElement_array *) getmem (sizeofarray(result,n));
  result->len = n;

  for (i=0; i<n; i++)
    result->array[i] = va_arg(ap, const RingElement *);
  va_end(ap);
  return IM2_Vector_make(F,result);
}

const Vector_array *make_vector_array(int len, ...)
{
  va_list ap;
  int i;
  Vector_array *result;
  va_start(ap, len);
  result = (Vector_array *) getmem (sizeofarray(result,len));
  result->len = len;

  for (i=0; i<len; i++)
    result->array[i] = va_arg(ap, const Vector *);
  va_end(ap);
  return result;
}

void display_monomial(const Monomial *m)
{
  if (m == 0)
    printf("%s\n", tocharstar(IM2_last_error_message()));
  else
    printf("%s\n", tocharstar(IM2_Monomial_to_string(m)));
}

void display_monorder(const MonomialOrdering *mo)
{
  if (mo == 0)
    printf("%s\n", tocharstar(IM2_last_error_message()));
  else
    printf("%s\n", tocharstar(IM2_MonomialOrdering_to_string(mo)));
}

void display_monoid(const Monoid *M)
{
  if (M == 0)
    printf("%s\n", tocharstar(IM2_last_error_message()));
  else
    printf("%s\n", tocharstar(IM2_Monoid_to_string(M)));
}

void display_ring(const Ring *R)
{
  if (R == 0)
    printf("%s\n", tocharstar(IM2_last_error_message()));
  else
    printf("%s\n", tocharstar(IM2_Ring_to_string(R))); 
}

void display_relem(const RingElement *f)
{
  if (f == 0)
    printf("%s\n", tocharstar(IM2_last_error_message()));
  else
    printf("%s\n", tocharstar(IM2_RingElement_to_string(f)));
}

void display_freemodule(const FreeModule *F)
{
  if (F == 0)
    printf("%s\n", tocharstar(IM2_last_error_message()));
  else
    printf("%s\n", tocharstar(IM2_FreeModule_to_string(F)));
}

void display_vector(const Vector *f)
{
  if (f == 0)
    printf("%s\n", tocharstar(IM2_last_error_message()));
  else
    printf("%s\n", tocharstar(IM2_Vector_to_string(f)));
}

void display_matrix(const Matrix *f)
{
  if (f == 0)
    printf("%s\n", tocharstar(IM2_last_error_message()));
  else
    printf("%s\n", tocharstar(IM2_Matrix_to_string(f)));
}

void display_sparsemat(const MutableMatrix *f)
{
  if (f == 0)
    printf("%s\n", tocharstar(IM2_last_error_message()));
  else
    printf("%s\n", tocharstar(IM2_MutableMatrix_to_string(f)));
}

int is_eq(M2_string elem, char *answer)
{
  int i;
  int len = strlen(answer);
  if (len != elem->len)
    return 0;
  for (i=0; i<len; i++)
    if (elem->array[i] != answer[i])
      return 0;
  return 1;
}

int arrayint_is_eq(M2_arrayint a, M2_arrayint b)
{
  int i;
  if (a->len != b->len)
    return 0;
  for (i=0; i<a->len; i++)
    if (a->array[i] != b->array[i])
      return 0;
  return 1;
}
#if 0
int arrayint_is_eq(M2_arrayint a, int len, ...)
{
  va_list ap;
  int i;
  va_start(ap, len);
  if (a->len != len) return 0;
  for (i=0; i<len; i++)
    if (a->array[i] != va_arg(ap, int)) return 0;
  va_end(ap);
  return 1;
}
#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check"
// End:
*/

