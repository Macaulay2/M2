// Copyright 2011 Michael E. Stillman

#include "aring-zzp.hpp"
#include "ringmap.hpp"

namespace M2 {
int ARingZZp::findPrimitiveRoot(int P)
{
  int i, j, q;

  int prim_root;
  if (P == 2)
    prim_root = 1;
  else
    {
      j = 1;
      for (i = 2; (i < P && j < P - 1); i++)
        for (q = i, j = 1; (q != 1 && j < P); q = (q * i) % P, j++)
          ;
      prim_root = i - 1;
    }
  return prim_root;
}

void ARingZZp::initialize_tables()
{
  int i, n;

  prim_root = findPrimitiveRoot(p);

  log_table = newarray_atomic(int, p);
  exp_table = newarray_atomic(int, p);

  for (i = 0, n = 1; i < p - 1; i++, n = (n * prim_root) % p)
    {
      log_table[n] = i;  // i = log_(base _prim_root)(n)
      exp_table[i] = n;  // n = (_prim_root)^i
    }
  exp_table[p1] = 1;
  exp_table[0] = 0;
  log_table[1] = p1;
  log_table[0] = 0;

#if 0
    fprintf(stderr, "char %d primitive %d\n", p, prim_root);
    fprintf(stderr, "exp: ");
    for (int i=0; i<p; i++)
      fprintf(stderr, "%d ", exp_table[i]);
    fprintf(stderr, "\nlog: ");
    for (int i=0; i<p; i++)
      fprintf(stderr, "%d ", log_table[i]);
    fprintf(stderr, "\n");
#endif
}

ARingZZp::ARingZZp(size_t p0) : charac(p0), p(static_cast<int>(p0)), p1(p - 1)
{
  if (p == 2)
    minus_one = 1;
  else
    minus_one = (p - 1) / 2;

  initialize_tables();
}

ARingZZp::~ARingZZp()
{
  deletearray(log_table);
  deletearray(exp_table);
}

void ARingZZp::text_out(buffer &o) const { o << "AZZ/" << characteristic(); }
void ARingZZp::elem_text_out(buffer &o,
                             ElementType a,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{
  long n = coerceToLongInteger(a);
  if (n < 0)
    {
      o << '-';
      n = -n;
    }
  else if (p_plus)
    o << '+';
  if (p_one || n != 1) o << n;
}

void ARingZZp::eval(const RingMap *map,
                    const elem f,
                    int first_var,
                    ring_elem &result) const
{
  long a = coerceToLongInteger(f);
  result = map->get_ring()->from_long(a);
}
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
