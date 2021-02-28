#include "dpoly.hpp"

#include <cassert>
#include <cctype>
#include <cstdlib>
#include <sstream>
#include <vector>

#include "interface/random.h"
#include "ZZ.hpp"

#define DEBUGGCDno

long gcd_extended(long a, long b, long &u, long &v)
{
  long g;
  long q;
  long u1, v1, g1;
  long utemp, vtemp, gtemp;

  g1 = b;
  u1 = 0;
  v1 = 1;
  g = a;
  u = 1;
  v = 0;
  while (g1 != 0)
    {
      q = g / g1;
      gtemp = g - q * g1;
      utemp = u - q * u1;
      vtemp = v - q * v1;
      g = g1;
      u = u1;
      v = v1;
      g1 = gtemp;
      u1 = utemp;
      v1 = vtemp;
    }
  return g;
}

void ZZp_FROM_INT(long charac, long &a, long b)
{
  a = b % charac;
  if (a < 0) a += charac;
}
void ZZp_NEGATE(long charac, long &a) { a = charac - a; }
void ZZp_APXY(long charac, long &a, long b, long c)
{
  a = (a + b * c) % charac;
}
void ZZp_ADD_TO(long charac, long &a, long b)
{
  a += b;
  if (a >= charac) a -= charac;
}
void ZZp_SUBTRACT_TO(long charac, long &a, long b)
{
  a -= b;
  if (a < 0) a += charac;
}
void ZZp_MULT(long charac, long &a, long b)
{
  a *= b;
  a %= charac;
}
void ZZp_INVERT(long charac, long &result, long b)
{
  long u, v;
  gcd_extended(charac, b, u, v);
  if (v < 0) v += charac;
  result = v;
}
void ZZp_RANDOM(long charac, long &result)
{
  result = rawRandomInt(static_cast<int32_t>(charac));
}

void DPoly::initialize(long p, int nvars0, const_poly *ext0)
{
  charac = p;
  nvars = nvars0;
  nlevels = nvars0;
  extensions = newarray(poly, nlevels);
  if (ext0 == 0)
    for (int i = 0; i < nlevels; i++) extensions[i] = 0;
  else
    for (int i = 0; i < nlevels; i++)
      {
        extensions[i] = copy(nlevels - 1, ext0[i]);
        down_level(i, nlevels - 1, extensions[i]);
      }
}
DPoly::DPoly(long p, int nvars0, const_poly *ext0)
{
  initialize(p, nvars0, ext0);
}

int DPoly::degree_of_extension(int level)
{
  // if negative, then that variable is transcendental over lower vars
  if (level < 0 || level >= nlevels) return -1;
  poly f = extensions[level];
  if (f == 0) return -1;
  return f->deg;
}

bool DPoly::down_level(int newlevel, int oldlevel, poly &f)
{
  if (f == 0) return true;
  for (int i = oldlevel; i > newlevel; i--)
    {
      if (f->deg > 0)
        {
          dealloc_poly(f);
          return false;
        }
      poly g = f->arr.polys[0];
      f->arr.polys[0] = 0;
      dealloc_poly(f);
      f = g;
    }
  return true;
}

static int n_nonzero_terms(int level, const_poly f)
{
  if (f == 0) return 0;
  int nterms = 0;
  if (level == 0)
    {
      for (int i = 0; i <= f->deg; i++)
        if (f->arr.ints[i] != 0) nterms++;
    }
  else
    {
      for (int i = 0; i <= f->deg; i++)
        if (f->arr.polys[i] != 0) nterms++;
    }
  return nterms;
}

void DPoly::elem_text_out(buffer &o,
                          int level,
                          const poly f,
                          bool p_one,
                          bool p_plus,
                          bool p_parens,
                          M2_ArrayString names) const
{
  // o << to_string(level, f);
  if (f == 0)
    {
      o << "0";
      return;
    }

  int nterms = n_nonzero_terms(level, f);
  bool needs_parens = p_parens && (nterms >= 2);

  if (needs_parens)
    {
      if (p_plus) o << '+';
      o << '(';
      p_plus = false;
    }

  bool one = is_one(level, f);

  if (one)
    {
      if (p_plus) o << "+";
      if (p_one) o << "1";
      return;
    }

  M2_string this_varname = names->array[level];

  if (level == 0)
    {
      bool firstterm = true;
      for (int i = f->deg; i >= 0; i--)
        if (f->arr.ints[i] != 0)
          {
            if (!firstterm || p_plus) o << "+";
            firstterm = false;
            if (i == 0 || f->arr.ints[i] != 1) o << f->arr.ints[i];
            if (i > 0) o << this_varname;
            if (i > 1) o << i;
          }
      if (needs_parens) o << ")";
    }
  else
    {
      bool firstterm = true;
      for (int i = f->deg; i >= 0; i--)
        if (f->arr.polys[i] != 0)
          {
            bool this_p_parens = p_parens || (i > 0);

            if (i == 0 || !is_one(level - 1, f->arr.polys[i]))
              elem_text_out(o,
                            level - 1,
                            f->arr.polys[i],
                            p_one,
                            p_plus || !firstterm,
                            this_p_parens,
                            names);
            else if (p_plus || !firstterm)
              o << "+";
            if (i > 0) o << this_varname;
            if (i > 1) o << i;

            firstterm = false;
          }
      if (needs_parens) o << ")";
    }
}

void DPoly::extensions_text_out(buffer &o, M2_ArrayString names) const
{
  for (int i = 0; i < nlevels; i++)
    {
      if (extensions[i] != 0)
        {
          o << newline << "    ";
          elem_text_out(o, i, extensions[i], true, false, false, names);
        }
    }
}

void DPoly::increase_size_0(int newdeg, poly &f)
{
  assert(f != 0);
  if (f->len <= newdeg)
    {
      long *newelems = newarray_atomic(long, newdeg + 1);
      long *fp = f->arr.ints;
      for (int i = 0; i <= f->deg; i++) newelems[i] = fp[i];
      for (int i = f->deg + 1; i < newdeg + 1; i++) newelems[i] = 0;
      deletearray(fp);
      f->arr.ints = newelems;
      f->len = newdeg + 1;
      f->deg = newdeg;
    }
}

void DPoly::increase_size_n(int newdeg, poly &f)
{
  assert(f != 0);
  if (f->len <= newdeg)
    {
      poly *newelems = newarray(poly, newdeg + 1);
      poly *fp = f->arr.polys;
      for (int i = 0; i <= f->deg; i++) newelems[i] = fp[i];
      for (int i = f->deg + 1; i < newdeg + 1; i++) newelems[i] = 0;
      deletearray(fp);
      f->arr.polys = newelems;
      f->len = newdeg + 1;
      f->deg = newdeg;
    }
}

poly DPoly::alloc_poly_n(int deg, poly *elems)
// if elems == 0, then set all coeffs to 0.
{
  poly result = new poly_struct;
  result->arr.polys = newarray(poly, deg + 1);
  result->deg = deg;
  result->len = deg + 1;
  if (elems == 0)
    for (int i = 0; i <= deg; i++) result->arr.polys[i] = 0;
  else
    for (int i = 0; i <= deg; i++) result->arr.polys[i] = elems[i];
  return result;
}

poly DPoly::alloc_poly_0(int deg, long *elems)
{
  poly result = new poly_struct;
  result->arr.ints = newarray_atomic(long, deg + 1);
  result->deg = deg;
  result->len = deg + 1;
  if (elems == 0)
    for (int i = 0; i <= deg; i++) result->arr.ints[i] = 0;
  else
    for (int i = 0; i <= deg; i++) result->arr.ints[i] = elems[i];
  return result;
}

void DPoly::dealloc_poly(poly &f)
// only f is freed, not any pointers in the array of f
{
  if (f == 0) return;
  deletearray(f->arr.polys);
  delete f;
  f = 0;
}

poly DPoly::read_poly_n(char *&str, int level)
{
  int len = 0;
  poly *elems = newarray(poly, 100);
  poly this_elem = 0;
  if (*str != '[')
    {
      fprintf(stderr, "expected '[', got %s\n", str);
      exit(1);
    }
  str++;
  // Now loop
  while (*str != ']')
    {
      while (isspace(*str)) str++;
      if (*str == ',')
        {
          str++;
        }
      else if (*str == '[')
        {
          this_elem = read_poly(str, level - 1);  // eats ]
          while (isspace(*str)) str++;
          if (*str == ',') str++;
        }
      else
        {
          fprintf(stderr, "expected , or [, but got %s\n", str);
          exit(1);
        }
      elems[len++] = this_elem;
      this_elem = 0;
    }
  // the only way to get here is if *str == ']'.  Eat that char.
  str++;
  poly result = DPoly::alloc_poly_n(len - 1, elems);
  deletearray(elems);
  return result;
}

poly DPoly::read_poly_0(char *&str)
{
  int len = 0;
  long *elems = newarray_atomic(long, 100);
  long this_elem = 0;
  if (*str != '[')
    {
      fprintf(stderr, "expected '[', got %s\n", str);
      exit(1);
    }
  str++;
  // Now loop
  while (*str != ']')
    {
      while (isspace(*str)) str++;
      if (*str == ',')
        {
          str++;
        }
      else if (isdigit(*str))
        {
          char *end;
          this_elem = strtol(str, &end, 10);
          str = end;
          while (isspace(*str)) str++;
          if (*str == ',') str++;
        }
      else
        {
          fprintf(stderr, "expected , or [, but got %s\n", str);
          exit(1);
        }
      elems[len++] = this_elem;
      this_elem = 0;
    }
  // the only way to get here is if *str == ']'.  Eat that char.
  str++;
  poly result = DPoly::alloc_poly_0(len - 1, elems);
  deletearray(elems);
  return result;
}

poly DPoly::read_poly(char *&str, int level)
{
  if (level > 0) return read_poly_n(str, level);
  return read_poly_0(str);
}

std::ostream &DPoly::append_to_stream(std::ostream &o, int level, const poly f)
{
  if (f == 0)
    o << "0";
  else if (level == 0)
    {
      long *p = f->arr.ints;
      o << "[";
      for (int i = 0; i <= f->deg; i++)
        {
          if (i > 0) o << ",";
          if (p[i] != 0) o << p[i];
        }
      o << "]";
    }
  else
    {
      poly *p = f->arr.polys;
      o << "[";
      for (int i = 0; i <= f->deg; i++)
        {
          if (i > 0) o << ",";
          if (p[i] != 0) append_to_stream(o, level - 1, p[i]);
        }
      o << "]";
    }
  return o;
}
char *DPoly::to_string(int level, const poly f)
{
  std::ostringstream o;
  append_to_stream(o, level, f);
  o << '\0';
  const char *s = o.str().c_str();  // only valid until o is destroyed
  size_t n = strlen(s);
  char *result = new char[n + 1];
  strcpy(result, s);
  return result;
}

void DPoly::display_poly(FILE *fil, int level, const poly f)
{
  if (f == 0)
    fprintf(fil, "0");
  else if (level == 0)
    {
      long *p = f->arr.ints;
      // fprintf(fil, "[(%ld)", f->deg);
      fprintf(fil, "[");
      for (int i = 0; i <= f->deg; i++)
        {
          if (i > 0) fprintf(fil, ",");
          if (p[i] != 0) fprintf(fil, "%ld", p[i]);
        }
      fprintf(fil, "]");
    }
  else
    {
      poly *p = f->arr.polys;
      // fprintf(fil, "[(%ld)", f->deg);
      fprintf(fil, "[");
      for (int i = 0; i <= f->deg; i++)
        {
          if (i > 0) fprintf(fil, ",");
          if (p[i] != 0) display_poly(fil, level - 1, p[i]);
        }
      fprintf(fil, "]");
    }
}

void dpoly(int level, const poly f) { DPoly::display_poly(stdout, level, f); }
bool DPoly::is_equal(int level, const poly f, const poly g)
{
  if (f == 0)
    {
      if (g == 0) return true;
      return false;
    }
  if (g == 0 || f->deg != g->deg) return false;
  if (level == 0)
    {
      long *fp = f->arr.ints;
      long *gp = g->arr.ints;
      for (int i = 0; i <= f->deg; i++)
        if (fp[i] != gp[i]) return false;
      return true;
    }
  // level > 0
  poly *fp = f->arr.polys;
  poly *gp = g->arr.polys;
  for (int i = 0; i <= f->deg; i++)
    if (!is_equal(level - 1, fp[i], gp[i])) return false;
  return true;
}

poly DPoly::copy(int level, const_poly f)
{
  if (f == 0) return 0;
  poly result;
  if (level == 0)
    {
      result = alloc_poly_0(f->deg);
      for (int i = 0; i <= f->deg; i++) result->arr.ints[i] = f->arr.ints[i];
    }
  else
    {
      result = alloc_poly_n(f->deg);
      for (int i = 0; i <= f->deg; i++)
        result->arr.polys[i] = copy(level - 1, f->arr.polys[i]);
    }
  return result;
}

poly DPoly::from_long(int level, long c)
{
  if (c == 0) return 0;
  poly result = alloc_poly_0(0);
  result->arr.ints[0] = c;
  for (int i = 1; i <= level; i++)
    {
      poly a = result;
      result = alloc_poly_n(0);
      result->arr.polys[0] = a;
    }
  return result;
}

poly DPoly::var(int level, int v)
// make the variable v (but at level 'level')
{
  if (v > level) return 0;
  int which = (v == 0 ? 1 : 0);
  poly result =
      alloc_poly_0(which);  // TODO: check that this initializes elements to 0
  result->arr.ints[which] = 1;
  for (int i = 1; i <= level; i++)
    {
      which = (i == v ? 1 : 0);
      poly a = result;
      result = alloc_poly_n(which);
      result->arr.polys[which] = a;
    }
  return result;
}

poly DPoly::random_0(int deg)
{
  if (deg < 0) deg = 3;  // Take a random element of degree 0.
  poly f = alloc_poly_0(deg);
  for (int i = 0; i <= deg; i++) ZZp_RANDOM(charac, f->arr.ints[i]);
  reset_degree_0(f);  // possibly modifies f, if it is zero.
  return f;
}
poly DPoly::random_n(int level, int deg)
{
  if (deg < 0) deg = 3;  // Take a random element of degree 0.
  poly f = alloc_poly_n(deg);
  for (int i = 0; i <= deg; i++) f->arr.polys[i] = random(level - 1);
  reset_degree_n(level, f);  // possibly modifies f, if it is zero.
  return f;
}
poly DPoly::random(int level, int deg)
{
  if (deg < 0) deg = 0;  // Take a random element of degree 0.
  if (level == 0) return random_0(deg);
  return random_n(level, deg);
}
poly DPoly::random(int level)
{
  return random(level, degree_of_extension(level));
}

int DPoly::compare(int level, poly f, poly g)
// returns -1 if f < g, 0 if f == g, and 1 if f > g
// order used: first degree, then compare elements 0..charac-1
// 0 is the lowest
{
  if (f == 0)
    {
      if (g == 0) return 0;
      return 1;
    }
  if (g == 0) return -1;
  if (f->deg > g->deg) return -1;
  if (f->deg < g->deg) return 1;

  if (level == 0)
    {
      for (int i = f->deg; i >= 0; i--)
        {
          long cmp = f->arr.ints[i] - g->arr.ints[i];
          if (cmp > 0) return -1;
          if (cmp < 0) return 1;
        }
    }
  else
    {
      for (int i = f->deg; i >= 0; i--)
        {
          int cmp = compare(level - 1, f->arr.polys[i], g->arr.polys[i]);
          if (cmp > 0) return -1;
          if (cmp < 0) return 1;
        }
    }
  return 0;
}

bool DPoly::is_one(int level, poly f)
{
  if (f == 0) return false;
  if (f->deg != 0) return false;
  if (level == 0)
    return 1 == f->arr.ints[0];
  else
    return is_one(level - 1, f->arr.polys[0]);
}
void DPoly::negate_in_place(int level, poly &f)
{
  if (f == 0) return;
  if (level == 0)
    {
      int deg = f->deg;
      long *p = f->arr.ints;
      for (int i = 0; i <= deg; i++)
        if (p[i] != 0) ZZp_NEGATE(charac, p[i]);
    }
  else
    {
      int deg = f->deg;
      poly *p = f->arr.polys;
      for (int i = 0; i <= deg; i++)
        if (p[i] != 0) negate_in_place(level - 1, p[i]);
    }
}

void DPoly::reset_degree_0(poly &f)
{
  int fdeg = f->deg;
  for (int j = fdeg; j >= 0; --j)
    if (f->arr.ints[j] != 0)
      {
        f->deg = j;
        return;
      }
  // at this point, everything is 0!
  dealloc_poly(f);  // sets f to 0
}
void DPoly::reset_degree_n(int level, poly &f)
{
  int fdeg = f->deg;
  for (int j = fdeg; j >= 0; --j)
    if (f->arr.polys[j] != 0)
      {
        f->deg = j;
        return;
      }
  // at this point, everything is 0!
  dealloc_poly(f);  // sets f to 0
}

void DPoly::add_term(int level, poly &result, long coeff, exponents exp) const
{
  // modifies result.
  // exp is an array [0..level-1] of exponent values for each variable
  // 0..level-1
  // the outer variable is at index 0.
  // coeff is an already normalized coefficient, and is not 0.

  int e = exp[0];

  if (result == 0)
    result = alloc_poly_n(e, 0);
  else if (result->deg < e)
    increase_size_n(e, result);

  if (level == 0)
    ZZp_ADD_TO(charac, result->arr.ints[e], coeff);
  else
    add_term(level - 1, result->arr.polys[e], coeff, exp + 1);
}

void DPoly::add_in_place_0(poly &f, const poly g)
{
  int i;
  if (g == 0) return;
  if (f == 0)
    {
      f = copy(0, g);
      return;
    }
  int fdeg = f->deg;
  int gdeg = g->deg;

  increase_size_0(g->deg, f);
  for (i = 0; i <= gdeg; i++)
    ZZp_ADD_TO(charac, f->arr.ints[i], g->arr.ints[i]);
  if (gdeg > fdeg)
    f->deg = gdeg;
  else if (gdeg == fdeg)
    reset_degree_0(f);
}

void DPoly::add_in_place_n(int level, poly &f, const poly g)
{
  int i;
  if (g == 0) return;
  if (f == 0)
    {
      f = copy(level, g);
      return;
    }
  int fdeg = f->deg;
  int gdeg = g->deg;

  increase_size_n(g->deg, f);
  for (i = 0; i <= gdeg; i++)
    add_in_place(level - 1, f->arr.polys[i], g->arr.polys[i]);
  if (gdeg > fdeg)
    f->deg = gdeg;
  else if (gdeg == fdeg)
    {
      // need to change the degree
      for (int j = fdeg; j >= 0; --j)
        if (f->arr.polys[j] != 0)
          {
            f->deg = j;
            return;
          }
      // at this point, everything is 0!
      dealloc_poly(f);
    }
}

void DPoly::add_in_place(int level, poly &f, const poly g)
{
  if (level == 0)
    add_in_place_0(f, g);
  else
    add_in_place_n(level, f, g);
}

void DPoly::subtract_in_place_0(poly &f, const poly g)
{
  int i;
  if (g == 0) return;
  if (f == 0)
    {
      f = copy(0, g);
      negate_in_place(0, f);
      return;
    }
  int fdeg = f->deg;
  int gdeg = g->deg;

  increase_size_0(g->deg, f);
  for (i = 0; i <= gdeg; i++)
    ZZp_SUBTRACT_TO(charac, f->arr.ints[i], g->arr.ints[i]);
  if (gdeg > fdeg)
    f->deg = gdeg;
  else if (gdeg == fdeg)
    {
      // need to change the degree
      for (int j = fdeg; j >= 0; --j)
        if (f->arr.ints[j] != 0)
          {
            f->deg = j;
            return;
          }
      // at this point, everything is 0!
      dealloc_poly(f);
    }
}

void DPoly::subtract_in_place_n(int level, poly &f, const poly g)
{
  int i;
  if (g == 0) return;
  if (f == 0)
    {
      f = copy(level, g);
      negate_in_place(level, f);
      return;
    }
  int fdeg = f->deg;
  int gdeg = g->deg;

  increase_size_n(g->deg, f);
  for (i = 0; i <= gdeg; i++)
    subtract_in_place(level - 1, f->arr.polys[i], g->arr.polys[i]);
  if (gdeg > fdeg)
    f->deg = gdeg;
  else if (gdeg == fdeg)
    {
      // need to change the degree
      for (int j = fdeg; j >= 0; --j)
        if (f->arr.polys[j] != 0)
          {
            f->deg = j;
            return;
          }
      // at this point, everything is 0!
      dealloc_poly(f);
    }
}

void DPoly::subtract_in_place(int level, poly &f, const poly g)
{
  if (level == 0)
    subtract_in_place_0(f, g);
  else
    subtract_in_place_n(level, f, g);
}

poly DPoly::mult_0(const poly f, const poly g, bool reduce_by_extension)
{
  if (f == 0 || g == 0) return 0;
  poly result = alloc_poly_0(f->deg + g->deg);

  for (int i = 0; i <= f->deg; i++)
    {
      long a = f->arr.ints[i];
      for (int j = 0; j <= g->deg; j++)
        ZZp_APXY(charac, result->arr.ints[i + j], a, g->arr.ints[j]);
    }

  if (reduce_by_extension && extensions[0] != 0)
    remainder(0, result, extensions[0]);
  return result;
}
poly DPoly::mult_n(int level,
                   const poly f,
                   const poly g,
                   bool reduce_by_extension)
{
  if (f == 0 || g == 0) return 0;
  poly result = alloc_poly_n(f->deg + g->deg);

  for (int i = 0; i <= f->deg; i++)
    {
      poly a = f->arr.polys[i];
      if (a != 0)
        for (int j = 0; j <= g->deg; j++)
          {
            poly b = g->arr.polys[j];
            poly c = mult(level - 1, a, b, true);
            if (c != 0)
              {
                add_in_place(level - 1, result->arr.polys[i + j], c);
                dealloc_poly(c);
              }
          }
    }

  if (reduce_by_extension && extensions[level] != 0)
    remainder(level, result, extensions[level]);
  return result;
}
poly DPoly::mult(int level,
                 const poly f,
                 const poly g,
                 bool reduce_by_extension)
{
  if (level == 0) return mult_0(f, g, reduce_by_extension);
  return mult_n(level, f, g, reduce_by_extension);
}

poly DPoly::invert(int level, const poly a)
{
  // plan: compute the extended gcd of a and extensions[level]
  //   as univariate polynomials (at level 'level').
  // either return 0, if the gcd returned was not 1, or return
  // result_u.
  poly u, v;
  poly g = gcd_coefficients(level, a, extensions[level], u, v);
  if (!is_one(level, g))
    {
      dealloc_poly(u);
    }
  dealloc_poly(g);
  dealloc_poly(v);
  return u;
}

void DPoly::mult_by_coeff_0(poly &f, long b)
{
  if (f == 0) return;
  long *p = f->arr.ints;
  long deg = f->deg;
  if (b == 0)
    dealloc_poly(f);
  else if (b != 1)
    for (int i = 0; i <= deg; i++)
      {
        if (*p != 0) ZZp_MULT(charac, *p, b);
        p++;
      }
}
void DPoly::mult_by_coeff_n(int level, poly &f, poly b)
{
  if (f == 0) return;
  poly *p = f->arr.polys;
  long deg = f->deg;
  if (b == 0)
    {
      dealloc_poly(f);
    }
  else if (!is_one(level - 1, b))
    for (int i = 0; i <= deg; i++)
      {
        if (*p != 0) *p = mult(level - 1, *p, b, true);
        p++;
      }
}
void DPoly::make_monic_0(poly &f, long &result_multiplier)
{
  if (f == 0) return;
  long *p = f->arr.ints;
  long a = p[f->deg];
  long b;
  ZZp_INVERT(charac, b, a);
  mult_by_coeff_0(f, b);
  result_multiplier = b;
}
void DPoly::make_monic_n(int level, poly &f, poly &result_multiplier)
{
  if (f == 0) return;
  poly *p = f->arr.polys;
  poly a = p[f->deg];
  poly b = invert(level - 1, a);
  mult_by_coeff_n(level, f, b);
  result_multiplier = b;
}

void DPoly::make_monic(int level, poly &f)
{
  if (f == 0) return;
  if (level == 0)
    {
      long not_used;
      make_monic_0(f, not_used);
    }
  else
    {
      poly not_used;
      make_monic_n(level, f, not_used);
      dealloc_poly(not_used);
    }
}

bool DPoly::make_monic3(int level, poly &u1, poly &u2, poly &u3)
// let c be the inverse of the lead coefficient of u3.
// return false if this lead coeff is not invertible
// else return true
// replace u1, u2, u3 by c*u1, c*u2, c*u3
{
  if (u3 == 0) return true;

  if (level == 0)
    {
      long c = 0;
      make_monic_0(u3, c);
      if (c == 0) return false;
      mult_by_coeff_0(u1, c);
      mult_by_coeff_0(u2, c);
    }
  else
    {
      poly c = 0;
      make_monic_n(level, u3, c);
      if (c == 0) return false;
      mult_by_coeff_n(level, u1, c);
      mult_by_coeff_n(level, u2, c);
      dealloc_poly(c);
    }
  return true;
}

poly DPoly::division_in_place_monic(int level, poly &f, const poly g)
{
  // ASSUMPTION: g is MONIC, non-zero
  if (f == 0) return 0;
  if (f->deg < g->deg)
    {
      return 0;
    }
  int shift = f->deg - g->deg;
  poly quot = alloc_poly_n(shift);

  if (level == 0)
    {
      long *p = f->arr.ints;
      long *q = g->arr.ints;
      for (int d = f->deg; shift >= 0; d--, shift--)
        {
          long a = p[d];
          if (a != 0)
            {
              quot->arr.ints[shift] = a;
              ZZp_NEGATE(charac, a);
              for (int j = 0; j <= g->deg; j++)
                ZZp_APXY(charac, p[shift + j], a, q[j]);
            }
        }
      reset_degree_0(f);
    }
  else
    {
      poly *p = f->arr.polys;
      poly *q = g->arr.polys;
      for (int d = f->deg; shift >= 0; d--, shift--)
        {
          poly a = p[d];
          if (a != 0)
            {
              quot->arr.polys[shift] = copy(level - 1, a);
              for (int j = 0; j <= g->deg; j++)
                {
                  poly b = mult(level - 1, a, q[j], true);
                  subtract_in_place(level - 1, p[j + shift], b);
                }
            }
        }
      reset_degree_n(level, f);
    }
  return quot;
}
bool DPoly::division_in_place(int level,
                              poly &f,
                              const poly g,
                              poly &result_quot)
// returns false if the lead coeff is not invertible
{
  assert(g != 0);
  if (f == 0 || f->deg < g->deg)
    {
      result_quot = 0;
      return true;
    }
  int shift = f->deg - g->deg;
  result_quot = alloc_poly_n(shift);

  if (level == 0)
    {
      // TODO: this code seems completely wrong!??!!  too?
      long *p = f->arr.ints;
      long *q = g->arr.ints;
      long leadcoeff = q[g->deg];
      long invlead = 1;
      if (leadcoeff != 1)
        {
          ZZp_INVERT(charac, invlead, leadcoeff);
        }
      for (int d = f->deg; shift >= 0; d--, shift--)
        {
          long a = p[d];
          if (a != 0)
            {
              ZZp_MULT(charac, a, invlead);
              result_quot->arr.ints[shift] = a;
              ZZp_NEGATE(charac, a);
              for (int j = 0; j <= g->deg; j++)
                ZZp_APXY(charac, p[shift + j], a, q[j]);
            }
        }
      reset_degree_0(f);
      return true;
    }
  else
    {
      // TODO: this code seems completely wrong!??!!
      poly *p = f->arr.polys;
      poly *q = g->arr.polys;
      poly leadcoeff = q[g->deg];
      poly invlead;
      if (is_one(level - 1, leadcoeff))
        invlead = leadcoeff;
      else
        {
          invlead = invert(level - 1, leadcoeff);
          if (invlead == 0) return false;
        }
      for (int d = f->deg; shift >= 0; d--, shift--)
        {
          poly a = p[d];
          if (a != 0)
            {
              poly b = mult(level - 1, invlead, a, true);
              result_quot->arr.polys[shift] = copy(level - 1, b);
              for (int j = 0; j <= g->deg; j++)
                {
                  b = mult(level - 1, a, q[j], true);
                  subtract_in_place(level - 1, p[j + shift], b);
                }
            }
        }
      reset_degree_n(level, f);
      return true;
    }
}

void DPoly::remainder(int level, poly &f, const poly g)
{
  if (g == 0) return;
  poly quot = 0;
  division_in_place(level, f, g, quot);
  dealloc_poly(quot);
}

void DPoly::pseudo_remainder(int level, poly &f, const poly g)
{
  if (g == 0) return;
  // TODO: write
}
poly DPoly::pseudo_division(int level, poly &f, const poly g)
{
  if (g == 0) return 0;
  // TODO: write
  return 0;
}
poly DPoly::resultant(int level, poly f, poly g)
{
  // TODO: write
  return 0;
}
static void swap_poly(poly &f, poly &g)
{
  poly a = f;
  f = g;
  g = a;
}
poly DPoly::gcd(int level, const poly f, const poly g)
{
  poly F = copy(level, f);
  poly G = copy(level, g);
  if (G == 0)
    {
      G = F;
      F = 0;
    }
  for (;;)
    {
#ifdef DEBUGGCD
      printf("G = %s\n", to_string(level, G));
#endif
      make_monic(level, G);
      if (G == 0) return 0;  // failed

#ifdef DEBUGGCD
      printf("monic G = %s\n", to_string(level, G));
      printf("F       = %s\n", to_string(level, F));
#endif

      remainder(level, F, G);  // modifies F
      if (F == 0) return G;

#ifdef DEBUGGCD
      printf("F mod G     = %s\n", to_string(level, F));
#endif

      swap_poly(F, G);
    }
}

poly DPoly::gcd_coefficients(int level,
                             const poly f,
                             const poly g,
                             poly &result_u,
                             poly &result_v)
{
  // Assumption:
  //  f and g are non-zero
  poly v1, v2, v3;
  poly u1, u2, u3;
  poly q = 0;

  v1 = 0;
  v2 = from_long(level, 1);
  v3 = copy(level, g);

  u1 = from_long(level, 1);
  u2 = 0;
  u3 = copy(level, f);

  if (v3 == 0 || (u3 != 0 && v3->deg > u3->deg))
    {
      swap_poly(u1, v1);
      swap_poly(u2, v2);
      swap_poly(u3, v3);
    }

// At the end of the loop:
// u1 * f + u2 * g == u3
// v1 * f + v2 * g == v3
// (and (v1,v2,v3) is close to the gcd

#ifdef DEBUGGCD
  if (level == 1)
    {
      printf("u1 = %s\n", to_string(level, u1));
      printf("u2 = %s\n", to_string(level, u2));
      printf("u3 = %s\n", to_string(level, u3));

      printf("v1 = %s\n", to_string(level, v1));
      printf("v2 = %s\n", to_string(level, v2));
      printf("v3 = %s\n", to_string(level, v3));
    }
#endif

  while (v3 != 0)
    {
      if (!make_monic3(level, v1, v2, v3))
        {
          // deallocate some polynomials, then return 0.  No monic gcd
          dealloc_poly(q);
          dealloc_poly(u1);
          dealloc_poly(u2);
          dealloc_poly(u3);
          dealloc_poly(v1);
          dealloc_poly(v2);
          dealloc_poly(v3);
          result_u = 0;
          result_v = 0;
          return 0;
        }
      q = division_in_place_monic(
          level,
          u3,
          v3);  // u3 := u3 - q*v3, as v3 is monic, this is always possible

#ifdef DEBUGGCD
      if (level == 1)
        {
          printf("q = %s\n", to_string(level, q));
          printf("u3 = %s\n", to_string(level, u3));
        }
#endif

      negate_in_place(level, q);
      poly a = mult(level, q, v1, false);
      poly b = mult(level, q, v2, false);
      add_in_place(level, u1, a);
      add_in_place(level, u2, b);

#ifdef DEBUGGCD
      if (level == 1)
        {
          printf("u1 = %s\n", to_string(level, u1));
          printf("u2 = %s\n", to_string(level, u2));
          printf("u3 = %s\n", to_string(level, u3));
        }
#endif
      dealloc_poly(a);  // MES: totally wipeout polys a, b here!
      dealloc_poly(b);
      swap_poly(u1, v1);
      swap_poly(u2, v2);
      swap_poly(u3, v3);
    }

#ifdef DEBUGGCD
  if (level == 1)
    {
      printf("u1 = %s\n", to_string(level, u1));
      printf("u2 = %s\n", to_string(level, u2));
      printf("u3 = %s\n", to_string(level, u3));
    }
#endif

  // At this point u3 is monic, and is the monic gcd

  // v3 is already 0 here.
  dealloc_poly(v1);
  dealloc_poly(v2);

  result_u = u1;
  result_v = u2;
  return u3;
}

int DPoly::degree(int level, int whichvar, const poly f) const
{
  if (f == 0) return -1;
  if (whichvar == 0) return f->deg;
  // At this point, we need to find the max degree of the given var
  int deg = -1;
  for (int i = 0; i <= f->deg; i++)
    {
      poly g = f->arr.polys[i];
      if (g != 0)
        {
          int d = degree(level - 1, whichvar - 1, g);
          if (d > deg) deg = d;
        }
    }
  return deg;
}

poly DPoly::mult_by_int_0(long a, const poly f)
{
  poly result = alloc_poly_0(f->deg);
  for (int i = 0; i <= f->deg; i++)
    {
      long c = f->arr.ints[i];
      if (c != 0)
        {
          ZZp_MULT(charac, c, a);
          result->arr.ints[i] = c;
        }
    }
  reset_degree_0(result);
  return result;
}
poly DPoly::mult_by_int_n(int level, long a, const poly f)
{
  poly result = alloc_poly_n(f->deg);
  for (int i = 0; i <= f->deg; i++)
    {
      poly c = f->arr.polys[i];
      if (c != 0) result->arr.polys[i] = mult_by_int(level - 1, a, c);
    }
  reset_degree_n(level, result);
  return result;
}

poly DPoly::mult_by_int(int level, long a, const poly f)
{
  if (f == 0) return 0;
  if (level == 0) return mult_by_int_0(a, f);
  return mult_by_int_n(level, a, f);
}

poly DPoly::diff_0(const poly f)
{
  if (f == 0 || f->deg == 0) return 0;
  poly result = alloc_poly_0(f->deg - 1);
  for (int i = 1; i <= f->deg; i++)
    {
      long c = f->arr.ints[i];
      if (c != 0)
        {
          ZZp_MULT(charac, c, i);
          result->arr.ints[i - 1] = c;
        }
    }
  reset_degree_0(result);
  return result;
}

poly DPoly::diff_n(int level, int whichvar, const poly f)
{
  poly result;
  if (whichvar == 0)
    {
      result = alloc_poly_0(f->deg - 1);
      for (int i = 1; i <= f->deg; i++)
        {
          poly c = f->arr.polys[i];
          if (c != 0) result->arr.polys[i - 1] = mult_by_int(level - 1, i, c);
        }
    }
  else
    {
      result = alloc_poly_0(f->deg);
      for (int i = 0; i <= f->deg; i++)
        {
          poly c = f->arr.polys[i];
          if (c != 0) result->arr.polys[i] = diff(level - 1, whichvar - 1, c);
        }
    }
  reset_degree_n(level, result);
  return result;
}

poly DPoly::diff(int level, int whichvar, const poly f)
{
  if (f == 0) return 0;
  if (level == 0) return diff_0(f);
  return diff_n(level, whichvar, f);
}

poly DPoly::power_mod(int level, const poly f, mpz_srcptr m, const poly g)
// f^m mod g
{
  // We assume that m > 0. THIS IS NOT CHECKED!!
  mpz_t n;
  mpz_init_set(n, m);
  poly prod = from_long(level, 1);
  poly base = copy(level, f);
  poly tmp;

  for (;;)
    {
      //      fprintf(stdout, "prod = ");
      //      dpoly(level,prod);
      //      fprintf(stdout, "\nbase = ");
      //      dpoly(level,base);
      //      fprintf(stdout, "\n");
      if (RingZZ::mod_ui(n, 2) == 1)
        {
          tmp = mult(level, prod, base, false);
          remainder(level, tmp, g);
          // TODO: free prod
          prod = tmp;
        }
      mpz_tdiv_q_2exp(n, n, 1);
      if (mpz_sgn(n) == 0)
        {
          mpz_clear(n);
          //      fprintf(stdout, "final prod = ");
          //      dpoly(level,prod);
          //              fprintf(stdout, "\nbase = ");
          //              dpoly(level,base);
          //              fprintf(stdout, "\n");
          // TODO: free base
          return prod;
        }
      else
        {
          tmp = mult(level, base, base, false);
          remainder(level, tmp, g);
          // TODO: free base
          base = tmp;
        }
    }
}

poly DPoly::lowerP(int level, const poly f)
{
  int i, j;
  poly result;
  if (f == 0) return 0;
  int charac_as_int = static_cast<int>(charac);
  int newdeg = f->deg / charac_as_int;  // should be exact...
  if (level == 0)
    {
      result = alloc_poly_0(newdeg);
      // In this situation, we just need to grab every p*i coeff...
      for (i = 0, j = 0; i <= newdeg; i++, j += charac_as_int)
        result->arr.ints[i] = f->arr.ints[j];
    }
  else
    {
      result = alloc_poly_n(newdeg);
      mpz_t order;
      mpz_init(order);
      unsigned long extdeg = 1;
      for (i = 0; i < level; i++) extdeg *= degree_of_extension(i);
      mpz_ui_pow_ui(order, charac_as_int, extdeg - 1);
      for (i = 0, j = 0; i <= newdeg; i++, j += charac_as_int)
        {
          // need p-th roots of the coefficients.  So we take p^(n-1)
          // power (if coefficients are in field of size p^n)
          poly a = f->arr.polys[j];
          poly b = power_mod(level - 1, a, order, extensions[level - 1]);
          result->arr.polys[i] = b;
        }
      mpz_clear(order);
    }
  return result;
}

int DPoly::index_of_var(int level, const poly f) const
{
  if (f == 0 or level < 0 or f->deg >= 2) return -1;
  if (level == 0)
    {
      if (f->deg == 0) return -1;
      // At this point, degree is 1.
      if (f->arr.ints[0] == 0 and f->arr.ints[1] == 1)
        return 0;
      else
        return -1;
    }
  else
    {
      if (f->arr.polys[0] == 0 and is_one(level - 1, f->arr.polys[1]))
        return level;
      if (f->deg == 1) return -1;
      return index_of_var(level - 1, f->arr.polys[0]);
    }
}

void DPoly::degrees_of_vars(int level,
                            const poly f,
                            std::vector<int> &result_maxdegs) const
{
  // Set the values of result_maxdegs at indices: 0..level
  result_maxdegs[level] = std::max(result_maxdegs[level], f->deg);
  if (level == 0) return;
  for (int i = 0; i <= f->deg; i++)
    if (f->arr.polys[i] != 0)
      degrees_of_vars(level - 1, f->arr.polys[i], result_maxdegs);
}

DRing::DRing(long charac, int nvars, const_poly *exts)
    : level(nvars - 1), D(charac, nvars, exts), P(charac)
{
}

DRing *DRing::create(long p, int nvars0, const_poly *ext0)
{
  return new DRing(p, nvars0, ext0);
}

void DRing::set_from_int(poly &result, mpz_srcptr r)
{
  mpz_t a;
  mpz_init(a);
  mpz_mod_ui(a, r, P);
  long c = mpz_get_si(a);
  mpz_clear(a);
  if (c < 0) c += P;
  result = D.from_long(level, c);
}

bool DRing::set_from_mpq(poly &result, mpq_srcptr r)
{
  // returns false if r doesn't lift
  mpz_t a;
  mpz_init(a);
  mpz_mod_ui(a, mpq_numref(r), P);
  long ctop = mpz_get_si(a);
  mpz_mod_ui(a, mpq_denref(r), P);
  long cbottom = mpz_get_si(a);
  mpz_clear(a);
  if (ctop < 0) ctop += P;
  if (cbottom < 0) cbottom += P;
  if (cbottom == 0)
    {
      result = 0;
      return false;
    }
  ZZp_INVERT(P, cbottom, cbottom);
  ZZp_MULT(P, ctop, cbottom);

  result = D.from_long(level, ctop);
  return true;
}

int DRing::extension_degree(int firstvar)  // returns -1 if infinite
{
  int result = 1;
  for (int i = 0; i <= level - firstvar; i++)
    {
      int d = D.degree_of_extension(i);
      if (d < 0) return -1;
      result *= d;
    }
  return result;
}

void DRing::elem_text_out(buffer &o,
                          const poly f,
                          bool p_one,
                          bool p_plus,
                          bool p_parens,
                          M2_ArrayString names) const
{
  D.elem_text_out(o, level, f, p_one, p_plus, p_parens, names);
}

void DRing::add_term(elem &result, long coeff, exponents exp) const
{
  long c;
  ZZp_FROM_INT(P, c, coeff);  // puts it into normal form, just in case.
  if (c == 0) return;
  D.add_term(level, result, c, exp);
}

void DPolyTraverser::traverse(const_poly f)
{
  exponents exp = new int[D->nvars];
  for (size_t i = 0; i < D->nvars; i++) exp[i] = 0;
  traverse1(D->nlevels - 1,
            f,
            exp);  // the return value is only for the recursive algorithm
  delete[] exp;
}

bool DPolyTraverser::traverse1(int level, const_poly f, exponents exp)
{
  if (level == 0)
    {
      long *cfs = f->arr.ints;
      for (int i = f->deg; i >= 0; --i)
        if (cfs[i] != 0)
          {
            exp[level] = i;
            if (!viewTerm(cfs[i], exp)) return false;
          }
      exp[level] = 0;
    }
  else
    {
      poly *cfs = f->arr.polys;
      for (int i = f->deg; i >= 0; --i)
        if (cfs[i] != 0)
          {
            exp[level] = i;
            if (!traverse1(level - 1, cfs[i], exp)) return false;
          }
      exp[level] = 0;
    }
  return true;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
