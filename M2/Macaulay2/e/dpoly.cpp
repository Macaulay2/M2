#include "dpoly.hpp"
#include <cstdlib>
#include <cctype>
#include <strstream>
#include <cassert>

long gcd_extended(long a,
		  long b,
		  long &u,
		  long &v)
{
  long g;
  long q ;
  long u1, v1, g1;
  long utemp, vtemp, gtemp;
 
  g1 = b; u1 = 0; v1 = 1;
  g  = a;  u = 1;  v = 0;
  while (g1 != 0)
    {
      q = g / g1 ;
      gtemp=g - q * g1;
      utemp=u - q * u1;
      vtemp=v - q * v1;
      g  = g1;     u = u1;     v = v1 ;
      g1 = gtemp; u1 = utemp; v1 = vtemp;
    }
  return g;
}

void ZZp_NEGATE(long charac, long &a) { a = charac - a; }
void ZZp_APXY(long charac, long &a, long b, long c) { a = (a+b*c) % charac; }
void ZZp_ADD_TO(long charac, long &a, long b) { a += b; if (a >= charac) a -= charac; }
void ZZp_SUBTRACT_TO(long charac, long &a, long b) { a -= b; if (a < 0) a += charac; }
void ZZp_MULT(long charac, long &a, long b) { a *= b; a %= charac; }
void ZZp_INVERT(long charac, long &result, long b) {
  long u, v;
  gcd_extended(charac, b, u, v);
  if (v < 0) v += charac;
  result = v;
}
void ZZp_RANDOM(long charac, long &result) { result = rawRandomInt(charac); }

void DPoly::initialize(long p, int nvars0, const_poly *ext0)
{
  charac = p;
  nvars = nvars0;
  nlevels = nvars0;
  extensions = new poly[nlevels];
  if (ext0 == 0)
    for (int i=0; i<nlevels; i++)
      extensions[i] = 0;
  else
    for (int i=0; i<nlevels; i++)
      {
	extensions[i] = copy(nlevels, ext0[i]);
	down_level(i, nlevels, extensions[i]);
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
  for (int i=newlevel; i>oldlevel; i--)
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
      for (int i=0; i<=f->deg; i++)
	if (f->arr.ints[i] != 0) nterms++;
    }
  else
    {
      for (int i=0; i<=f->deg; i++)
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
  //o << to_string(level, f);
  if (f == 0)
    {
      o << "0";
      return;
    }

  int nterms = n_nonzero_terms(level,f);
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
      for (int i=f->deg; i>=0; i--)
	if (f->arr.ints[i] != 0)
	  {
	    if (!firstterm || p_plus) o << "+";
	    firstterm = false;
	    if (i == 0 || f->arr.ints[i] != 1)
	      o << f->arr.ints[i];
	    if (i  > 0)
	      o << this_varname;
	    if (i > 1)
	      o << i;
	  }
      if (needs_parens) o << ")";
    }
  else
    {
      bool firstterm = true;
      for (int i=f->deg; i>=0; i--)
	if (f->arr.polys[i] != 0)
	  {
	    bool this_p_parens = p_parens || (i > 0);

	    if (i == 0 || !is_one(level-1,f->arr.polys[i]))
	      elem_text_out(o, level-1,f->arr.polys[i], p_one, p_plus || !firstterm, this_p_parens, names);
	    else if (p_plus || !firstterm)
	      o << "+";
	    if (i  > 0)
	      o << this_varname;
	    if (i > 1)
	      o << i;

	    firstterm = false;
	  }
      if (needs_parens) o << ")";
    }
}

void DPoly::increase_size_0(int newdeg, poly &f)
{
  assert(f != 0);
  if (f->len <= newdeg)
    {
      long *newelems = new long[newdeg+1];
      long *fp = f->arr.ints;
      for (int i=0; i<= f->deg; i++)
	newelems[i] = fp[i];
      for (int i = f->deg+1; i < newdeg+1; i++)
	newelems[i] = 0;
      delete [] fp;
      f->arr.ints = newelems;
      f->len = newdeg+1;
      f->deg = newdeg;
    }
}

void DPoly::increase_size_n(int newdeg, poly &f)
{
  assert(f != 0);
  if (f->len <= newdeg)
    {
      poly *newelems = new poly[newdeg+1];
      poly *fp = f->arr.polys;
      for (int i=0; i<= f->deg; i++)
	newelems[i] = fp[i];
      for (int i = f->deg+1; i < newdeg+1; i++)
	newelems[i] = 0;
      delete [] fp;
      f->arr.polys = newelems;
      f->len = newdeg+1;
      f->deg = newdeg;
    }
}

poly DPoly::alloc_poly_n(long deg, poly *elems)
// if elems == 0, then set all coeffs to 0.
{
  poly result = new poly_struct;
  result->arr.polys = new poly[deg+1];
  result->deg = deg;
  result->len = deg+1;
  if (elems == 0)
    for (int i=0; i <= deg; i++)
      result->arr.polys[i] = 0;
  else
    for (int i=0; i <= deg; i++)
      result->arr.polys[i] = elems[i];
  return result;
}

poly DPoly::alloc_poly_0(long deg, long *elems)
{
  poly result = new poly_struct;
  result->arr.ints = new long[deg+1];
  result->deg = deg;
  result->len = deg+1;
  if (elems == 0)
    for (int i=0; i <= deg; i++)
      result->arr.ints[i] = 0;
  else
    for (int i=0; i <= deg; i++)
      result->arr.ints[i] = elems[i];
  return result;
}

void DPoly::dealloc_poly(poly &f)
 // only f is freed, not any pointers in the array of f
{
  if (f == 0) return;
  delete [] f->arr.polys;
  delete f;
  f = 0;
}

poly DPoly::read_poly_n(char * &str, int level)
{
  int len = 0;
  poly *elems = new poly[100];
  poly this_elem = 0;
  if (*str != '[')
    {
      fprintf(stderr,"expected '[', got %s\n", str);
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
	  this_elem = read_poly(str, level-1); // eats ]
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
  poly result = DPoly::alloc_poly_n(len-1, elems);
  delete [] elems;
  return result;
}

poly DPoly::read_poly_0(char * &str)
{
  int len = 0;
  long *elems = new long[100];
  long this_elem = 0;
  if (*str != '[')
    {
      fprintf(stderr,"expected '[', got %s\n", str);
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
	  this_elem = strtol(str,&end, 10);
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
  poly result = DPoly::alloc_poly_0(len-1, elems);
  delete [] elems;
  return result;
}

poly DPoly::read_poly(char * &str, int level)
{
  if (level > 0)
    return read_poly_n(str, level);
  return read_poly_0(str);
}

std::ostream& DPoly::append_to_stream(std::ostream &o, int level, const poly f)
{
  if (f == 0) 
    o << "0";
  else if (level == 0)
    {
      long *p = f->arr.ints;
      o << "[";
      for (int i=0; i<=f->deg; i++)
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
      for (int i=0; i<=f->deg; i++)
	{
	  if (i > 0) o << ",";
	  if (p[i] != 0) append_to_stream(o, level-1, p[i]);
	}
      o << "]";
    }
  return o;
}
char *DPoly::to_string(int level, const poly f)
{
  std::ostrstream o;
  append_to_stream(o, level, f);
  o << '\0';
  char *s = o.str(); // only valid until o is destroyed
  int n = strlen(s);
  char *result = new char[n+1];
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
      //fprintf(fil, "[(%ld)", f->deg);
      fprintf(fil, "[");
      for (int i=0; i<=f->deg; i++)
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
      for (int i=0; i<=f->deg; i++)
	{
	  if (i > 0) fprintf(fil, ",");
	  if (p[i] != 0) display_poly(fil, level-1, p[i]);
	}
      fprintf(fil, "]");
    }
}

void dpoly(int level, const poly f)
{
  DPoly::display_poly(stdout, level, f);
}

bool DPoly::is_equal(int level, const poly f, const poly g)
{
  if (f == 0)
    {
      if (g == 0) return true;
      return false;
    }
  if (f->deg != g->deg) return false;
  if (level == 0)
    {
      long *fp = f->arr.ints;
      long *gp = g->arr.ints;
      for (int i=0; i<=f->deg; i++)
	if (fp[i] != gp[i]) return false;
      return true;
    }
  // level > 0
  poly *fp = f->arr.polys;
  poly *gp = g->arr.polys;
  for (int i=0; i<=f->deg; i++)
    if (!is_equal(level-1,fp[i],gp[i])) return false;
  return true;
}

poly DPoly::copy(int level, const_poly f)
{
  if (f == 0) return 0;
  poly result;
  if (level == 0)
    {
      result = alloc_poly_0(f->deg);
      for (int i=0; i<=f->deg; i++) result->arr.ints[i] = f->arr.ints[i];
    }
  else
    {
      result = alloc_poly_n(f->deg);
      for (int i=0; i<=f->deg; i++) result->arr.polys[i] = copy(level-1, f->arr.polys[i]);
    }
  return result;
}

poly DPoly::from_int(int level, long c)
{
  if (c == 0) return 0;
  poly result = alloc_poly_0(0);
  result->arr.ints[0] = c;
  for (int i=1; i<=level; i++)
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
  poly result = alloc_poly_0(which); // TODO: check that this initializes elements to 0
  result->arr.ints[which] = 1;
  for (int i=1; i<=level; i++)
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
  if (deg < 0) deg = 3; // Take a random element of degree 0.
  poly f = alloc_poly_0(deg);
  for (int i=0; i<=deg; i++)
    ZZp_RANDOM(charac, f->arr.ints[i]);
  reset_degree_0(f); // possibly modifies f, if it is zero.
  return f;
}
poly DPoly::random_n(int level, int deg)
{
  if (deg < 0) deg = 3; // Take a random element of degree 0.
  poly f = alloc_poly_n(deg);
  for (int i=0; i<=deg; i++)
    f->arr.polys[i] = random(level-1);
  reset_degree_n(level,f); // possibly modifies f, if it is zero.
  return f;
}
poly DPoly::random(int level, int deg)
{
  if (deg < 0) deg = 3; // Take a random element of degree 0.
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
      for (int i=f->deg; i>=0; i--)
	{
	  int cmp = f->arr.ints[i] - g->arr.ints[i];
	  if (cmp > 0) return -1;
	  if (cmp < 0) return 1;
	}
    }
  else
    {
      for (int i=f->deg; i>=0; i--)
	{
	  int cmp = compare(level-1, f->arr.polys[i], g->arr.polys[i]);
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
    return is_one(level-1, f->arr.polys[0]);
}
void DPoly::negate_in_place(int level, poly &f)
{
  if (f == 0) return;
  if (level == 0)
    {
      int deg = f->deg;
      long *p = f->arr.ints;
      for (int i=0; i<=deg; i++)
	if (p[i] != 0) ZZp_NEGATE(charac, p[i]);
    }
  else
    {
      int deg = f->deg;
      poly *p = f->arr.polys;
      for (int i=0; i<=deg; i++)
	if (p[i] != 0) negate_in_place(level-1, p[i]);
    }
}

void DPoly::reset_degree_0(poly &f)
{
  int fdeg = f->deg;
  for (int j = fdeg; j>=0; --j)
    if (f->arr.ints[j] != 0) {
      f->deg = j;
      return;
    }
  // at this point, everything is 0!
  dealloc_poly(f); // sets f to 0
}
void DPoly::reset_degree_n(int level, poly &f)
{
  int fdeg = f->deg;
  for (int j = fdeg; j>=0; --j)
    if (f->arr.polys[j] != 0) {
      f->deg = j;
      return;
    }
  // at this point, everything is 0!
  dealloc_poly(f); // sets f to 0
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
  for (i=0; i<=gdeg; i++) ZZp_ADD_TO(charac, f->arr.ints[i], g->arr.ints[i]);
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
  for (i=0; i<=gdeg; i++) add_in_place(level-1, f->arr.polys[i], g->arr.polys[i]);
  if (gdeg > fdeg) f->deg = gdeg;
  else if (gdeg == fdeg) {
    // need to change the degree
    for (int j = fdeg; j>=0; --j)
      if (f->arr.polys[j] != 0) {
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
  for (i=0; i<=gdeg; i++) ZZp_SUBTRACT_TO(charac, f->arr.ints[i], g->arr.ints[i]);
  if (gdeg > fdeg) f->deg = gdeg;
  else if (gdeg == fdeg) {
    // need to change the degree
    for (int j = fdeg; j>=0; --j)
      if (f->arr.ints[j] != 0) {
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
  for (i=0; i<=gdeg; i++) subtract_in_place(level-1, f->arr.polys[i], g->arr.polys[i]);
  if (gdeg > fdeg) f->deg = gdeg;
  else if (gdeg == fdeg) {
    // need to change the degree
    for (int j = fdeg; j>=0; --j)
      if (f->arr.polys[j] != 0) {
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

  for (int i=0; i<=f->deg; i++)
    {
      long a = f->arr.ints[i];
      for (int j=0; j<=g->deg; j++)
	ZZp_APXY(charac, result->arr.ints[i+j], a, g->arr.ints[j]);
    }

  if (reduce_by_extension && extensions[0] != 0)
    remainder(0, result, extensions[0]);
  return result;
}
poly DPoly::mult_n(int level, const poly f, const poly g, bool reduce_by_extension)
{
  if (f == 0 || g == 0) return 0;
  poly result = alloc_poly_n(f->deg + g->deg);

  for (int i=0; i<=f->deg; i++)
    {
      poly a = f->arr.polys[i];
      if (a != 0)
	for (int j=0; j<=g->deg; j++)
	  {
	    poly b = g->arr.polys[j];
	    poly c = mult(level-1,a,b, true);
	    if (c != 0)
	      {
		add_in_place(level-1,result->arr.polys[i+j], c);
		dealloc_poly(c);
	      }
	  }
    }

  if (reduce_by_extension && extensions[level] != 0)
    remainder(level, result, extensions[level]);
  return result;
}
poly DPoly::mult(int level, const poly f, const poly g, bool reduce_by_extension)
{
  if (level == 0)
    return mult_0(f, g, reduce_by_extension);
  return mult_n(level, f, g, reduce_by_extension);
}

poly  DPoly::invert(int level, const poly a)
{
  // plan: compute the extended gcd of a and extensions[level]
  //   as univariate polynomials (at level 'level').
  // either return 0, if the gcd returned was not 1, or return 
  // result_u.
  poly u, v;
  poly g = gcd_coefficients(level, a, extensions[level], u, v);
  if (!is_one(level,g))
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
    for (int i=0; i<= deg; i++)
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
  else if (!is_one(level-1, b))
    for (int i=0; i<= deg; i++)
      {
	if (*p != 0) *p = mult(level-1, *p, b, true);
	p++;
      }
}
void DPoly::make_monic_0(poly & f, long &result_multiplier)
{
  if (f == 0) return;
  long *p = f->arr.ints;
  long a = p[f->deg];
  long b;
  ZZp_INVERT(charac, b, a);
  mult_by_coeff_0(f, b);
  result_multiplier = b;
}
void DPoly::make_monic_n(int level, poly & f, poly &result_multiplier)
{
  if (f == 0) return;
  poly *p = f->arr.polys;
  poly a = p[f->deg];
  poly b = invert(level-1,a);
  mult_by_coeff_n(level, f, b);
  result_multiplier = b;
}

void DPoly::make_monic(int level, poly & f)
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

poly  DPoly::division_in_place_monic(int level, poly & f, const poly g)
{
  // ASSUMPTION: g is MONIC
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
      for (int d=f->deg; shift>=0; d--, shift--)
	{
	  long a = p[d];
	  if (a != 0)
	    {
	      quot->arr.ints[shift] = a;
	      ZZp_NEGATE(charac, a);
	      for (int j=0; j<=g->deg; j++) 
		ZZp_APXY(charac, p[shift+j], a, q[j]);
	    }
	}
      bool iszero = true;
      for (int j=g->deg-1; j>=0; j--)
	if (p[j] != 0)
	  {
	    f->deg = j;
	    iszero = false;
	    break;
	  }
      if (iszero)
	{
	  dealloc_poly(f);
	}
    }
  else
    {
      poly *p = f->arr.polys;
      poly *q = g->arr.polys;
      for (int d=f->deg; shift>=0; d--, shift--)
	{
	  poly a = p[d];
	  if (a != 0)
	    {
	      quot->arr.polys[shift] = copy(level-1,a);
	      // MES: negate a...  maybe a should be the copy?
	      for (int j=0; j<=g->deg; j++) 
		{
		  poly b = mult(level-1,a,q[j],true);
		  add_in_place(level-1,p[j+shift],b);
		}
	    }
	}
      // Now set the degree MES
    }
  return quot;
}
bool  DPoly::division_in_place(int level, poly & f, const poly g, poly & result_quot)
// returns false if the lead coeff is not invertible
{
  if (f == 0) return 0;
  if (f->deg < g->deg)
    {
      result_quot = 0;
      return true;
    }
  int shift = f->deg - g->deg;
  result_quot = alloc_poly_n(shift);
  
  if (level == 0)
    {
      long *p = f->arr.ints;
      long *q = g->arr.ints;
      long leadcoeff = q[g->deg];
      long invlead = 1;
      if (leadcoeff != 1)
	{
	  ZZp_INVERT(charac, invlead, leadcoeff);
	}
      for (int d=f->deg; shift>=0; d--, shift--)
	{
	  long a = p[d];
	  if (a != 0)
	    {
	      ZZp_MULT(charac, a, invlead);
	      result_quot->arr.ints[shift] = a;
	      ZZp_NEGATE(charac, a);
	      for (int j=0; j<=g->deg; j++) 
		ZZp_APXY(charac, p[shift+j], a, q[j]);
	    }
	}
      bool iszero = true;
      for (int j=g->deg-1; j>=0; j--)
	if (p[j] != 0)
	  {
	    f->deg = j;
	    iszero = false;
	    break;
	  }
      if (iszero)
	{
	  dealloc_poly(f);
	}
      return true;
    }
  else
    {
      poly *p = f->arr.polys;
      poly *q = g->arr.polys;
      poly leadcoeff = q[g->deg];
      poly invlead;
      if (is_one(level-1,leadcoeff))
	invlead = leadcoeff;
      else
	{
	  invlead = invert(level-1,leadcoeff);
	  if (invlead == 0)
	    return false;
	}
      for (int d=f->deg; shift>=0; d--, shift--)
	{
	  poly a = p[d];
	  if (a != 0)
	    {
	      poly b = mult(level-1,invlead,a,true);
	      result_quot->arr.polys[shift] = copy(level-1,b);
	      for (int j=0; j<=g->deg; j++) 
		{
		  b = mult(level-1,a,q[j],true);
		  subtract_in_place(level-1,p[j+shift],b);
		}
	    }
	}
      bool iszero = true;
      for (int j=g->deg-1; j>=0; j--)
	if (p[j] != 0)
	  {
	    f->deg = j;
	    iszero = false;
	    break;
	  }
      if (iszero)
	{
	  dealloc_poly(f);
	}
      return true;
    }
}

void DPoly::remainder(int level, poly & f, const poly g)
{
  poly quot = division_in_place_monic(level, f, g);
  dealloc_poly(quot);
}

static void swap_poly(poly &f, poly &g)
{
  poly a = f;
  f = g;
  g = a;
}
poly  DPoly::gcd(int level, const poly f, const poly g)
{
  poly F = copy(level,f);
  poly G = copy(level,g);
  for (;;)
    {
#ifdef DEBUG
      printf("G = %s\n", to_string(level,G));
#endif
      make_monic(level,G);
      if (G == 0) return 0; // failed

#ifdef DEBUG
      printf("monic G = %s\n", to_string(level,G));
      printf("F       = %s\n", to_string(level,F));
#endif

      remainder(level, F, G); // modifies F
      if (F == 0) return G;

#ifdef DEBUG
      printf("F mod G     = %s\n", to_string(level,F));
#endif

      swap_poly(F,G);
    }
}

poly  DPoly::gcd_coefficients(int level, const poly f, const poly g, 
					  poly &result_u, poly &result_v)
{
  // Assumption:
  //  f and g are non-zero
  poly v1,v2,v3;
  poly u1,u2,u3;
  poly q;

  v1 = 0;
  v2 = from_int(level,1);
  v3 = copy(level,g);

  u1 = from_int(level,1);
  u2 = 0;
  u3 = copy(level,f);

  // At the end of the loop:
  // u1 * f + u2 * g == u3
  // v1 * f + v2 * g == v3
  // (and (v1,v2,v3) is close to the gcd

#ifdef DEBUG
  printf("u1 = %s\n", to_string(level,u1));
  printf("u2 = %s\n", to_string(level,u2));
  printf("u3 = %s\n", to_string(level,u3));

  printf("v1 = %s\n", to_string(level,v1));
  printf("v2 = %s\n", to_string(level,v2));
  printf("v3 = %s\n", to_string(level,v3));
#endif

  while (v3 != 0)  // MES: add in correct condition!!
    {
      if (!division_in_place(level, u3, v3, q)) // u3 := u3 - q*v3, true returned if division possible
	{
	  return 0;
	}

#ifdef DEBUG
      printf("q = %s\n", to_string(level,q));
      printf("u3 = %s\n", to_string(level,u3));
#endif
      // this last line can fail, if lc(v3) is not monic.  In this case
      // we need to return 0.
      negate_in_place(level,q);
      poly a = mult(level, q, v1, false);
      poly b = mult(level, q, v2, false);
      add_in_place(level,u1,a);
      add_in_place(level,u2,b);
#ifdef DEBUG
      printf("u1 = %s\n", to_string(level,u1));
      printf("u2 = %s\n", to_string(level,u2));
      printf("u3 = %s\n", to_string(level,u3));
#endif
      dealloc_poly(a); // MES: totally wipeout polys a, b here!
      dealloc_poly(b);
      swap_poly(u1,v1);
      swap_poly(u2,v2);
      swap_poly(u3,v3);
    }

#ifdef DEBUG
  printf("u1 = %s\n", to_string(level,u1));
  printf("u2 = %s\n", to_string(level,u2));
  printf("u3 = %s\n", to_string(level,u3));
#endif

  // make monic
  if (level == 0)
    {
      long c = 0;
      make_monic_0(u3, c);
      mult_by_coeff_0(u1, c);
      mult_by_coeff_0(u2, c);
    }
  else
    {
      poly c = 0;
      make_monic_n(level, u3, c);
      mult_by_coeff_n(level, u1, c);
      mult_by_coeff_n(level, u2, c);
      dealloc_poly(c);
    }

  result_u = u1 ;
  result_v = u2;
  return u3;
}

DRing::DRing(long charac, int nvars, const_poly *exts)
  : level(nvars-1),
    D(charac, nvars, exts),
    P(charac)
{
}

DRing * DRing::create(long p, int nvars0, const_poly *ext0)
{
  return new DRing(p, nvars0, ext0);
}

void DRing::set_from_int(poly &result, mpz_ptr r)
{
  mpz_t a;
  mpz_init(a);
  mpz_mod_ui(a, r, P);
  long c = mpz_get_si(a);
  mpz_clear(a);
  if (c < 0) c += P;
  result = D.from_int(level, c);
}

bool DRing::set_from_rational(poly &result, mpq_ptr r) 
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
  
  result = D.from_int(level, ctop);
  return true;
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

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
