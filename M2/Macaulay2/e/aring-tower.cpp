// Copyright 2012 Michael E. Stillman

#include "aring-tower.hpp"

namespace M2 {

ARingTower::~ARingTower()
{
  // TODO: write me.
  // needs to free the extension polynomials
}

// The main construction routine for tower rings
//  (1) names[i] is the name of the i-th variable, used solely for display.
//  (2) extensions is a std vector of length <= #variables (size of names
//  array).
//     The i-th element is a polynomial of level i (- <= i < mNumVars
//     (which allows NULL as a value too)
ARingTower::ARingTower(const BaseRingType &baseRing,
                       const std::vector<std::string> &names,
                       const std::vector<ElementType> &extensions)
    : mBaseRing(baseRing), mVarNames(names), mExtensions()
{
  assert(names.size() >= 1);
  mNumVars = static_cast<int>(names.size());
  mStartLevel = mNumVars - 1;

  // Now copy all of the extension polynomials
  assert(extensions.size() <= names.size());
  for (size_t i = 0; i < names.size(); i++)
    {
      if (extensions.size() < i)
        {
          // mExtensions.push_back(mRing.copy(i, extensions[i]));
        }
      else
        mExtensions.push_back(static_cast<ElementType>(NULL));
    }
}

const ARingTower *ARingTower::create(const ARingZZpFFPACK &baseRing,
                                     const std::vector<std::string> &names)
{
  std::vector<ElementType> extensions;
  return new ARingTower(baseRing, names, extensions);
}

const ARingTower *ARingTower::create(const ARingTower &R,
                                     const std::vector<std::string> &new_names)
{
  // TODO: write
  return 0;
}

const ARingTower *ARingTower::create(const ARingTower &R,
                                     const std::vector<ElementType> &extensions)
{
  // TODO: check that 'extensions' has the correct form for R.
  //  if not: throw an exception
  //  else:
  return new ARingTower(R.baseRing(), R.varNames(), extensions);
}

///////////////////
// Allocation /////
///////////////////
// TODO: what is the contract here???!!
ARingPolynomial ARingTower::alloc_poly_n(int deg) const
// if elems == 0, then set all coeffs to 0.
{
  ARingPolynomial result = new ARingPolynomialStruct;
  result->polys = new ARingPolynomial[deg + 1];
  result->deg = deg;
  result->len = deg + 1;
  for (int i = 0; i <= deg; i++) result->polys[i] = 0;
  return result;
}

ARingPolynomial ARingTower::alloc_poly_0(int deg) const
{
  ARingPolynomial result = new ARingPolynomialStruct;
  result->coeffs = new ARingZZpFFPACK::ElementType[deg + 1];
  result->deg = deg;
  result->len = deg + 1;
  for (int i = 0; i <= deg; i++) result->coeffs[i] = 0;
  return result;
}

void ARingTower::dealloc_poly(ARingPolynomial &f) const
// only f is freed, not any pointers in the array of f
{
  if (f == 0) return;
  delete[] f->polys;
  delete f;
  f = 0;
}

void ARingTower::clear(int level, ARingPolynomial &f) const
// free all space associated to f, set f to 0.
{
  if (f == 0) return;
  if (level == 0)
    {
      for (int i = 0; i <= f->deg; i++) mBaseRing.clear(f->coeffs[i]);
      delete[] f->coeffs;
    }
  else
    {
      for (int i = 0; i <= f->deg; i++) clear(level - 1, f->polys[i]);
      delete[] f->polys;
    }
  delete f;
  f = 0;
}

void ARingTower::reset_degree(ARingPolynomial &f) const
{
  if (f == 0) return;
  int fdeg = f->deg;
  for (int j = fdeg; j >= 0; --j)
    if (f->polys[j] != 0)
      {
        f->deg = j;
        return;
      }
  // at this point, everything is 0!
  dealloc_poly(f);  // sets f to 0
}

ARingPolynomial ARingTower::copy(int level, const ARingPolynomial f) const
{
  if (f == 0) return 0;
  ARingPolynomial result = alloc_poly_n(f->deg);
  if (level == 0)
    for (int i = 0; i <= f->deg; i++) result->coeffs[i] = f->coeffs[i];
  else
    for (int i = 0; i <= f->deg; i++)
      result->polys[i] = copy(level - 1, f->polys[i]);
  return result;
}

// TODO: should increase_capacity set the degree??  I don't think so...
void ARingTower::increase_capacity(int newdeg, ARingPolynomial &f) const
{
  assert(f != 0);
  if (f->len <= newdeg)
    {
      ARingPolynomial *newelems = newarray(ARingPolynomial, newdeg + 1);
      ARingPolynomial *fp = f->polys;
      for (int i = 0; i <= f->deg; i++) newelems[i] = fp[i];
      for (int i = f->deg + 1; i < newdeg + 1; i++) newelems[i] = 0;
      delete[] fp;
      f->polys = newelems;
      f->len = newdeg + 1;
      f->deg = newdeg;
    }
}

///////////////////
// Display ////////
///////////////////

void ARingTower::text_out(buffer &o) const
{
  o << "Tower[ZZ/" << characteristic() << "[";
  for (size_t i = 0; i < n_vars() - 1; i++) o << varNames()[i] << ",";
  if (n_vars() > 0) o << varNames()[n_vars() - 1];
  o << "]]";
  extensions_text_out(o);
}

void ARingTower::extensions_text_out(buffer &o) const
{
  for (int i = 0; i < mExtensions.size(); i++)
    {
      if (mExtensions[i] != 0)
        {
          o << newline << "    ";
          elem_text_out(o, i, mExtensions[i], true, false, false);
        }
    }
}

namespace {
  int n_nonzero_terms(int level, ARingPolynomial f)
  {
    if (f == 0) return 0;
    int nterms = 0;
    if (level == 0)
      {
        for (int i = 0; i <= f->deg; i++)
          if (f->coeffs[i] != 0) nterms++;
      }
    else
      {
        for (int i = 0; i <= f->deg; i++)
          if (f->polys[i] != 0) nterms++;
      }
    return nterms;
  }
};

bool ARingTower::is_one(int level, const ARingPolynomial f) const
{
  if (f == 0) return false;
  if (f->deg != 0) return false;
  if (level == 0)
    return 1 == f->coeffs[0];
  else
    return is_one(level - 1, f->polys[0]);
}

void ARingTower::elem_text_out(buffer &o,
                               int level,
                               const ARingPolynomial f,
                               bool p_one,
                               bool p_plus,
                               bool p_parens) const
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

  const std::string &this_varname = varNames()[level];

  if (level == 0)
    {
      bool firstterm = true;
      for (int i = f->deg; i >= 0; i--)
        if (f->coeffs[i] != 0)
          {
            if (!firstterm || p_plus) o << "+";
            firstterm = false;
            if (i == 0 || f->coeffs[i] != 1)
              mBaseRing.elem_text_out(o, f->coeffs[i], p_one, p_plus, p_parens);
            if (i > 0) o << this_varname;
            if (i > 1) o << i;
          }
      if (needs_parens) o << ")";
    }
  else
    {
      bool firstterm = true;
      for (int i = f->deg; i >= 0; i--)
        if (f->polys[i] != 0)
          {
            bool this_p_parens = p_parens || (i > 0);

            if (i == 0 || !is_one(level - 1, f->polys[i]))
              elem_text_out(o,
                            level - 1,
                            f->polys[i],
                            p_one,
                            p_plus || !firstterm,
                            this_p_parens);
            else if (p_plus || !firstterm)
              o << "+";
            if (i > 0) o << this_varname;
            if (i > 1) o << i;

            firstterm = false;
          }
      if (needs_parens) o << ")";
    }
}

ARingPolynomial ARingTower::var(int level, int v) const
// make the variable v (but at level 'level')
{
  if (v > level) return 0;
  int which = (v == 0 ? 1 : 0);
  ARingPolynomial result =
      alloc_poly_0(which);  // TODO: check that this initializes elements to 0
  result->coeffs[which] = 1;
  for (int i = 1; i <= level; i++)
    {
      which = (i == v ? 1 : 0);
      ARingPolynomial a = result;
      result = alloc_poly_n(which);
      result->polys[which] = a;
    }
  return result;
}

bool ARingTower::is_equal(int level, const ARingPolynomial f, const ARingPolynomial g) const
{
  if (f == 0)
    {
      if (g == 0) return true;
      return false;
    }
  if (g == 0 || f->deg != g->deg) return false;
  if (level == 0)
    {
      BaseCoefficientType *fp = f->coeffs;
      BaseCoefficientType *gp = g->coeffs;
      for (int i = 0; i <= f->deg; i++)
        if (fp[i] != gp[i]) return false;
      return true;
    }
  // level > 0
  ARingPolynomial *fp = f->polys;
  ARingPolynomial *gp = g->polys;
  for (int i = 0; i <= f->deg; i++)
    if (!is_equal(level - 1, fp[i], gp[i])) return false;
  return true;
}

void ARingTower::add_in_place(int level, ARingPolynomial &f, const ARingPolynomial g) const
{
  if (g == 0) return;
  if (f == 0)
    {
      f = copy(level, g);
      return;
    }
  int fdeg = f->deg;
  int gdeg = g->deg;

  increase_capacity(g->deg, f);
  if (level == 0)
    for (int i = 0; i <= gdeg; i++)
      mBaseRing.add(f->coeffs[i], f->coeffs[i], g->coeffs[i]);
  else
    for (int i = 0; i <= gdeg; i++)
      add_in_place(level - 1, f->polys[i], g->polys[i]);

  if (gdeg > fdeg)
    f->deg = gdeg;
  else if (gdeg == fdeg)
    reset_degree(f);
}

void ARingTower::negate_in_place(int level, ARingPolynomial &f) const
{
  if (f == 0) return;
  int deg = f->deg;
  if (level == 0)
    {
      for (int i = 0; i <= deg; i++)
        if (f->coeffs[i] != 0) mBaseRing.negate(f->coeffs[i], f->coeffs[i]);
    }
  else
    {
      for (int i = 0; i <= deg; i++)
        if (f->polys[i] != 0) negate_in_place(level - 1, f->polys[i]);
    }
}

void ARingTower::subtract_in_place(int level, ARingPolynomial &f, const ARingPolynomial g) const
{
  if (g == 0) return;
  if (f == 0)
    {
      f = copy(level, g);
      negate_in_place(level, f);
      return;
    }
  int fdeg = f->deg;
  int gdeg = g->deg;

  increase_capacity(g->deg, f);
  if (level == 0)
    for (int i = 0; i <= gdeg; i++)
      mBaseRing.subtract(f->coeffs[i], f->coeffs[i], g->coeffs[i]);
  else
    for (int i = 0; i <= gdeg; i++)
      subtract_in_place(level - 1, f->polys[i], g->polys[i]);

  if (gdeg > fdeg)
    f->deg = gdeg;
  else if (gdeg == fdeg)
    reset_degree(f);
}

void ARingTower::mult_by_coeff(int level,
                               ARingPolynomial &f,
                               const BaseCoefficientType &b) const
{
  assert(!mBaseRing.is_zero(b));
  if (f == 0) return;

  long deg = f->deg;
  if (level == 0)
    {
      for (int i = 0; i <= deg; i++)
        if (f->coeffs[i] != 0) mBaseRing.mult(f->coeffs[i], f->coeffs[i], b);
    }
  else
    {
      for (int i = 0; i <= deg; i++)
        if (f->polys[i] != 0) mult_by_coeff(level - 1, f->polys[i], b);
    }
}

void ARingTower::mult_by_coeff(ARingPolynomial &f, const BaseCoefficientType &b) const
{
  if (f == 0) return;
  if (mBaseRing.is_zero(b))
    {
      clear(f);
      return;
    }
  // TODO: add this line one is_one is implemented in ZZpFFPACK: if
  // (mBaseRing.is_one(b)) return;
  mult_by_coeff(mStartLevel, f, b);
}

};  // namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
