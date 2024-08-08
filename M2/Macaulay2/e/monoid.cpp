// Copyright 1996 Michael E. Stillman

#include "monoid.hpp"

#include <assert.h>
#include <string.h>

#include "ExponentList.hpp"
#include "ExponentVector.hpp"
#include "buffer.hpp"
#include "error.h"
#include "exceptions.hpp"
#include "overflow.hpp"
#include "polyring.hpp"
#include "util.hpp"

// TODO: remove this
#include "interface/monomial-ordering.h"

Monoid *Monoid::trivial_monoid = nullptr;

// ONLY to be called by PolyRing::get_trivial_poly_ring()
void Monoid::set_trivial_monoid_degree_ring(const PolynomialRing *DR)
{
  Monoid *M = get_trivial_monoid();
  M->mDegreeRing = DR;
  M->mDegreeMonoid = M;
}

Monoid::Monoid()
    : mDegreeMonoid(nullptr),  // will be set later
      mDegreeRing(nullptr),    // will be set later
      mo_(nullptr),
      monorder_(nullptr),
      mVariableCount(0),
      mVariableNames({}),
      mDegrees({}),
      mHeftVector({}),
      mHeftDegrees({}),
      exp_size(0),
      monomial_size_(0),
      monomial_bound_(0),
      first_weights_slot_(-1),
      n_invertible_vars_(0),
      n_before_component_(0),
      n_after_component_(0),
      component_up_(true),
      local_vars({}),
      overflow(nullptr)
{
}

Monoid::~Monoid() {}
Monoid *Monoid::get_trivial_monoid()
{
  if (trivial_monoid == nullptr) trivial_monoid = new Monoid;

  return trivial_monoid;
}

Monoid *Monoid::create(const MonomialOrdering *mo,
                       const PolynomialRing *deg_ring,
                       const std::vector<std::string> &names,
                       const std::vector<int> &degs,
                       const std::vector<int> &hefts)
{
  unsigned int nvars = rawNumberOfVariables(mo);
  unsigned int degrk = deg_ring->n_vars();
  if (degs.size() != nvars * degrk)
    {
      ERROR("degree list should be of length %d", nvars * degrk);
      return nullptr;
    }
  if (names.size() != nvars)
    {
      ERROR("expected %d variable names", nvars);
      return nullptr;
    }

  return new Monoid(mo, deg_ring, names, degs, hefts);
}

Monoid::Monoid(const MonomialOrdering *mo,
               const PolynomialRing *deg_ring,
               const std::vector<std::string> names,
               const std::vector<int> degs,
               const std::vector<int> hefts)
    : mDegreeMonoid(deg_ring->getMonoid()),
      mDegreeRing(deg_ring),
      mo_(mo),
      monorder_(nullptr),  // set below
      mVariableCount(rawNumberOfVariables(mo)),
      mVariableNames(names),
      mDegrees(degs),
      mHeftVector(hefts),
      mHeftDegrees({}),   // set below, except in the trivial case.
      exp_size(0),        // set below
      monomial_size_(0),  // set below
      monomial_bound_(0),
      first_weights_slot_(-1),  // set below
      n_invertible_vars_(0),    // set below
      n_before_component_(0),   // set below
      n_after_component_(0),    // set below
      component_up_(true),      // set below
      local_vars({}),           // set below
      overflow(nullptr)
// nslots: set below
{
  monorder_ = monomialOrderMake(mo);

  monomial_size_ = monorder_->nslots;
  n_before_component_ = monorder_->nslots_before_component;
  n_after_component_ = monomial_size_ - n_before_component_;
  component_up_ = monorder_->component_up;

  // Set nslots_
  int total = 0;
  for (int i = 0; i < monorder_->nblocks; i++)
    {
      total += monorder_->blocks[i].nslots;
      nslots_.push_back(total);
    }

  // Set first_weight_value_
  bool get_out = false;
  first_weights_slot_ = -1;
  for (int i = 0; i < monorder_->nblocks && !get_out; i++)
    {
      switch (monorder_->blocks[i].typ)
        {
          case MO_LEX:
          case MO_LEX2:
          case MO_LEX4:
          case MO_NC_LEX:
            get_out = true;
            break;
          case MO_REVLEX:
          case MO_LAURENT:
          case MO_LAURENT_REVLEX:
          case MO_GREVLEX:
          case MO_GREVLEX2:
          case MO_GREVLEX4:
          case MO_GREVLEX_WTS:
          case MO_GREVLEX2_WTS:
          case MO_GREVLEX4_WTS:
          case MO_WEIGHTS:
            first_weights_slot_ = 0;
          case MO_POSITION_UP:
            continue;
          case MO_POSITION_DOWN:
            continue;
          default:
            INTERNAL_ERROR("monomial order block type not handled");
        }
    }
  exp_size = EXPONENT_BYTE_SIZE(mVariableCount);

  n_invertible_vars_ = rawNumberOfInvertibleVariables(mo_);

  set_degrees();
  set_overflow_flags();

  local_vars = M2_arrayint_to_stdvector<int>(rawNonTermOrderVariables(mo));

  for (int i=0; i<n_vars(); ++i)
    {
      bool isLaurent = isLaurentVariable(i);
      mLaurentVariablesPredicate.push_back(isLaurent);
    }

  // Debugging only:
  //  fprintf(stderr, "%d variables < 1\n", local_vars->len);
  //  if (local_vars->len > 0)
  //    {
  //      fprintf(stderr, "they are: ");
  //      for (int i=0; i<local_vars->len; i++)
  //      fprintf(stderr, "%d ", local_vars->array[i]);
  //      fprintf(stderr, "\n");
  //    }
}

void Monoid::set_degrees()
{
  if (mDegreeMonoid == nullptr)
    {
      mDegreeOfVar.push_back(static_cast<const_monomial>(nullptr));
      return;
    }

  auto degrk = mDegreeMonoid->n_vars();
  auto *iter = mDegrees.data();

  if (mHeftVector.size() != degrk)
    {
      ERROR("internal error: mHeftVector.size() == %d != degrk == %d",
            mHeftVector.size(),
            degrk);
      return;
    }
  if (degrk > 0)
    for (int i = 0; i < mVariableCount; i++)
      {
        auto d = exponents::weight(degrk, iter, mHeftVector);
        auto m = mDegreeMonoid->make_one();
        mDegreeMonoid->from_expvector(iter, m);
        mDegreeOfVar.push_back(m);
        mHeftDegrees.push_back(d);
        iter += degrk;
      }
  else
    {
      auto m = mDegreeMonoid->make_one();
      mDegreeOfVar.assign(mVariableCount, m);
      mHeftDegrees.assign(mVariableCount, 1);
    }
  // an extra entry for the degree of the zero element in the degree monoid
  mDegreeOfVar.push_back(mDegreeMonoid->make_one());
}

std::vector<int> Monoid::getFirstWeightVector() const
{
  std::vector<int> result;

  // grab the first weight vector
  if (getMonomialOrdering()->len > 0 and
      getMonomialOrdering()->array[0]->type == MO_WEIGHTS)
    {
      mon_part content = getMonomialOrdering()->array[0];
      std::copy(content->wts,
                content->wts + content->nvars,
                std::back_inserter(result));
      result.resize(n_vars());
    }
  return result;
}

std::vector<int> Monoid::getPrimaryDegreeVector() const
{
  return primary_degree_of_vars();
}

void Monoid::set_overflow_flags()
{
  overflow = newarray_atomic(enum overflow_type, monomial_size_);
  enum overflow_type flag;
  int i = 0, k = 0;
  for (; i < monorder_->nblocks; i++)
    {
      mo_block *b = &monorder_->blocks[i];
      switch (monorder_->blocks[i].typ)
        {
          case MO_REVLEX:
          case MO_WEIGHTS:
          case MO_LAURENT:
          case MO_LAURENT_REVLEX:
          case MO_NC_LEX:
            flag = OVER;
            goto fillin;
          case MO_POSITION_UP:
          case MO_POSITION_DOWN:
            ERROR(
                "internal error - MO_POSITION_DOWN or MO_POSITION_UP "
                "encountered");
            assert(0);
            break;
          case MO_LEX:
          case MO_GREVLEX:
          case MO_GREVLEX_WTS:
            flag = OVER1;
            goto fillin;
          case MO_LEX2:
          case MO_GREVLEX2:
          case MO_GREVLEX2_WTS:
            flag = OVER2;
            goto fillin;
          case MO_LEX4:
          case MO_GREVLEX4:
          case MO_GREVLEX4_WTS:
            flag = OVER4;
            goto fillin;
          fillin:
            assert(b->first_slot == k);
            for (int p = b->nslots; p > 0; p--)
              {
                assert(k < monomial_size_);
                overflow[k++] = flag;
              }
            break;
          default:
            ERROR("internal error - missing case");
            assert(0);
            break;
        }
    }
  assert(k == monomial_size_);
}

bool Monoid::primary_degrees_of_vars_positive() const
{
  for (int i = 0; i < mVariableCount; i++)
    if (mHeftDegrees[i] <= 0) return false;
  return true;
}

void Monoid::text_out(buffer &o) const
{
  int i;
  o << "[";
  for (i = 0; i < mVariableCount - 1; i++) o << mVariableNames[i] << ",";
  if (mVariableCount > 0) o << mVariableNames[mVariableCount - 1];

  int degrk = mDegreeMonoid->n_vars();
  o << "," << newline << "  DegreeLength => " << degrk;

  o << "," << newline << "  Degrees => {";
  for (i = 0; i < mVariableCount; i++)
    {
      if (i != 0) o << ", ";
      if (degrk != 1) o << '{';
      for (int j = 0; j < degrk; j++)
        {
          if (j != 0) o << ", ";
          o << mDegrees[i * degrk + j];
        }
      if (degrk != 1) o << '}';
    }
  o << "}";

  if (mHeftVector.size() != 0)
    {
      o << "," << newline << "  Heft => {";
      for (i = 0; i < mHeftVector.size(); i++)
        {
          if (i != 0) o << ", ";
          o << mHeftVector[i];
        }
      o << "}";
    }

  if (mo_ != nullptr)
    {
      o << "," << newline << "  ";
      o << IM2_MonomialOrdering_to_string(mo_);
    }

  o << newline << "  ]";
}

unsigned int Monoid::computeHashValue(const_monomial m) const
{
  unsigned int seed = 0x3124252;
  unsigned int hash = 0x43435728;
  int len = monomial_size();
  for (int i = 0; i < len; i++)
    {
      unsigned int val = *m++;
      hash += seed * val;
      seed = seed + 1342234;
    }
  return hash;
}

void Monoid::mult(const_monomial m, const_monomial n, monomial result) const
{
  overflow_type *t = overflow;
  for (int i = monomial_size_; i != 0; i--) switch (*t++)
      {
        case OVER:
          *result++ = safe::add(*m++, *n++);
          break;
        case OVER1:
          *result++ = safe::pos_add(*m++, *n++);
          break;
        case OVER2:
          *result++ = safe::pos_add_2(*m++, *n++);
          break;
        case OVER4:
          *result++ = safe::pos_add_4(*m++, *n++);
          break;
        default:
          throw(exc::internal_error("missing case"));
      }
}

int Monoid::num_parts() const { return monorder_->nblocks; }
int Monoid::n_slots(int nparts) const
{
  if (nparts == 0 or num_parts() == 0) return 0;
  nparts--;
  if (nparts < 0) return monomial_size();
  if (nparts >= num_parts()) nparts = num_parts() - 1;
  return nslots_[nparts];
}

bool Monoid::in_subring(int nslots, const_monomial m) const
{
  for (int i = 0; i < nslots; i++)
    if (*m++) return false;
  return true;
}

int Monoid::partial_compare(int num, const_exponents m, const_monomial n0) const
{
  if (num == 0) return EQ;
  int n[this->mVariableCount];
  to_expvector(n0, n);
  for (int i = 0; i < num; i++)
    if (m[i] != n[i]) return m[i] < n[i] ? LT : GT;
  return EQ;
}

int Monoid::compare(const_monomial m,
                    int mcomp,
                    const_monomial n,
                    int ncomp) const
{
  int i = n_before_component_;
  while (1)
    {
      if (i == 0) break;
      if (*m > *n) return GT;
      if (*m < *n) return LT;
      m++, n++;
      --i;
    }
  if (component_up_)
    {
      if (mcomp < ncomp) return LT;
      if (mcomp > ncomp) return GT;
    }
  else
    {
      if (mcomp < ncomp) return GT;
      if (mcomp > ncomp) return LT;
    }
  i = n_after_component_;
  while (1)
    {
      if (i == 0) break;
      if (*m > *n) return GT;
      if (*m < *n) return LT;
      m++, n++;
      --i;
    }
  return EQ;
}

monomial Monoid::make_new(const_monomial d) const
{
  if (mVariableCount == 0) return nullptr;
  monomial result = newarray_atomic(int, monomial_size());
  copy(d, result);
  return result;
}
monomial Monoid::make_one() const
{
  if (mVariableCount == 0) return nullptr;
  monomial result = newarray_atomic(int, monomial_size());
  one(result);
  return result;
}
void Monoid::remove(monomial d) const
{
#if 0
//   freemem(d);
#endif
}

void Monoid::one(monomial result) const
{
  for (int i = 0; i < monomial_size(); i++) *result++ = 0;
}

void Monoid::copy(const_monomial m, monomial result) const
{
  memcpy(result, m, monomial_size() * sizeof(int));
}

bool Monoid::divides_partial_order(const_monomial m, const_monomial n) const
// Is each exponent m_i <= n_i, for all i=0..nvars-1?
{
  if (mVariableCount == 0) return true;

  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents_t EXP2 = ALLOCATE_EXPONENTS(exp_size);
  // can we speed this up by not unpacking ??
  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  return exponents::divides(mVariableCount, EXP1, EXP2);
}

bool Monoid::divides(const_monomial m, const_monomial n) const
// Does m divide n?
{
  if (mVariableCount == 0) return true;

  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents_t EXP2 = ALLOCATE_EXPONENTS(exp_size);
  // can we speed this up by not unpacking ??
  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  if (numInvertibleVariables() == 0)
    return exponents::divides(mVariableCount, EXP1, EXP2);
  for (int i = 0; i < mVariableCount; ++i)
    if (not mLaurentVariablesPredicate[i] and EXP1[i] > EXP2[i])
      return false;
  return true;
}

void Monoid::power(const_monomial m, int n, monomial result) const
{
  if (mVariableCount == 0) return;

  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
  exponents::power(mVariableCount, EXP1, n, EXP1);
  from_expvector(EXP1, result);
}

void Monoid::monsyz(const_monomial m,
                    const_monomial n,
                    monomial sm,
                    monomial sn) const
{
  if (mVariableCount == 0) return;

  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents_t EXP2 = ALLOCATE_EXPONENTS(exp_size);

  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  for (int i = 0; i < mVariableCount; i++)
    if (EXP1[i] > EXP2[i])
      {
        EXP2[i] = EXP1[i] - EXP2[i];
        EXP1[i] = 0;
      }
    else
      {
        EXP1[i] = EXP2[i] - EXP1[i];
        EXP2[i] = 0;
      }
  from_expvector(EXP1, sm);
  from_expvector(EXP2, sn);
}

void Monoid::gcd(const_monomial m, const_monomial n, monomial p) const
{
  if (mVariableCount == 0) return;

  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents_t EXP2 = ALLOCATE_EXPONENTS(exp_size);

  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  exponents::gcd(mVariableCount, EXP1, EXP2, EXP1);
  from_expvector(EXP1, p);
}

void Monoid::lcm(const_monomial m, const_monomial n, monomial p) const
{
  if (mVariableCount == 0) return;

  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents_t EXP2 = ALLOCATE_EXPONENTS(exp_size);

  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  exponents::lcm(mVariableCount, EXP1, EXP2, EXP1);
  from_expvector(EXP1, p);
}

// TODO: replace buffer and use standard IO
void Monoid::elem_text_out(buffer &o, const_monomial m, bool p_one) const
{
  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
//   elem_text_out(o, EXP1, p_one);
// }
// void Monoid::elem_text_out(buffer &o, const_exponents EXP1, bool p_one) const
// {
  int len_ = 0;
  for (unsigned int v = 0; v < mVariableCount; v++)
    if (EXP1[v] != 0)
      {
        len_++;
        if (mVariableNames.size() < v)
          o << ".";
        else
          o << mVariableNames[v];
        int e = EXP1[v];
        int single = (mVariableNames[v].size() == 1);
        if (e > 1 && single)
          o << e;
        else if (e > 1)
          o << "^" << e;
        else if (e < 0)
          o << "^(" << e << ")";
      }
  if (len_ == 0 && p_one) o << "1";
}

// void Monoid::elem_text_out(buffer &o, const_varpower m, bool p_one) const
// {
//   index_varpower i = m;
//   if (!i.valid() and p_one) o << "1";
//   for (; i.valid(); ++i)
//     {
//       int v = i.var();
//       int e = i.exponent();
//       if (mVariableNames.size() < v)
//         o << ".";
//       else
//         o << mVariableNames[v];
//       int single = (mVariableNames[v].size() == 1);
//       if (e > 1 && single)
//         o << e;
//       else if (e > 1)
//         o << "^" << e;
//       else if (e < 0)
//         o << "^(" << e << ")";
//     }
// }

void Monoid::multi_degree(const_monomial m, monomial result) const
{
  if (mDegreeMonoid->n_vars() == 0) return;

  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);

  mDegreeMonoid->one(result);
  if (mVariableCount == 0) return;
  monomial mon1 = mDegreeMonoid->make_one();

  to_expvector(m, EXP1);

  for (int i = 0; i < mVariableCount; i++)
    if (EXP1[i] != 0)
      {
        mDegreeMonoid->power(mDegreeOfVar[i], EXP1[i], mon1);
        mDegreeMonoid->mult(result, mon1, result);
      }
  mDegreeMonoid->remove(mon1);
}

void Monoid::degree_of_varpower(const_varpower vp, monomial result) const
{
  if (mVariableCount == 0) return;
  if (mDegreeMonoid->n_vars() == 0) return;

  mDegreeMonoid->one(result);
  monomial mon1 = mDegreeMonoid->make_one();

  for (index_varpower j = vp; j.valid(); ++j)
    {
      int v = j.var();
      int e = j.exponent();
      mDegreeMonoid->power(mDegreeOfVar[v], e, mon1);
      mDegreeMonoid->mult(result, mon1, result);
    }
  mDegreeMonoid->remove(mon1);
}

int Monoid::primary_degree(const_monomial m) const
{
  return degree_weights(m, primary_degree_of_vars());
}

int Monoid::degree_weights(const_monomial m, const std::vector<int> &wts) const
{
  if (mVariableCount == 0) return 0;

  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
  int sz = (wts.size() < mVariableCount ? wts.size() : mVariableCount);
  return exponents::weight(sz, EXP1, wts);
}

// TODO: this doesn't have the same overflow check as the one above
// implement overflow check for int64_t and combine with the above
template<typename T>
T Monoid::degree_weights(const_monomial m, const std::vector<T>& wts) const
{
  if (mVariableCount == 0) return 0;

  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
  int sz = (wts.size() < mVariableCount ? wts.size() : mVariableCount);
  T wt = 0;
  for (int i=0; i<sz; i++)
    wt += EXP1[i] * wts[i];
  return wt;
}

template int Monoid::degree_weights<int>(const_monomial m, const std::vector<int>& wts) const;

int Monoid::simple_degree(const_monomial m) const
{
  if (mVariableCount == 0) return 0;

  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
  return exponents::simple_degree(mVariableCount, EXP1);
}

bool Monoid::is_one(const_monomial m) const
{
  for (int i = 0; i < monomial_size(); i++)
    if (*m++ != 0) return false;
  return true;
}

bool Monoid::is_invertible(const_monomial m) const
// is every variable that occurs
// in 'm' allowed to be negative?
{
  // Only the trivial monomial is invertible in this case
  if (n_invertible_vars_ == 0) { return is_one(m); }

  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
  for (int i = 0; i < mVariableCount; i++)
    if (!monorder_->is_laurent[i] && EXP1[i] > 0) return false;
  return true;
}

void Monoid::from_varpower(const_varpower vp, monomial result) const
{
  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  varpower::to_expvector(mVariableCount, vp, EXP1);
  from_expvector(EXP1, result);
}

void Monoid::to_varpower(const_monomial m, gc_vector<int>& result) const
{
  exponents_t EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
  varpower::from_expvector(mVariableCount, EXP1, result);
}

void Monoid::from_expvector(const_exponents exp, monomial result) const
{
  monomialOrderEncodeFromActualExponents(monorder_, exp, result);
}

void Monoid::to_expvector(const_monomial m, exponents_t result_exp) const
{
  monomialOrderDecodeToActualExponents(monorder_, m, result_exp);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e monoid.o "
// indent-tabs-mode: nil
// End:
