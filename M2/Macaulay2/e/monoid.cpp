// Copyright 1996 Michael E. Stillman

#include <ctype.h>
#include "util.hpp"
#include "text-io.hpp"
#include "monoid.hpp"

#include <assert.h>
#include <string.h>

#include "buffer.hpp"
#include "error.h"
#include "exceptions.hpp"
#include "interface/monomial-ordering.h"
#include "ntuple.hpp"
#include "overflow.hpp"
#include "polyring.hpp"
#include "varpower.hpp"

Monoid *Monoid::trivial_monoid = 0;

// ONLY to be called by PolyRing::get_trivial_poly_ring()
void Monoid::set_trivial_monoid_degree_ring(const PolynomialRing *DR)
{
  Monoid *M = get_trivial_monoid();
  M->degree_ring_ = DR;
  M->degree_monoid_ = M;
}

Monoid::Monoid()
    : nvars_(0),
      varnames_(nullptr),
      degvals_(nullptr),
      heftvals_(nullptr),
      heft_degree_of_var_(nullptr),
      degree_ring_(nullptr),    // will be set later
      degree_monoid_(nullptr),  // will be set later
      mo_(nullptr),
      monorder_(nullptr),
      overflow(0),
      exp_size(0),
      monomial_size_(0),
      monomial_bound_(0),
      n_invertible_vars_(0),
      n_before_component_(0),
      n_after_component_(0),
      component_up_(true),
      local_vars(nullptr),
      first_weights_slot_(-1)
{
}

Monoid::~Monoid() {}
Monoid *Monoid::get_trivial_monoid()
{
  if (trivial_monoid == 0) trivial_monoid = new Monoid;

  return trivial_monoid;
}

Monoid *Monoid::create(const MonomialOrdering *mo,
                       M2_ArrayString names,
                       const PolynomialRing *deg_ring,
                       M2_arrayint degs,
                       M2_arrayint hefts)
{
  unsigned int nvars = rawNumberOfVariables(mo);
  ;
  unsigned int eachdeg = deg_ring->n_vars();
  if (degs->len != nvars * eachdeg)
    {
      ERROR("degree list should be of length %d", nvars * eachdeg);
      return 0;
    }
  if (nvars != names->len)
    {
      ERROR("expected %d variable names", nvars);
      return 0;
    }

  return new Monoid(mo, names, deg_ring, degs, hefts);
}

Monoid *Monoid::create(const MonomialOrdering *mo,
                        const std::vector<std::string>& names,
                        const PolynomialRing *DR, /* degree ring */
                        const std::vector<int>& degs,
                        const std::vector<int>& hefts)
{
  
  return create(mo,
                toM2ArrayString(names),
                DR,
                stdvector_to_M2_arrayint(degs),
                stdvector_to_M2_arrayint(hefts));
}

Monoid::Monoid(const MonomialOrdering *mo,
               M2_ArrayString names,
               const PolynomialRing *deg_ring,
               M2_arrayint degs,
               M2_arrayint hefts)
    : nvars_(rawNumberOfVariables(mo)),
      varnames_(names),
      degvals_(degs),
      heftvals_(hefts),
      heft_degree_of_var_(nullptr), // set below, except in the trivial case.
      degree_ring_(deg_ring),
      degree_monoid_(deg_ring->getMonoid()),
      mo_(mo),
      monorder_(nullptr), // set below
      overflow(0),
      exp_size(0), // set below
      monomial_size_(0), // set below
      monomial_bound_(0),
      n_invertible_vars_(0), // set below
      n_before_component_(0), // set below
      n_after_component_(0), // set below
      component_up_(true), // set below
      local_vars(nullptr), // set below
      first_weights_slot_(-1) // set below
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
  exp_size = EXPONENT_BYTE_SIZE(nvars_);

  n_invertible_vars_ = rawNumberOfInvertibleVariables(mo_);

  set_degrees();
  set_overflow_flags();

  local_vars = rawNonTermOrderVariables(mo);

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
  if (degree_monoid_ == NULL)
    {
      degree_of_var_.push_back(static_cast<const_monomial>(NULL));
      return;
    }

  // Set 'degree_of_var
  int degvars = degree_monoid_->n_vars();
  int *t = degvals_->array;

  heft_degree_of_var_ = M2_makearrayint(nvars_);
  if (heftvals_->len != degvars)
    {
      ERROR("internal error: heftvals_->len == %d != degvars == %d",
            heftvals_->len,
            degvars);
      return;
    }
  if (degvars > 0)
    for (int i = 0; i < nvars_; i++)
      {
        monomial m = degree_monoid_->make_one();
        degree_monoid_->from_expvector(t, m);
        degree_of_var_.push_back(m);
        heft_degree_of_var_->array[i] = ntuple::weight(degvars, t, heftvals_);
        t += degvars;
      }
  else
    {
      for (int i = 0; i < nvars_; i++) heft_degree_of_var_->array[i] = 1;
    }
  degree_of_var_.push_back(degree_monoid_->make_one());
}

std::vector<int> Monoid::getFirstWeightVector() const
{
  std::vector<int> result;

  // grab the first weight vector
  if (getMonomialOrdering()->len > 0 and
      getMonomialOrdering()->array[0]->type == MO_WEIGHTS)
    {
      int i;
      result.reserve(n_vars());
      const int *wts = getMonomialOrdering()->array[0]->wts;
      for (i = 0; i < getMonomialOrdering()->array[0]->nvars; i++)
        result.push_back(wts[i]);
      for (; i < n_vars(); i++) result.push_back(0);
    }
  return result;
}

std::vector<int> Monoid::getPrimaryDegreeVector() const
{
  std::vector<int> result;

  M2_arrayint degs = primary_degree_of_vars();

  for (int i = 0; i < degs->len; i++) result.push_back(degs->array[i]);

  return result;
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
  for (int i = 0; i < nvars_; i++)
    if (primary_degree_of_var(i) <= 0) return false;
  return true;
}

void Monoid::text_out(buffer &o) const
{
  int i;
  o << "[";
  for (i = 0; i < nvars_ - 1; i++) o << varnames_->array[i] << ",";
  if (nvars_ > 0) o << varnames_->array[nvars_ - 1];

  int ndegrees = degree_monoid()->n_vars();
  o << "," << newline << "  DegreeLength => " << ndegrees;

  o << "," << newline << "  Degrees => {";
  for (i = 0; i < nvars_; i++)
    {
      if (i != 0) o << ", ";
      if (ndegrees != 1) o << '{';
      for (int j = 0; j < ndegrees; j++)
        {
          if (j != 0) o << ", ";
          o << degvals_->array[i * ndegrees + j];
        }
      if (ndegrees != 1) o << '}';
    }
  o << "}";

  if (heftvals_ != NULL)
    {
      o << "," << newline << "  Heft => {";
      for (i = 0; i < heftvals_->len; i++)
        {
          if (i != 0) o << ", ";
          o << heftvals_->array[i];
        }
      o << "}";
    }

  if (mo_ != 0)
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

void Monoid::from_expvector(const_exponents exp, monomial result) const
{
  monomialOrderEncodeFromActualExponents(monorder_, exp, result);
}

M2_arrayint Monoid::to_arrayint(const_monomial monom) const
{
  M2_arrayint result = M2_makearrayint(n_vars());
  to_expvector(monom, result->array);
  return result;
}

void Monoid::to_expvector(const_monomial m, exponents result_exp) const
{
  monomialOrderDecodeToActualExponents(monorder_, m, result_exp);
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
  int n[this->nvars_];
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
  if (nvars_ == 0) return NULL;
  monomial result = newarray_atomic(int, monomial_size());
  copy(d, result);
  return result;
}
monomial Monoid::make_one() const
{
  if (nvars_ == 0) return NULL;
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
  if (nvars_ == 0) return true;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);
  // can we speed this up by not unpacking ??
  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  return ntuple::divides(nvars_, EXP1, EXP2);
}

bool Monoid::divides(const_monomial m, const_monomial n) const
// Does m divide n?
{
  if (nvars_ == 0) return true;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);
  // can we speed this up by not unpacking ??
  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  if (numInvertibleVariables() == 0)
    return ntuple::divides(nvars_, EXP1, EXP2);
  for (int i=0; i < nvars_; ++i)
    if (not mLaurentVariablesPredicate[i] and EXP1[i] > EXP2[i])
      return false;
  return true;
}

void Monoid::power(const_monomial m, int n, monomial result) const
{
  if (nvars_ == 0) return;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
  ntuple::power(nvars_, EXP1, n, EXP1);
  from_expvector(EXP1, result);
}

void Monoid::monsyz(const_monomial m,
                    const_monomial n,
                    monomial sm,
                    monomial sn) const
{
  if (nvars_ == 0) return;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);

  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  for (int i = 0; i < nvars_; i++)
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
  if (nvars_ == 0) return;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);

  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  ntuple::gcd(nvars_, EXP1, EXP2, EXP1);
  from_expvector(EXP1, p);
}

void Monoid::lcm(const_monomial m, const_monomial n, monomial p) const
{
  if (nvars_ == 0) return;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);

  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  ntuple::lcm(nvars_, EXP1, EXP2, EXP1);
  from_expvector(EXP1, p);
}

void Monoid::elem_text_out(buffer &o, const_monomial m, bool p_one) const
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);

  to_expvector(m, EXP1);
  ntuple::elem_text_out(o, nvars_, EXP1, varnames_, p_one);
}

void Monoid::multi_degree(const_monomial m, monomial result) const
{
  if (degree_monoid()->n_vars() == 0) return;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);

  degree_monoid()->one(result);
  if (nvars_ == 0) return;
  monomial mon1 = degree_monoid()->make_one();

  to_expvector(m, EXP1);

  for (int i = 0; i < nvars_; i++)
    if (EXP1[i] != 0)
      {
        degree_monoid()->power(degree_of_var(i), EXP1[i], mon1);
        degree_monoid()->mult(result, mon1, result);
      }
  degree_monoid()->remove(mon1);
}

void Monoid::degree_of_varpower(const_varpower vp, monomial result) const
{
  if (nvars_ == 0) return;
  if (degree_monoid()->n_vars() == 0) return;

  degree_monoid()->one(result);
  monomial mon1 = degree_monoid()->make_one();

  for (index_varpower j = vp; j.valid(); ++j)
    {
      int v = j.var();
      int e = j.exponent();
      degree_monoid()->power(degree_of_var(v), e, mon1);
      degree_monoid()->mult(result, mon1, result);
    }
  degree_monoid()->remove(mon1);
}

int Monoid::primary_degree(const_monomial m) const
{
  return degree_weights(m, primary_degree_of_vars());
}

int Monoid::degree_weights(const_monomial m, M2_arrayint wts) const
{
  if (nvars_ == 0) return 0;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
  int sz = (wts->len < nvars_ ? wts->len : nvars_);
  return ntuple::weight(sz, EXP1, wts);
}

template<typename T>
T Monoid::degree_weights(const_monomial m, const std::vector<T>& wts) const
{
  if (nvars_ == 0) return 0;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
  int sz = (wts.size() < nvars_ ? wts.size() : nvars_);
  T wt = 0;
  for (int i=0; i<sz; i++)
    wt += EXP1[i] * wts[i];
  return wt;
}

template int Monoid::degree_weights<int>(const_monomial m, const std::vector<int>& wts) const;

int Monoid::simple_degree(const_monomial m) const
{
  if (nvars_ == 0) return 0;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
  return ntuple::degree(nvars_, EXP1);
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
  if (n_invertible_vars_ == 0)
    {
      // Only the trivial monomial is invertible in this case
      return is_one(m);
    }

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
  for (int i = 0; i < nvars_; i++)
    if (!monorder_->is_laurent[i] && EXP1[i] > 0) return false;
  return true;
}

void Monoid::from_varpower(const_varpower vp, monomial result) const
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  varpower::to_ntuple(nvars_, vp, EXP1);
  from_expvector(EXP1, result);
}

void Monoid::to_varpower(const_monomial m, intarray &result_vp) const
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  to_expvector(m, EXP1);
  varpower::from_ntuple(nvars_, EXP1, result_vp);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e monoid.o "
// indent-tabs-mode: nil
// End:
