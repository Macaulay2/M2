// Copyright 2005, Michael E. Stillman

#ifndef _qring_hpp_
#define _qring_hpp_

#include "style.hpp"
#include "ringelem.hpp"
#include <vector>

class PolyRing;
class FreeModule;
class MonomialIdeal;
class MonomialTable;
class MonomialTableZZ;
class gbvector;
class GBRing;

/**
 * \ingroup ringinfo
 */
class QRingInfo : public our_new_delete
{
  VECTOR(Nterm *) quotient_ideal;
  VECTOR(gbvector *) quotient_gbvectors;

 protected:
  const PolyRing *R;
  bool overZZ_;  // really: base is basic, ZZ, or frac ring

  size_t exp_size;    // byte size of temp exponents_t
  size_t monom_size;  // and monomials, to be allocated on stack

  void appendQuotientElement(Nterm *f, gbvector *g);
  QRingInfo(const PolyRing *R);

 public:
  QRingInfo() : R(0), overZZ_(false), exp_size(0), monom_size(0) {}
  virtual void destroy(GBRing *GR);
  virtual ~QRingInfo();

  int n_quotients() const { return INTSIZE(quotient_ideal); }
  Nterm *quotient_element(int i) const { return quotient_ideal[i]; }
  const gbvector *quotient_gbvector(int i) const
  {
    return quotient_gbvectors[i];
  }

  virtual void normal_form(ring_elem &f) const {}
  virtual void gbvector_normal_form(const FreeModule *F, gbvector *&f) const {}
  virtual void gbvector_normal_form(const FreeModule *F,
                                    gbvector *&f,
                                    bool use_denom,
                                    ring_elem &denom) const
  {
    gbvector_normal_form(F, f);
  }

  virtual const MonomialIdeal *get_quotient_monomials() const { return 0; }
  // Each bag value is an "Nterm *".

  virtual MonomialTable *get_quotient_MonomialTable() const { return 0; }
  // Each id is an index into quotient_ideal_

  virtual const MonomialTableZZ *get_quotient_MonomialTableZZ() const
  {
    return 0;
  }
  // Each id is an index into quotient_ideal_
};

/**
 * \ingroup ringinfo
 */
class QRingInfo_field : public QRingInfo
{
 protected:
  MonomialIdeal *Rideal;
  MonomialTable *ringtable;

 public:
  QRingInfo_field(const PolyRing *ambientR, const VECTOR(Nterm *) & quotients);
  void destroy(GBRing *GR);
  ~QRingInfo_field();

  virtual const MonomialIdeal *get_quotient_monomials() const { return Rideal; }
  // Each bag value is an "Nterm *".

  virtual MonomialTable *get_quotient_MonomialTable() const
  {
    return ringtable;
  }
  // Each id is an index into quotient_ideal_
};

/**
 * \ingroup ringinfo
 */
class QRingInfo_field_basic : public QRingInfo_field
{
  void reduce_lead_term_basic_field(Nterm *&f, const Nterm *g) const;

 public:
  QRingInfo_field_basic(const PolyRing *ambientR,
                        const VECTOR(Nterm *) & quotients);
  ~QRingInfo_field_basic();

  void normal_form(ring_elem &f) const;

  void gbvector_normal_form(const FreeModule *F, gbvector *&f) const;
};

/**
 * \ingroup ringinfo
 */
class QRingInfo_field_QQ : public QRingInfo_field
{
  void reduce_lead_term_QQ(Nterm *&f, const Nterm *g) const;

 public:
  QRingInfo_field_QQ(const PolyRing *ambientR,
                     const VECTOR(Nterm *) & quotients);
  ~QRingInfo_field_QQ();

  void normal_form(ring_elem &f) const;

  void gbvector_normal_form(const FreeModule *F, gbvector *&f) const;

  void gbvector_normal_form(const FreeModule *F,
                            gbvector *&f,
                            bool use_denom,
                            ring_elem &denom) const;
};

/**
 * \ingroup ringinfo
 */
class QRingInfo_ZZ : public QRingInfo
{
  MonomialTableZZ *ringtableZZ;
  bool is_ZZ_quotient_;  // true if this is a quotient of a polynomial ring over
                         // ZZ, AND
                         // there is an integer in the factored ideal.
  ring_elem ZZ_quotient_value_;  // This is the integer in the factor ideal, if
                                 // is_ZZ_quotient is set.

  bool reduce_lead_term_ZZ(Nterm *&f, const Nterm *g) const;

 public:
  QRingInfo_ZZ(const PolyRing *ambientR, const VECTOR(Nterm *) & quotients);
  void destroy(GBRing *GR);
  ~QRingInfo_ZZ();

  bool is_ZZ_quotient() const { return is_ZZ_quotient_; }
  ring_elem ZZ_quotient_value() const { return ZZ_quotient_value_; }
  void normal_form(ring_elem &f) const;

  void gbvector_normal_form(const FreeModule *F, gbvector *&f) const;

  virtual const MonomialTableZZ *get_quotient_MonomialTableZZ() const
  {
    return ringtableZZ;
  }
  // Each id is an index into quotient_ideal_
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
