// Copyright 2005, Michael E. Stillman

#ifndef _qring_hpp_
#define _qring_hpp_

#include "ringelem.hpp"
#include <vector>

class PolyRing;
class FreeModule;
class MonomialIdeal;
class MonomialTable;
class MonomialTableZZ;
class gbvector;

class QRingInfo : public our_new_delete
{
  std::vector<Nterm *, gc_alloc> quotient_ideal;
  std::vector<gbvector *, gc_alloc> quotient_gbvectors;

protected:
  const PolyRing *R;
  bool overZZ_; // really: base is basic, ZZ, or frac ring

  int *MONOM1_;
  int *EXP1_, *EXP2_;

  void appendQuotientElement(Nterm *f, gbvector *g);
  QRingInfo(const PolyRing *R);

  virtual ~QRingInfo() {}
public:
  QRingInfo() : R(0), overZZ_(false), MONOM1_(0), EXP1_(0), EXP2_(0) {}

  int n_quotients() const { return quotient_ideal.size(); }
  Nterm * quotient_element(int i) const { return quotient_ideal[i]; }
  const gbvector * quotient_gbvector(int i) const { return quotient_gbvectors[i]; }

  virtual void normal_form(ring_elem &f) const { }

  virtual void normal_form(const FreeModule *F, gbvector *&f) const { }

  virtual const MonomialIdeal *  get_quotient_monomials() const { return 0; }
  // Each bag value is an "Nterm *".

  virtual const MonomialTable * get_quotient_MonomialTable() const { return 0; }
  // Each id is an index into quotient_ideal_

  virtual const MonomialTableZZ * get_quotient_MonomialTableZZ() const { return 0; }
  // Each id is an index into quotient_ideal_
};

class QRingInfo_field : public QRingInfo
{
  MonomialIdeal *Rideal;
  MonomialTable *ringtable;

  void reduce_lead_term_basic_field(Nterm * &f, const Nterm * g) const;
public:
  QRingInfo_field(const PolyRing *ambientR,
		  const std::vector<Nterm *, gc_alloc> &quotients);
  ~QRingInfo_field();

  void normal_form(ring_elem &f) const;

  void normal_form(const FreeModule *F, gbvector *&f) const;

  virtual const MonomialIdeal *  get_quotient_monomials() const { return Rideal; }
  // Each bag value is an "Nterm *".

  virtual const MonomialTable * get_quotient_MonomialTable() const { return ringtable; }
  // Each id is an index into quotient_ideal_
};

class QRingInfo_ZZ : public QRingInfo
{
  MonomialTableZZ *ringtableZZ;
  bool is_ZZ_quotient_;		// true if this is a quotient of a polynomial ring over ZZ, AND
				// there is an integer in the factored ideal.
  ring_elem ZZ_quotient_value_;	// This is the integer in the factor ideal, if is_ZZ_quotient is set.


  bool reduce_lead_term_ZZ(Nterm * &f, const Nterm * g) const;
public:
  QRingInfo_ZZ(const PolyRing *ambientR,
	       const std::vector<Nterm *, gc_alloc> &quotients);
  ~QRingInfo_ZZ();

  void normal_form(ring_elem &f) const;
  
  void normal_form(const FreeModule *F, gbvector *&f) const;

  virtual const MonomialTableZZ * get_quotient_MonomialTableZZ() const { return ringtableZZ; }
  // Each id is an index into quotient_ideal_
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
