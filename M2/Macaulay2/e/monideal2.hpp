// Copyright 1999 by Michael E. Stillman
#ifndef _monideal2_hh_
#define _monideal2_hh_

#include <vector>
using namespace std;
#include "varpower.hpp"
#include "montable.hpp"
#include "matrix.hpp"

class MonomialIIdeal : public immutable_object
{
  const Ring *R;
  MonomialTable<int> *mi;
public:
  MonomialIIdeal(const Ring *R, MonomialTable<int> *table)
    : R(R), mi(mi) { bump_up(R); }

  virtual ~MonomialIIdeal() { bump_down(R); }

  static MonomialIIdeal *make(const Matrix &m, int component);
  static MonomialIIdeal *make(const Ring *R, 
			      vector<MonomialTable<int>::tagged_monomial *> &new_elems);
  Matrix to_matrix() const;

  const Ring * get_ring() const { return R; }
  const MonomialTable<int> * get_table() const { return mi; }
  int size() const { return mi->length(); }

  bool is_equal(const MonomialIIdeal *mi) const;
  MonomialIIdeal *radical() const;

  MonomialIIdeal *add(MonomialIIdeal *I) const;
  MonomialIIdeal *mult(MonomialIIdeal *J) const;
  MonomialIIdeal *intersect(const int *m) const; // m is a varpower monomial
  MonomialIIdeal *quotient(const int *m) const; // m is a varpower monomial
  MonomialIIdeal *quotient(const MonomialIIdeal *J) const;
  MonomialIIdeal *erase(const int *m) const; // m is a varpower monomial
  MonomialIIdeal *sat(const MonomialIIdeal *J) const;

  MonomialIIdeal *borel() const;
  bool is_borel() const;

  MonomialIIdeal *intersect(const MonomialIIdeal *J) const;
  MonomialIIdeal *operator-(const MonomialIIdeal *F) const;

  // Infrastructure
  class_identifier class_id() const { return CLASS_MonomialIIdeal; }
  type_identifier  type_id () const { return TY_MonomialIdeal; }
  const char * type_name   () const { return "MonomialIIdeal"; }

  MonomialIIdeal * cast_to_MonomialIIdeal() { return this; }
  const MonomialIIdeal * cast_to_MonomialIIdeal() const { return this; }

  void bin_out(buffer &o) const;
  void text_out(buffer &o) const;

  int length_of() const { return size(); }
};

#endif


