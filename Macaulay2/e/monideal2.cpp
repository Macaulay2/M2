// Copyright 1999 by Michael E. Stillman

#include "monideal2.hpp"
#include "text_io.hpp"
#include "bin_io.hpp"

MonomialIIdeal *MonomialIIdeal::make(const Matrix &m, int component)
{
}

MonomialIIdeal *MonomialIIdeal::make(const Ring *R, 
			      vector<MonomialTable<int>::tagged_monomial *> &new_elems)
{
}
 
Matrix MonomialIIdeal::to_matrix() const
{
}

bool MonomialIIdeal::is_equal(const MonomialIIdeal *I) const
{
  if (this == I) return true;
  if (size() != I->size()) return false;
  MonomialTable<int>::iterator i = mi->first();
  MonomialTable<int>::iterator j = I->mi->first();
  while (i.valid())
    {
      if (!varpower::is_equal(i.get_monomial().raw(), j.get_monomial().raw()))
	return false;
      ++i;
      ++j;
    }
  return true;
}

void MonomialIIdeal::text_out(buffer &o) const
{
  const Monoid *M = R->Nmonoms();
  int *m = M->make_one();
  for (MonomialTable<int>::iterator j = mi->last(); j.valid(); --j)
    {
      const int *vp = j.get_monomial().raw();
      M->from_varpower(vp, m);
      M->elem_text_out(o, m);
      if (comp_printlevel > 0)
	o << '(' << j.get_tag() << ")";
      o << ' ';
    }
  M->remove(m);
}

void MonomialIIdeal::bin_out(buffer &o) const
{
  bin_int_out(o, size());
  const Monoid *M = R->Nmonoms();
  int *m = M->make_one();
  for (MonomialTable<int>::iterator j = mi->last(); j.valid(); --j)
    {
      const int *vp = j.get_monomial().raw();
      M->from_varpower(vp, m);
      M->elem_bin_out(o, m);
      bin_int_out(o, j.get_tag());
    }
  M->remove(m);
}

static void borel1(vector<MonomialTable<int>::tagged_monomial *> &result, 
		   int *m, int loc, int nvars)
{
  if (loc == 0)
    {
      MonomialTable<int>::tagged_monomial *b = new MonomialTable<int>::tagged_monomial(0);
      varpower::from_ntuple(nvars, m, b->mon);
      result.push_back(b);
    }
  else
    {
      int a = m[loc];
      for (int i=0; i<=a; i++)
	{
	  borel1(result,m,loc-1,nvars);
	  m[loc]--;
	  m[loc-1]++;
	}
      m[loc] += a + 1;
      m[loc-1] -= a + 1;
    }
}

MonomialIIdeal *MonomialIIdeal::borel() const
    // Return the smallest borel monomial ideal containing 'this'.
{
  vector<MonomialTable<int>::tagged_monomial *> new_elems;
  intarray bexp;
  for (MonomialTable<int>::iterator i = mi->first(); i.valid(); ++i)
    {
      bexp.shrink(0);
      varpower::to_ntuple(R->n_vars(), i.get_monomial().raw(), bexp);
      borel1(new_elems, bexp.raw(),
	     R->n_vars()-1, R->n_vars());
    }
  return MonomialIIdeal::make(R, new_elems);
}

bool MonomialIIdeal::is_borel() const
{
  int nvars = R->n_vars();
  intarray bexp(nvars);
  for (MonomialTable<int>::iterator i = mi->first(); i.valid(); ++i)
    {
      MonomialTable<int>::tagged_monomial *c;
      bexp.shrink(0);
      varpower::to_ntuple(nvars, i.get_monomial().raw(), bexp);
      for (int j=nvars-1; j>=1; j--)
	if (bexp[j] > 0)
	  {
	    bexp[j]--;
	    bexp[j-1]++;
	    bool isthere = mi->find_divisor(nvars, bexp.raw(), c);
	    bexp[j]++;
	    bexp[j-1]--;
	    if (!isthere) return false;
	  }
    }
  return true;
}

MonomialIIdeal *MonomialIIdeal::radical() const
{
}
MonomialIIdeal *MonomialIIdeal::add(MonomialIIdeal *I) const
{
}
MonomialIIdeal *MonomialIIdeal::mult(MonomialIIdeal *J) const
{
}
MonomialIIdeal *MonomialIIdeal::intersect(const int *m) const // m is a varpower monomial
{
}
MonomialIIdeal *MonomialIIdeal::quotient(const int *m) const // m is a varpower monomial
{
}
MonomialIIdeal *MonomialIIdeal::quotient(const MonomialIIdeal *J) const
{
}
MonomialIIdeal *MonomialIIdeal::erase(const int *m) const // m is a varpower monomial
{
}
MonomialIIdeal *MonomialIIdeal::sat(const MonomialIIdeal *J) const
{
}
MonomialIIdeal *MonomialIIdeal::intersect(const MonomialIIdeal *J) const
{
}
MonomialIIdeal *MonomialIIdeal::operator-(const MonomialIIdeal *F) const
{
}

