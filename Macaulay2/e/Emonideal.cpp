// Copyright 1999 Michael E. Stillman

#include "Emonideal.hpp"
#include "bin_io.hpp"
#include "text_io.hpp"

extern int comp_printlevel;

stash *EMonomialIdeal::mystash;

#if 0
EMonomialLookupTable::EMonomialLookupTable(const Ring *R, queue<Bag *> &elems, queue<Bag *> &rejects)
  : obj(new EMonomialLookupTable_rec(R))
{
  array< queue<Bag *> *> bins;
  Bag *b, *b1;
  while (elems.remove(b)) 
    {
      int d = varpower::simple_degree(b->monom().raw());
      if (d >= bins.length())
	for (int i=bins.length(); i<=d; i++)
	  bins.append((queue<Bag *> *)NULL);
      if (bins[d] == (queue<Bag *> *)NULL)
	bins[d] = new queue<Bag *>;
      bins[d]->insert(b);
    }
  int n = get_ring()->n_vars();
  intarray exp;
  for (int i=0; i < bins.length(); i++)
    if (bins[i] != NULL)
      {
	while (bins[i]->remove(b))
	  {
	    const int *mon = b->monom().raw();
	    exp.shrink(0);
	    varpower::to_ntuple(n, mon, exp);
	    if (search_expvector(exp.raw(), b1))
	      rejects.insert(b);
	    else
	      insert_minimal(b);
	  }
	delete bins[i];
      }
}

EMonomialLookupTable::EMonomialLookupTable(const Ring *R, queue<Bag *> &elems)
  : obj(new EMonomialLookupTable_rec(R))
{
  array< queue<Bag *> *> bins;
  Bag *b;
  while (elems.remove(b)) 
    {
      int d = varpower::simple_degree(b->monom().raw());
      if (d >= bins.length())
	for (int i=bins.length(); i<=d; i++)
	  bins.append((queue<Bag *> *)NULL);
      if (bins[d] == (queue<Bag *> *)NULL)
	bins[d] = new queue<Bag *>;
      bins[d]->insert(b);
    }
  for (int i=0; i < bins.length(); i++)
    if (bins[i] != NULL)
      {
	while (bins[i]->remove(b)) insert(b);
	delete bins[i];
      }
}
EMonomialLookupTable::EMonomialLookupTable(const Ring *R)
  : obj(new EMonomialLookupTable_rec(R))
{
}
#endif

EMonomialIdeal::EMonomialIdeal(const EMatrix *m, int r)
{
  // MES
}

EMatrix *EMonomialIdeal::toMatrix() const
{
  // MES
}

EMonomialIdeal *EMonomialIdeal::copy() const
{
  EMonomialLookupTable *result = new EMonomialLookupTable();
  for (EMonomialLookupTable::iterator i = mi->first(); i.valid(); ++i)
    result->insert_minimal(new Bag(*i));
  return new EMonomialIdeal(R,result);
}
#if 0
bool MonomialIdeal::is_equal(const MonomialIdeal &mi) const
{
  if (this == &mi) return true;
  if (length() != mi.length()) return false;
  Index<MonomialIdeal> i = first();
  Index<MonomialIdeal> j = mi.first();
  while (i.valid())
    {
      const int *m = operator[](i)->monom().raw();
      const int *n = mi[j]->monom().raw();
      if (!varpower::is_equal(m, n))
	return false;
      i++;
      j++;
    }
  return true;
}

MonomialIdeal MonomialIdeal::intersect(const MonomialIdeal &J) const
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    for (Index<MonomialIdeal> j = J.first(); j.valid(); j++)
      {
	Bag *b = new Bag(operator[](i)->basis_elem());
	varpower::lcm(operator[](i)->monom().raw(), 
		      J[j]->monom().raw(), b->monom());
	new_elems.insert(b);
      }
  MonomialIdeal result(get_ring(), new_elems);
  return result;
}

MonomialIdeal MonomialIdeal::intersect(const int *m) const
    // Compute (this : m), where m is a varpower monomial.
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = new Bag(operator[](i)->basis_elem());
      varpower::lcm(operator[](i)->monom().raw(), m, b->monom());
      new_elems.insert(b);
    }
  MonomialIdeal result(get_ring(), new_elems);
  return result;
}


MonomialIdeal MonomialIdeal::operator*(const MonomialIdeal &J) const
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    for (Index<MonomialIdeal> j = J.first(); j.valid(); j++)
      {
	Bag *b = new Bag(operator[](i)->basis_elem());
	varpower::mult(operator[](i)->monom().raw(), J[j]->monom().raw(),
		       b->monom());
	new_elems.insert(b);
      }
  MonomialIdeal result(get_ring(), new_elems);
  return result;
}

MonomialIdeal MonomialIdeal::operator+(const MonomialIdeal &J) const
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = new Bag(  operator[](i) );
      new_elems.insert(b);
    }
  for (Index<MonomialIdeal> j = J.first(); j.valid(); j++)
    {
      Bag *b = new Bag( J[j] );
      new_elems.insert(b);
    }
  MonomialIdeal result(get_ring(), new_elems);
  return result;
}

MonomialIdeal MonomialIdeal::operator-(const MonomialIdeal &J) const
    // Create the monomial ideal consisting of those elements of 'this'
    // that are not in 'J'.  The baggage is left the same.
{
  MonomialIdeal result(get_ring());
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *c;
      if (!J.search(operator[](i)->monom().raw(), c))
	  {
	    Bag *b = new Bag(  operator[](i) );
	    result.insert_minimal(b);
	  }
    }
  return result;
}

MonomialIdeal MonomialIdeal::quotient(const int *m) const
    // Compute (this : m), where m is a varpower monomial.
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = new Bag(operator[](i)->basis_elem());
      varpower::divide(operator[](i)->monom().raw(), m, b->monom());
      new_elems.insert(b);
    }
  MonomialIdeal result(get_ring(), new_elems);
  return result;
}

MonomialIdeal MonomialIdeal::quotient(const MonomialIdeal &J) const
{
  MonomialIdeal result(get_ring());
  Bag *b = new Bag(0);
  varpower::one(b->monom());
  result.insert(b);
  for (Index<MonomialIdeal> i = J.first(); i.valid(); i++)
    {
      MonomialIdeal result1 = quotient(operator[](i)->monom().raw());
      result = result.intersect(result1);
    }
  return result;
}

#if 0
MonomialIdeal MonomialIdeal::socle(const MonomialIdeal &J) const
{
  MonomialIdeal result(get_ring());
  for (Index<MonomialIdeal> i = J.first(); i.valid(); i++)
    {
      for (index_varpower j = operator[](i)->monom().raw(); j.valid(); j++)
	{
	  varpower::divide(...);
	  compute m/j
	  if xi (m/j) is in J for all xi
	    {
	      
	      Bag *b = new Bag(vp);
	      result.insert_minimal(b);
	    }
	}
    }
  return result;
}
#endif
MonomialIdeal MonomialIdeal::erase(const int *m) const
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = new Bag(operator[](i)->basis_elem());
      varpower::erase(operator[](i)->monom().raw(), m, b->monom());
      new_elems.insert(b);
    }
  MonomialIdeal result(get_ring(), new_elems);
  return result;
}

MonomialIdeal MonomialIdeal::sat(const MonomialIdeal &J) const
{
  MonomialIdeal result(get_ring());
  Bag *b = new Bag(0);
  varpower::one(b->monom());
  result.insert(b);
  for (Index<MonomialIdeal> i = J.first(); i.valid(); i++)
    {
      MonomialIdeal result1 = erase(operator[](i)->monom().raw());
      result = result.intersect(result1);
    }
  return result;
}

MonomialIdeal MonomialIdeal::radical() const
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = new Bag(  operator[](i)->basis_elem() );
      varpower::radical( operator[](i)->monom().raw(), b->monom() );
      new_elems.insert(b);
    }
  MonomialIdeal result(get_ring(), new_elems);
  return result;
}

static void borel1(queue<Bag *> &result, int *m, int loc, int nvars)
{
  if (loc == 0)
    {
      Bag *b = new Bag(0);
      varpower::from_ntuple(nvars, m, b->monom());
      result.insert(b);
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

MonomialIdeal MonomialIdeal::borel() const
    // Return the smallest borel monomial ideal containing 'this'.
{
  queue<Bag *> new_elems;
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = operator[](i);
      intarray bexp;
      varpower::to_ntuple(get_ring()->n_vars(), b->monom().raw(), bexp);
      borel1(new_elems, bexp.raw(),
	     get_ring()->n_vars()-1, get_ring()->n_vars());
    }
  MonomialIdeal result(get_ring(), new_elems);
  return result;
}

int MonomialIdeal::is_borel() const
{
  for (Index<MonomialIdeal> i = first(); i.valid(); i++)
    {
      Bag *b = operator[](i);
      Bag *c;
      intarray bexp;
      varpower::to_ntuple(get_ring()->n_vars(), b->monom().raw(), bexp);
      for (int j=get_ring()->n_vars()-1; j>=1; j--)
	if (bexp[j] > 0)
	  {
	    bexp[j]--;
	    bexp[j-1]++;
	    int isthere = search_expvector(bexp.raw(), c);
	    bexp[j]++;
	    bexp[j-1]--;
	    if (!isthere) return 0;
	  }
    }
  return 1;
}
#endif
// Other routines to add:
//   primary_decomposition(J)
//   partition(J)

//   linear versions of: quotient, ...
//   hilbert series

void EMonomialIdeal::text_out(buffer &o) const
{
#if 0
  const EMonoid *M = R->getMonoid();
  for (EMonomialLookupTable::iterator j = mi->last(); j.valid(); --j)
    {
      const monomial *m = M->from_varpower(j->monom());
      M->elem_text_out(o, m);
      if (comp_printlevel > 0)
	o << '(' << j->basis_elem() << ')';
      o << ' ';
    }
#endif
}
#if 0
EMatrix *EMonomialIdeal::toMatrix() const
{
  // Place the lead terms which are NOT lead terms in the base.
  // First: set up the matrix
  array< EMInode * > goods;

  for (EMonomialLookupTable::iterator j = mi->last(); j.valid(); --j)
    {
      const monomial *m = M->from_varpower(j->monom());
      // TEST HERE FOR m TO BE 'GOOD': MES
      goods.append(
      M->elem_text_out(o, m);
      if (comp_printlevel > 0)
	o << '(' << j->basis_elem() << ')';
      o << ' ';
    }
  
}

EMonomialIdeal::EMonomialIdeal(const EMatrix *m, int row)
{
}
#endif
