// Copyright 2000  Michael E. Stillman

#include "EEbasics.hpp"
#include "EElookup.hpp"
#include "ntuple.hpp"

EGBMemory::EGBMemory(const EInterface &I, const FreeModule *F, const FreeModule *Fsyz)
  : I(I)
{
  // set nvars;
  exponent_stash = new stash("exponents", nvars * sizeof(int));
  heuristicWeightVector = new_exponent_vector();
  for (int i=0; i<nvars; i++)
    heuristicWeightVector[i] = 1;
}

exponent_vector *EGBMemory::new_exponent_vector() const
{
  exponent_vector *result = (exponent_vector *) exponent_stash->new_elem();
  return result;
}
void EGBMemory::remove_exponent_vector(const exponent_vector *&a) const
{
  exponent_vector *exp = const_cast<int *>(a);
  exponent_stash->delete_elem(exp);
  const_cast<int *&>(a) = 0;
}


void EGBMemory::remove_ering_elem(ering_elem *&g)
  // This removes all fields of the ering_elem.
{
  remove_exponent_vector(g->_lcm);
  //  R->remove(g->f);  // We are just visiting these elements...
  // They should not go away from under us, as long as this class survives.
  delete g;
}

ering_elem *EGBMemory::make_ering_elem(ring_elem g) const
{
  int lo, hi;
  exponent_vector *exp = new_exponent_vector();

  I.to_exponents(I.lead_monomial_of_polynomial(g), exp);
  R->degree_weights(g,heuristicWeightVector,lo,hi);  // Use EInterface here??
  int indegree = ntuple::weight(nvars,exp,heuristicWeightVector);

  ering_elem *r = new ering_elem;
  r->_f = g; // not a copy -- don't free this!
  r->_lcm = exp;
  r->_degree = hi;
  r->_tdegree = hi - indegree;
  return r;
}

egb_elem *EGBMemory::make_egb_elem(int degree,
				  EVector &f, //grabs
				  EVector &fsyz, // grabs
				  ring_elem denom, // grabs
				  bool minimal) const
{
  egb_elem *result = new egb_elem;
  exponent_vector *exp = new_exponent_vector();
  I.to_exponents(I.lead_monomial(f), exp);

  int d = degree;
  d -= ntuple::weight(nvars,exp,heuristicWeightVector);
  d -= F_degree(I.lead_component(f));

  result->_degree = degree;
  result->_lcm = exp;
  result->_tdegree = d;
  result->_f = f;
  result->_fsyz = fsyz;
  result->_denom = denom;
  result->_is_minimal = minimal;
  return result;
}

void EGBMemory::remove_egb_elem(egb_elem *&g)
{
  remove_exponent_vector(g->_lcm);
  R->remove_vector(g->_f);
  R->remove_vector(g->_fsyz);
  R->remove(g->_denom);
  delete g;
  g = 0;
}





exponent_vector *EGBMemory::make_skew_lcm(const exponent_vector *exp,
					  int v,
					  int deg_of_exp,
					  int & result_degree) const
{
  result_degree = deg_of_exp + heuristicWeightVector[v];
  exponent_vector *result = new_exponent_vector();
  ntuple::copy(nvars, exp, result);
  result[v]++;
  return result;
}
exponent_vector *EGBMemory::make_lcm(const exponent_vector *exp1,
				     const exponent_vector *exp2,
				     int deg_of_exp1,
				     int deg_of_exp2,
				     int & result_degree) const
{
  exponent_vector *result = new_exponent_vector();
  int deg1 = deg_of_exp1;
  int deg2 = deg_of_exp2;
  for (int i=0; i<nvars; i++)
    {
      int a = exp1[i] - exp2[i];
      if (a == 0)
	{
	  result[i] = exp1[i];
	}
      else if (a > 0)
	{
	  result[i] = exp1[i];
	  deg2 += heuristicWeightVector[i] * a;
	}
      else
	{
	  result[i] = exp2[i];
	  deg1 += heuristicWeightVector[i] * (-a);
	}
    }
  if (deg1 > deg2)
    result_degree = deg1;
  else
    result_degree = deg2;
  return result;
}

es_pair *EGBMemory::make_ring_s_pair(egb_elem *p, ering_elem *r) const
{
  es_pair *result = new es_pair;
  result->next = NULL;
  result->_type = SP_RING;
  result->_first = p;
  result->_s._ringsyz = r;

  const exponent_vector *m1 = p->lead_exponents();
  const exponent_vector *m2 = r->lead_exponents();
  int deg2 = F_degree(I.lead_component(p->f())) + r->degree();
  result->_lcm = make_lcm(m1, m2, p->degree(), deg2, result->_degree);

  return result;
}

es_pair *EGBMemory::make_skew_s_pair(egb_elem *p, int v) const
{
  es_pair *result = new es_pair;
  result->next = NULL;
  result->_type = SP_SKEW;
  result->_first = p;
  result->_s._skewvar = v;

  result->_lcm = make_skew_lcm(p->lead_exponents(),
			       v,
			       p->degree(),
			       result->_degree);
  return result;
}

es_pair *EGBMemory::make_s_pair(egb_elem *a, egb_elem *b) const
{
  es_pair *result = new es_pair;
  result->next = NULL;
  result->_type = SP_SYZ;
  result->_first = a;
  result->_s._syz = b;

  result->_lcm = make_lcm(a->lead_exponents(),
			  b->lead_exponents(),
			  a->degree(),
			  b->degree(),
			  result->_degree); // sets result->degree.
  return result;
}

es_pair *EGBMemory::make_gen_pair(EVector &f, EVector &fsyz, ring_elem &denom) const
{
  int lo,hi;
  I.degree_lohi(heuristicWeightVector, F, f, lo, hi);
  int deg = hi;

  es_pair *result = new es_pair;
  result->next = NULL;
  result->_type = SP_GEN;
  result->_first = make_egb_elem(deg,f,fsyz,denom,true);

  result->_lcm = result->_first->lead_exponents();
  result->_degree = result->_first->degree();

  return result;
}

void EGBMemory::remove_pair(es_pair *& p)
{
  p->next = NULL;
  switch (p->_type) {
  case SP_RING:
  case SP_SKEW:
  case SP_SYZ:
    remove_exponent_vector(p->_lcm);
    break;
  case SP_GEN:
  case SP_DEFERRED:
    remove_egb_elem(p->_first);
    break;
  }

  delete p;
  p = NULL;
}

ERingTable *EGBMemory::create_ring_table() const
{
  // Loop through all quotient elements, make ering_elem's, and insert into the
  // result table.
  ERingTable *ring_table = new ERingTable(I);

  for (int i=0; i<I.n_quotients(); i++)
    {
      ering_elem *r = make_ering_elem(I.get_quotient_element(i));
      ring_table->insert(r);  // We are guaranteed that all the 'r' s are minimal.
    }
  return ring_table;
}
  
