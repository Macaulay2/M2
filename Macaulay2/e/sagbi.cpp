// Copyright 1997  Michael E. Stillman

#include "sagbi.hpp"

stash *sagbi_comp::mystash;

vec sagbi::subduct(const FreeModule *F,
		   vec f,
		   const RingMap &phi, 
		   gb_comp *J)
{
  vecterm head;
  vec result = &head;

  while (f != NULL)
    {
      vec g = f;
      f = f->next;
      g->next = NULL;

      Vector gv(F,F->copy(g));
      Vector junk;
      Vector g1v = J->reduce(gv, junk);
      vec g1 = g1v.get_value();
      
      // Is g1 a monomial in the new variables?
      if (F->in_subring(1,g1))
	{
	  g->next = f;
	  f = g;
	  vec phi_g1 = F->eval(phi,F,g1);
	  F->subtract_to(f, phi_g1);
	}
      else
	{
	  result->next = g;
	  result = g;
	}
    }

  result->next = NULL;
  return head.next;
}

Matrix sagbi::subduct(const Matrix &m, 
			const RingMap &phi,
			gb_comp *J)
{
  Matrix result(m.rows(), m.cols());

  for (int i=0; i<m.n_cols(); i++)
    result[i] = subduct(m.rows(), m.rows()->copy(m[i]), phi, J);
  return result;
}

sagbi_comp::sagbi_comp(const Matrix &m) : gb_comp(COMP_SAGBI) 
{
}

sagbi_comp::~sagbi_comp()
{
}

void sagbi_comp::enlarge(const Ring *R, int *wts)
{
}

void sagbi_comp::add_generators(const Matrix &m)
{
}

int sagbi_comp::calc(const int *deg, const intarray &stop_conditions)
{
return 0;
}

Matrix sagbi_comp::reduce(const Matrix &m, Matrix &lift)
{
return 0;
}

Vector sagbi_comp::reduce(const Vector &v, Vector &lift)
{
return 0;
}

int sagbi_comp::contains(const Matrix &m)
{
return 0;
}

bool sagbi_comp::is_equal(const gb_comp *q)
{
return 0;
}

// obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
Matrix sagbi_comp::min_gens_matrix()
{
return 0;
}

Matrix sagbi_comp::initial_matrix(int n)
{
return 0;
}

Matrix sagbi_comp::gb_matrix()
{
return 0;
}

Matrix sagbi_comp::change_matrix()
{
return 0;
}

Matrix sagbi_comp::syz_matrix()
{
return 0;
}

void sagbi_comp::stats() const
{
}
