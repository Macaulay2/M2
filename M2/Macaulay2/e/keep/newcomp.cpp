// Copyright 1996 Michael E. Stillman

#include "newcomp.hh"

Computation::Computation()
{
}

Computation::~Computation()
{
}

int Computation::do_all()
{
}

int Computation::do_degree(int d)
{
  if (initial_degree == NULL)
    {
      initial_degree = new int[1];
      if (!last->initial_degree(&d))
	{
	  *gError << "computation cannot determine initial degree";
	  delete [] initial_degree;
	  return COMP_ERROR;
	}
    }

  last_degree_shift = last->degree_shift();
  int resultcode = COMP_COMPUTING;
  for (int i=first; i<=d; i++)
    {
      resultcode = last->do_degree(i + last_degree_shift);
      if (resultcode != COMP_COMPUTING)
	return result_code;
    }
}

void Computation::set_stop_conditions(int *d, intarray &stops)
{
  
}

int Computation::status()
{
}

void Computation::stats()
{
}

