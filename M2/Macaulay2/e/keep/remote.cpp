// Copyright 1995 Michael E. Stillman

#include "remote.hh"

remote_gb_comp::remote_gb_comp(Matrix &M, const char *mach, 
			       const char *user, 
			       const char *s, const char *cmd)
: m(M), comp(mach, user, s, cmd,
	     M.Ring_of()->charac(), 
	     M.Ring_of()->n_vars()),
  next_degree(0)
{
  // MES: anything else needed here?
}

void remote_gb_comp::encode_poly(const vec &v, int *& buf, int &len) const
{
  int n = m.rows()->n_terms(v);
  len = 2*n;
  buf = new int[len];
  int i = 0;
  int nvars = m.Ring_of()->n_vars();
  int *exp = new int[nvars];
  for (Nvecterm *p = v; p != NULL; p = p->next)
    {
      buf[i++] = p->coeff;
      m.Ring_of()->Nmonoms()->unpack(p->monom, exp);
//MES      buf[i++] = montable->btos(exp);
    }
  delete [] exp;
}

vec remote_gb_comp::decode_poly(int *, int) const
{
  return NULL;
}

void remote_gb_comp::send_matrix()
{
  for (int i=0; i<m.n_cols(); i++)
    {
      int len, *buf;
      encode_poly(m[i], buf, len);
      comp.send_generator((char *) buf, sizeof(int)*len);
    }
}

int remote_gb_comp::compute_next_degree()
{
  comp.compute_degree(next_degree);
  next_degree++;
  while (comp.working())
    comp.update_io(1);
  return comp.n_pairs() == 0;
}

void remote_gb_comp::stats(ostream &) const
{
  // MES: write this
}

Matrix remote_gb_comp::gens()
{
  // Loop through and receive each generator, don't forget to free char *!
  array<int> leads;
  Matrix result(m.rows());
  comp.lead_terms(leads);
  for (int i=0; i<leads.length(); i++)
    {
      int len;
      char *buf;
      comp.receive_generator(buf, len);
      len /= sizeof(int);
      vec v = decode_poly((int *)buf, len);
      delete [] buf;
      result.append(v);
    }
  return result;
}

int remote_gb_comp::n_pairs_left()
{
  return comp.n_pairs();
}

