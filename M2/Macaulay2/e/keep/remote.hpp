// Copyright 1995 Michael E. Stillman

#ifndef _remote_hh_
#define _remote_hh_

#include "object.hh"
#include "matrix.hh"
#include "remotegb.hh"

class remote_gb_comp : public object_element 
{
private:
  void encode_poly(const vec &v, int *&buf, int &len) const;
  vec decode_poly(int *buf, int len) const;
public:
  Matrix m;
  remote_gbde comp;
  int next_degree;

  remote_gb_comp(Matrix &M, const char *mach, 
		 const char *user, 
		 const char *s, const char *cmd);
  ~remote_gb_comp() {}

  void send_matrix();
  int compute_next_degree();
  Matrix gens();
  int n_pairs_left();
  
  void stats(ostream &o) const;


  object_types type_of         () const { return TY_REMOTE_GB_COMP; }
  const char * type_name       () const { return "remote_gb_comp"; }
  remote_gb_comp *cast_to_remote_gb_comp()   { return this; }
  const remote_gb_comp *cast_to_remote_gb_comp() const { return this; }
  
  int length_of() const { return ((remote_gbde)comp).n_basis_elements(); }

  void bin_out(ostream &) const {}

  void text_out(ostream &o) const 
    {
      stats(o);
    }
};

#endif
