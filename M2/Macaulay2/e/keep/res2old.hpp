// Copyright 1997.  Michael E. Stillman

class res_comp_type : public type
{
public:

  static res_comp *make_res(Matrix &m, int length, int origsyz, int strategy);

//////////////////////////////////////////////
//  Performing the calculation ///////////////
//////////////////////////////////////////////

  virtual int calc(const int *DegreeLimit, const intarray &stop);

//////////////////////////////////////////////
//  Result matrices of the resolution ////////
//////////////////////////////////////////////

  virtual FreeModule *free_of(int i) const;
  virtual FreeModule *minimal_free_of(int i) const;
  virtual Matrix make(int i) const;
  virtual Matrix make_minimal(int i) const;

//////////////////////////////////////////////
//  Betti routines and numbers associated ////
//  with the resolution                   ////
//////////////////////////////////////////////
// Betti output is a flattened array of  /////
// length                                /////
// (high_degree() - low_degree() + 1)    /////
//    * (max_level() + 1)                /////
// The first row of the betti display is /////
// given first, then the second, etc     /////
//////////////////////////////////////////////

  virtual int n_pairs(int lev, int d) const;
  virtual int n_left(int lev, int d) const;
  virtual int n_minimal(int lev, int d) const;
  virtual int n_monoms(int lev, int d) const;

  virtual int low_degree() const;
  virtual int high_degree() const;
  virtual int max_level() const;
  virtual int regularity() const;

  void betti_skeleton(intarray &result) const;
  void betti_remaining(intarray &result) const;
  void betti_minimal(intarray &result) const;
  void betti_nmonoms(intarray &result) const;

  virtual void stats() const;

//////////////////////////////////////////////
//  Infrastructure ///////////////////////////
//////////////////////////////////////////////

public:  
  object_types type_of           () const { return TY_RES_COMP; }
  const char * type_name         () const { return "res computation"; }
  res_comp   * cast_to_res_comp  ()       { return this; }

  void bin_out(ostream &) const {}
  void text_out(ostream &o) const { o << "res_computation"; }

  int length_of() const { return max_level(); }
  object index_of(int i) { return make_minimal(i); }
};

class res2_comp : public res_comp
{
private:
  int n_nodes;
  gb_node **nodes;

  int lo_degree;
private:
  void setup(const Matrix &m, int csyz, int origsyz, int length, int strategy);
  
public:
  res2_comp(const Matrix &m, int length, int orig_syz, int strategy);
  res2_comp(const Matrix &m, int collect_syz, int orig_syz,
	  RingElement hf, int strategy);

  ~res2_comp();
  // Performing the computation
  int calc(const int *deg, const intarray &stop_conditions);
  int is_done();

  // reduction
  Matrix reduce(const Matrix &m, Matrix &lift);
  Vector reduce(const Vector &v, Vector &lift);

  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  Matrix min_gens_matrix();
  Matrix initial_matrix(int n);
  Matrix gb_matrix();
  Matrix change_matrix();
  Matrix syz_matrix();
  void stats() const;

  // infrastructure
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }


  res2_comp *cast_to_res2_comp() { return this; }
  void bin_out(ostream &) const {}
  const char * type_name         () const { return "res2"; }
  void text_out(ostream &o) const { o << "res2"; }

  int length_of() const { return nodes[1]->n_gb_elems(); }

};  
