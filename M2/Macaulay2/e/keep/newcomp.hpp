// Copyright 1996 Michael E. Stillman

class ComputationNode
{
  int initial_deg;
  int deg_shift;

  virtual ~ComputationNode();

  virtual int set_finish_conditions(intarry &stops);
  int initial_degree() const;
  int degree_shift() const;

  virtual int receive_vector(vec &f);
  virtual int receive_component(index_type *x);

  virtual int do_degree(ComputationNode *out, int d);
  virtual int do_all(ComputationNode *out);

  virtual int status();
};

class MatrixNode : public ComputationNode
{
  virtual Matrix get_gens();
};

class GBNode : public MatrixNode
{
protected:
  int homog_type;		// TY_HOMOG, TY_HOMOG_HF, TY_INHOMOG, TY_LOCAL
  int collectsyz;		// TY_SYZ_NONE, TY_SYZ_GENS, TY_SYZ_MINGENS
  int nrows;			// number of rows to keep
  int is_skew() const;		// Is the multiplication skew commutative?
  int is_ideal;			// 1 if gcd=1 pairs can be ignored.
  int coeff_type;		// TY_FIELD       if base is a 'small' field
				// TY_BIG_FIELD   if base is a fraction field of a poly ring
				//                over Q
				// TY_Z           if base is Z
				// TY_Q           if base is Q
				// TY_POLY_FIELD  if base is a fraction field of a poly ring 
				//                over a small ring
public:
  virtual int expect(int n);
  virtual Matrix min_gens_matrix();
  virtual Matrix initial_matrix(int n);
  virtual Matrix gb_matrix();
  virtual Matrix change_matrix();

  virtual Matrix reduce(Matrix m);
  virtual Matrix reduce(Matrix m, Matrix &lifted);
  virtual Matrix reduce(Matrix m, Matrix &denoms, Matrix &lifted);
};

///////////////////////////////////////////////////////////////////

class EmitterNode : public ComputationNode
{
  EmitterNode(Matrix &m);
  ~EmitterNode();

  // Now we must repeat the ones from ComputationNode, since all
  // of these are re-implemented.
  int initial_degree() const;
  int degree_shift() const;

  virtual int receive_vector(vec &f);
  virtual int receive_component(index_type *x);

  virtual int do_degree(ComputationNode *out, int d);
  virtual int do_all(ComputationNode *out);

  virtual int status();
};

class CollectorNode : public MatrixNode
{
  // This is a node that just collects elements from the previous node
  CollectorNode(ComputationNode *prev);
  ~CollectorNode();

  int initial_degree() const;
  int degree_shift() const;

  virtual int receive_vector(vec &f);
  virtual int receive_component(index_type *x);

  virtual int do_degree(ComputationNode *out, int d);
  virtual int do_all(ComputationNode *out);

  virtual int status();
};

class SchreyerNode : public GBnode
{
};

/////////////////////////////////////////////////////////

class Computation
{
  int *first_deg;
  ComputationNode *last;

  array<GBNode *> gbnodes;
  
public:
  int do_all();
  int do_degree(int d);
  virtual void set_stop_conditions(int *d, intarray &stops);
  int status();
  void stats();

  int n_gb() { return gbnodes.length(); }
  Matrix get_gens(int r) { return gbnodes[r]->get_gens(); }
  Matrix get_gb(int r) { return gbnodes[r]->get_gb(); }
  Matrix get_change(int r) { return gbnodes[r]->get_gb(); }

  static Computation *
    res_computation(const Matrix &m, 
		    int length,
		    int *abs_top_deg,
		    int gb_at_last_step);

  static Computation *
    gbhomog_computation(const Matrix &m,
			int collsyz,
			int nsyz,
			int strat);

  static Computation *
    sres_computation(const Matrix &m,
		     int length,
		     int *abs_top_deg,
		     int strat);
};

class ComputationObject : public object_element
{
public:
  Computation *C;

  ComputationObject(Computation *C) : C(C) {}
  ~ComputationObject() {}

  object_types type_of    () const { return TY_COMP; }
  const char * type_name  () const { return "computation"; }
  ComputationObject * cast_to_Computation() { return this; }
  
  void bin_out(ostream &o) const { C->bin_out(o); }
  void text_out(ostream &o) const { C->text_out(o); }

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

class GB : public type
{
  // gbasis
  // change of basis matrix
  // monomial ideals
  // indication of minimal generators

  // pairs to compute
  // state

  // forceGB
  // createGB
  // reduction routine, lift routine
  // stats routine

  // receive_vector
  // prepare_for_degree
  // expect(n)

  // insert
  // newpairs
  // reduce
  //
};
