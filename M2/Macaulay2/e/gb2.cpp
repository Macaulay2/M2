// Copyright 1997  Michael E. Stillman

#include "gb2.hpp"
#include "hilb.hpp"
#include "text_io.hpp"

extern ring_elem hilb(const Matrix &M, const Ring *RR);
extern int compare_type;

stash *gbres_comp::mystash;


gb_emitter::gb_emitter(const Matrix &m)
  : gens(m), g(NULL), n_left(m.n_cols()), n_i(0), n_in_degree(-1)
{
  this_degree = m.cols()->lowest_primary_degree() - 1;
  n_gens = 0;			// Also needs to be set at that time.
  these = new int[m.n_cols()];
}

gb_emitter::~gb_emitter()
{
  delete [] these;
}

int gb_emitter::calc_gb(int degree, const intarray & /*stop*/)
{
  // This is when we ship off the elements in this degree.  This should NEVER
  // be called if elements of lower degree have not been sent.
  if (this_degree != degree)
    {
      start_degree(degree);
    }
  for (;;)
    {
      if (system_interrupted)
	return COMP_INTERRUPTED;
      if (n_i >= n_gens) return COMP_DONE;
      if (g != NULL)
	g->receive_generator(gens.rows()->copy(gens[these[n_i]]), 
			     these[n_i]);
      n_i++;
      n_left--;
    }
}
int gb_emitter::calc_gens(int degree, const intarray &stop)
{
  return calc_gb(degree, stop);
}

int gb_emitter::start_degree(int deg)
{
  this_degree = deg;
  n_gens = 0;
  n_i = 0;
  for (int i=0; i<gens.n_cols(); i++)
    {
      if (gens.cols()->primary_degree(i) == this_degree)
	these[n_gens++] = i;
    }
  return n_gens;
}

void gb_emitter::flush()
{
  n_left -= (n_gens - n_i);
  n_gens = 0;
  n_i = 0;
}

bool gb_emitter::is_done()
{
  return (n_left == 0);
}
void gb_emitter::stats() const
{
}

typedef gb_node *gb_node_ptr;

void gbres_comp::setup(const Matrix &m, 
		     int length,
		     int origsyz,
		     int strategy)
{
  int i;
  const Ring *R = m.Ring_of()->cast_to_poly_ring();
  if (R == NULL) assert(0);

  FreeModule *Fsyz = R->make_FreeModule();
  if (length <= 0)
    {
      gError << "resolution length must be at least 1";
      length = 1;
    }
  if (length > 1 && origsyz > 0)
    {
      if (origsyz > m.n_cols())
	origsyz = m.n_cols();
      int *one = R->Nmonoms()->make_one();
      const int *mon;
      for (i=0; i<origsyz; i++)
	{
	  if (m[i] == NULL)
	    mon = one;
	  else
	    mon = m[i]->monom;
	  Fsyz->append(m.cols()->degree(i), mon, i);
	}
      R->Nmonoms()->remove(one);
    }

  lo_degree = m.cols()->lowest_primary_degree();

  n_nodes = length + 1;
  nodes = new gb_node_ptr[n_nodes];

  nodes[0] = new gb_emitter(m);
  nodes[1] = new gb2_comp(Fsyz,nodes[0],lo_degree,origsyz,1,strategy);
  nodes[0]->set_output(nodes[1]);
  if (n_nodes == 2)
    {
      // Don't compute syzygies at all.
      nodes[1]->set_output(NULL);
    }
  else if (n_nodes >= 3)
    {
      // Compute a resolution to length 'length', with last being
      // a gb node.
      int deg = lo_degree+1;
      if (origsyz > 0) deg--;
      for (i=2; i<n_nodes-1; i++)
	{
	  FreeModule *F = R->make_FreeModule();
	  nodes[i] = new gb2_comp(F,nodes[i-1],deg++,-1,i,strategy);
	  nodes[i-1]->set_output(nodes[i]);
	}
      FreeModule *F = R->make_FreeModule();
      nodes[n_nodes-1] = new gb2_comp(F,nodes[n_nodes-2],deg++,0,n_nodes-1,strategy);
      nodes[n_nodes-1]->set_output(NULL);      
    }
  strategy_flags = strategy;
}

gbres_comp::gbres_comp(const Matrix &m, int length, int origsyz, int strategy)
{
  setup(m,length,origsyz,strategy);
}

gbres_comp::gbres_comp(const Matrix &m, int length, int origsyz,
		 RingElement /*hf*/, int strategy)
{
  // MES: check homogeniety
  setup(m, length, origsyz, strategy);
  //nodes[0]->set_HF(hf);
}

gbres_comp::~gbres_comp()
{
  for (int i=0; i<n_nodes; i++)
    {
      nodes[i]->set_output(NULL);
      delete nodes[i];
    }
}


//---- state machine (roughly) for the computation ----

int gbres_comp::calc(const int *stop_degree, const intarray &stop)
{
  int ret;
  int old_compare_type = compare_type;
  compare_type = (strategy_flags >> 10);
  if (comp_printlevel >= 4) 
    {
      buffer o;
      o << "compare=" << compare_type << newline;
      emit(o.str());
    }
  for (int i=lo_degree; !is_done(); i++)
    {
      if (stop_degree != NULL && i > *stop_degree)
	{
	  ret = COMP_DONE_DEGREE_LIMIT;
	  break;
	}
      if (comp_printlevel >= 1)	
	{
	  buffer o;
	  o << "{" << i << "}";
	  emit(o.str());
	}
      ret = nodes[n_nodes-1]->calc_gens(i+n_nodes-3, stop);
      if (ret != COMP_DONE) break;
    }
  compare_type = old_compare_type;
  return COMP_DONE;
}

bool gbres_comp::is_done()
{
  for (int i=0; i<n_nodes; i++)
    if (!nodes[i]->is_done())
      return false;
  return true;
}
//--- Reduction --------------------------


Matrix gbres_comp::reduce(const Matrix &m, Matrix &lift)
{
  const FreeModule *F = nodes[0]->output_free_module();
  Matrix red(m.rows(), m.cols(), m.degree_shift());
  lift = Matrix(nodes[1]->output_free_module(), m.cols());
  if (m.n_rows() != F->rank()) {
       gError << "expected matrices to have same number of rows";
       return red;
  }
  for (int i=0; i<m.n_cols(); i++)
    {
      vec f = F->translate(m.rows(),m[i]);
      vec fsyz = NULL;

      nodes[1]->reduce(f, fsyz);
      nodes[1]->output_free_module()->negate_to(fsyz);
      red[i] = f;
      lift[i] = fsyz;
    }
  return red;
}

Vector gbres_comp::reduce(const Vector &v, Vector &lift)
{
  const FreeModule *F = nodes[0]->output_free_module();
  if (!v.free_of()->is_equal(F))
    {
      gError << "reduce: vector is in incorrect free module";
      return Vector(F, NULL);
    }
  vec f = F->copy(v.get_value());
  vec fsyz = NULL;

  nodes[1]->reduce(f, fsyz);
  nodes[1]->output_free_module()->negate_to(fsyz);

  lift = Vector(nodes[1]->output_free_module(), fsyz);
  return Vector(F, f);
}

//////////////////////////////////
// Obtaining matrices as output //
//////////////////////////////////

FreeModule *gbres_comp::free_module(int level)
{
  if (level >= 0 && level <= n_nodes-1)
    return nodes[level]->output_free_module();
  return nodes[0]->output_free_module()->Ring_of()->make_FreeModule();

}
Matrix gbres_comp::min_gens_matrix(int level)
{
  if (level <= 0 || level >= n_nodes)
    return Matrix(free_module(level-1), free_module(level));
  return nodes[level]->min_gens_matrix();
}
Matrix gbres_comp::get_matrix(int level)
{
  if (level <= 0 || level >= n_nodes)
    return Matrix(free_module(level-1), free_module(level));
  return nodes[level]->get_matrix();
}

Matrix gbres_comp::initial_matrix(int n, int level)
{
  if (level <= 0 || level >= n_nodes)
    return Matrix(free_module(level-1), free_module(level));
  return nodes[level]->initial_matrix(n);
}

Matrix gbres_comp::gb_matrix(int level)
{
  if (level <= 0 || level >= n_nodes)
    return Matrix(free_module(level-1), free_module(level));
  return nodes[level]->gb_matrix();
}

Matrix gbres_comp::change_matrix(int level)
{
  if (level <= 0 || level >= n_nodes)
    return Matrix(free_module(level-1), free_module(level));
  return nodes[level]->change_matrix();
}

void gbres_comp::stats() const
{
  emit_line("  #gb #pair #comp     m     z     o     u #hilb  #gcd #mons  #chg");
  for (int i=1; i<n_nodes; i++)
    nodes[i]->stats();
}

void gbres_comp::betti_minimal(intarray &result)
    // Negative numbers represent upper bounds
{
  int lev, i, j, d, k;
  int lo = nodes[0]->output_free_module()->lowest_primary_degree();
  int hi = lo;
  int len = 1;
  
  intarray *degs = new intarray[n_nodes];
  
  for (lev=0; lev<n_nodes; lev++)
    {
      const FreeModule *F = free_module(lev);
      if (F->rank() > 0)
	len = lev;

      for (i=0; i<F->rank(); i++)
	{
	  d = F->primary_degree(i) - lev;
	  if (d > hi) hi = d;
	  if (d-lo >= degs[lev].length())
	    {
	      for (k=degs[lev].length(); k<=d; k++)
		degs[lev].append(0);
	    }
	  degs[lev][d] += 1;
	}
    }
  for (lev=0; lev<=len; lev++)
    for (j=degs[lev].length(); j<=hi-lo; j++)
      degs[lev].append(0);

  result.append(lo);
  result.append(hi);
  result.append(len);
  for (d=lo; d<=hi; d++)
    for (lev=0; lev<=len; lev++)
      result.append(degs[lev][d]);

  delete [] degs;
}


/////////////////////////////////////////
// Commands for using this computation //
/////////////////////////////////////////

#include "interp.hpp"

void cmd_gbres_calc(object &op, object &odeg, object &oargs)
{
  gbres_comp *p = op->cast_to_gbres_comp();
  intarray *deg = odeg->intarray_of();
  intarray *args = oargs->intarray_of();
  if (args->length() != 6)
    {
      gError << "res: expected 5 elements in parameter array";
      return;
    }
  int *d;
  if (deg->length() == 0)
    d = NULL;
  else 
    d = deg->raw();

  //args[0] = LengthLimit
  //args[1] = SyzygyLimit
  //args[2] = PairLimit
  //args[3..5] = SyzygyLimit(nSyz, Level, Degree)
  gStack.insert(make_object_int(p->calc(d, *args)));
}
void cmd_gbres_stats(object &op)
{
  gbres_comp *p = op->cast_to_gbres_comp();
  p->stats();
}
void cmd_betti2_pairs(object & /*op*/)
{
#if 0
  gbres_comp *p = op->cast_to_gbres_comp();
  object_intarray *result = new object_intarray;
  //  p->betti_skeleton(*result->intarray_of());
  gStack.insert(result);
#endif
}
void cmd_betti2_remaining(object & /*op*/)
{
#if 0
  gbres_comp *p = op->cast_to_gbres_comp();
  object_intarray *result = new object_intarray;
  //  p->betti_remaining(*result->intarray_of());
  gStack.insert(result);
#endif
}
void cmd_betti2_minimal(object &op)
{
  gbres_comp *p = op->cast_to_gbres_comp();
  object_intarray *result = new object_intarray;
  p->betti_minimal(*result->intarray_of());
  gStack.insert(result);
}
void cmd_betti2_nmonoms(object & /*op*/)
{
#if 0
  gbres_comp *p = op->cast_to_gbres_comp();
  object_intarray *result = new object_intarray;
  //  p->betti_nmonoms(*result->intarray_of());
  gStack.insert(result);
#endif
}

void cmd_gbres_Nmap(object &op, object &on)
{
  gbres_comp *p = op->cast_to_gbres_comp();
  int n = on->int_of();
  gStack.insert(p->get_matrix(n));
}

void cmd_gbres_map(object &op, object &on)
{
  gbres_comp *p = op->cast_to_gbres_comp();
  int n = on->int_of();
  gStack.insert(p->get_matrix(n));
}

void cmd_gbres_Nmodule(object &op, object &on)
{
  gbres_comp *p = op->cast_to_gbres_comp();
  int n = on->int_of();
  gStack.insert(p->free_module(n));
}

void cmd_gbres_module(object &op, object &on)
{
  gbres_comp *p = op->cast_to_gbres_comp();
  int n = on->int_of();
  gStack.insert(p->free_module(n));
}
void cmd_gbres_mingens(object &op, object &on)
{
  gbres_comp *p = op->cast_to_gbres_comp();
  int n = on->int_of();
  gStack.insert(p->min_gens_matrix(n));
}
void cmd_gbres_getgb(object &op, object &on)
{
  gbres_comp *p = op->cast_to_gbres_comp();
  int n = on->int_of();
  gStack.insert(p->gb_matrix(n));
}
void cmd_gbres_change(object &op, object &on)
{
  gbres_comp *p = op->cast_to_gbres_comp();
  int n = on->int_of();
  gStack.insert(p->change_matrix(n));
}
void cmd_gbres_initial(object &op, object &on, object &olevel)
{
  gbres_comp *p = op->cast_to_gbres_comp();
  int n = on->int_of();
  int level = olevel->int_of();
  gStack.insert(p->initial_matrix(n,level));
}

int i_gbres_cmds()
{
  // Resolutions
  //install(ggres, cmd_gbres, TY_MATRIX, TY_INT, TY_INT);
  install(ggcalc, cmd_gbres_calc, TY_GBRES_COMP, TY_INTARRAY, TY_INTARRAY);

  install(ggstats, cmd_gbres_stats, TY_GBRES_COMP);
  install(ggpairs, cmd_betti2_pairs, TY_GBRES_COMP);
  install(ggremaining, cmd_betti2_remaining, TY_GBRES_COMP);
  install(ggbetti, cmd_betti2_minimal, TY_GBRES_COMP);
  install(ggnmonoms, cmd_betti2_nmonoms, TY_GBRES_COMP);

  install(ggresmap, cmd_gbres_map, TY_GBRES_COMP, TY_INT);
  install(ggresNmap, cmd_gbres_Nmap, TY_GBRES_COMP, TY_INT);
  install(ggresmodule, cmd_gbres_module, TY_GBRES_COMP, TY_INT);
  install(ggresNmodule, cmd_gbres_Nmodule, TY_GBRES_COMP, TY_INT);

  install(gggetmingens, cmd_gbres_mingens, TY_GBRES_COMP, TY_INT);
  install(gggetgb, cmd_gbres_getgb, TY_GBRES_COMP, TY_INT);
  install(gggetchange, cmd_gbres_change, TY_GBRES_COMP, TY_INT);
  install(gginitial, cmd_gbres_initial, TY_GBRES_COMP, TY_INT, TY_INT);
  return 1;
}
