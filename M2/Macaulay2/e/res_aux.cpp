// Copyright 1996 Michael E. Stillman
// Included from 'res.cc'
#include "matrixcon.hpp"

int res_comp::n_pairs(int lev, int d) const
{
  res_degree *p = get_degree_set(lev, d);
  if (p == NULL) return 0;
  return p->npairs;
}

int res_comp::n_left(int lev, int d) const
{
  res_degree *p = get_degree_set(lev, d);
  if (p == NULL) return 0;

  int result = 0;
  for (res_pair *q = p->first; q != NULL; q = q->next)
    if (q->syz_type == SYZ_S_PAIR 
	|| q->syz_type == SYZ_GEN) 
      result++;
      
  return result;
}

int res_comp::n_minimal(int lev, int d) const
{
  res_degree *p = get_degree_set(lev, d);
  if (p == NULL) return 0;
  int result = 0;
  for (res_pair *q = p->first; q != NULL; q = q->next)
    if (q->syz_type == SYZ_MINIMAL) result++;
      
  return result;
}

int res_comp::n_monoms(int lev, int d) const
{
  res_degree *p = get_degree_set(lev, d);
  if (p == NULL) return 0;
  int result = 0;
  for (res_pair *q = p->first; q != NULL; q = q->next)
    result += R->n_terms(q->syz);

  return result;
}

int res_comp::max_level() const
{
  return resn.length();
}

int res_comp::low_degree() const
{
  return lodegree;
}

int res_comp::high_degree() const
{
  return hidegree;
}

M2_arrayint res_comp::get_betti(int type) const
{
  int lo = low_degree();
  int hi = high_degree();
  int len = (type == 0 ? length_limit : max_level());

  int *bettis;
  betti_init(lo,hi,len,bettis);

  for (int d=lo; d<=hi; d++)
    for (int lev=0; lev<=len; lev++)
      {
	int val = 0;
	switch (type) {
	case 0:
	  val = n_minimal(lev,d);
	  break;
	case 1:
	  val = n_pairs(lev,d);
	  break;
	case 2:
	  val = n_left(lev,d);
	  break;
	case 3:
	  val = n_monoms(lev,d);
	  break;
	default:
	  val = -1;
	  break;
	}
	bettis[lev+(len+1)*(d-lo)] = val;
      }

  M2_arrayint result = betti_make(lo,hi,len,bettis);
  deletearray(bettis);
  return result;
}

M2_arrayint res_comp::betti_skeleton() const
{
  return get_betti(1);
}
M2_arrayint res_comp::betti_remaining() const
{
  return get_betti(2);
}
M2_arrayint res_comp::betti_minimal() const
{
  return get_betti(0);
}
M2_arrayint res_comp::betti_nmonoms() const
{
  return get_betti(3);
}

void res_comp::text_out(buffer &o, const res_pair *p) const
{
  res_pair *a = p->first;
  res_pair *b = p->second; // possibly NULL
  o << p->me << ' ';
  if (a != NULL)
    o << a->me << ' ' ;
  else o << ". ";
  if (b != NULL) o << b->me << ' ';
  else o << ". ";

  o << p->compare_num << ' ';

  switch (p->syz_type) {
  case SYZ_S_PAIR:
    o << "PR";
    break;
  case SYZ_GEN:
    o << "GN";
    break;
  case SYZ_MINIMAL:
    o << "SZ";
    break;
  case SYZ_NOT_MINIMAL:
    o << "GB";
    break;
  case SYZ_NOT_NEEDED:
    o << "NO";
    break;
  default:
    break;
  }

#if 0
//   if (p->mi_exists)
#endif
    o << "[mi: " << p->mi->length() << "]";
#if 0
//   else
//     {
//       res_pair *q = p->next_div;
//       int n = 0;
//       while (q != NULL) { n++; q = q->next_div; }
//       o << "[midiv: " << n << "]";
//     }
#endif
  M->elem_text_out(o, p->base_monom);
  if (gbTrace >= 3)
    {
      // Display the vector
      o << " syz: ";
      R->elem_text_out(o, p->syz);
    }
  o << newline;
}

void res_comp::text_out(const res_pair *p) const
{
  buffer o;
  text_out(o, p);
  emit(o.str());
}

void res_comp::stats() const
{
  buffer o;
  text_out(o);
  emit(o.str());
}
void res_comp::text_out(buffer &o) const
{
  o << "level/degree = " << n_level << '/' << n_degree << newline;
  o << "--- The total number of pairs in each level/slanted degree -----" << newline;
  M2_arrayint a = betti_skeleton();
  betti_display(o, a);
  o << "--- The number of pairs left to compute ------------------------" << newline;
  a = betti_remaining();
  betti_display(o, a);
  o << "--- (Lower bounds of) the minimal betti numbers ----------" << newline;
  a = betti_minimal();
  betti_display(o, a);
  if (gbTrace >= 1)
    {
      o << "--- Number of monomials  ---------------------------------" << newline;
      a = betti_nmonoms();
      betti_display(o, a);
    }

  // If the printlevel is high enough, display each element
  if (gbTrace >= 2)
    for (int lev=0; lev<resn.length(); lev++)
      {
	o << "---- level " << lev << " ----" << newline;
	for (int i=0; i<resn[lev]->bin.length(); i++)
	  {
	    res_degree *mypairs = resn[lev]->bin[i];
	    if (mypairs == NULL) continue;
	    for (res_pair *p = mypairs->first; p != NULL; p = p->next)
	      {
		o.put(i,4);
		o << ' ';
		text_out(o,p);
	      }
	  }
      }

}

const FreeModule *res_comp::free_of(int i) const
{
  FreeModule *result;
  result = P->make_Schreyer_FreeModule();
  if (i < 0 || i >= resn.length())
    return result;
  int *deg = P->degree_monoid()->make_one();
  int n = 0;
  res_level *lev = resn[i];
  for (int j=0; j<lev->bin.length(); j++)
    {
      res_degree *mypairs = lev->bin[j];
      for (res_pair *p = mypairs->first; p != NULL; p = p->next)
	{
	  multi_degree(p, deg);
	  result->append_schreyer(deg, p->base_monom, p->compare_num);
	  p->minimal_me = n++;
	}
    }
  P->degree_monoid()->remove(deg);
  
  return result;
}
const FreeModule *res_comp::minimal_free_of(int i) const
{
  FreeModule *result;
  if (i == 0)
    return generator_matrix->rows();
  result = P->make_FreeModule();
  if (i < 0 || i > length_limit)
    return result;
  int *deg = P->degree_monoid()->make_one();
  int nminimals = 0;
  res_level *lev = resn[i];
  for (int j=0; j<lev->bin.length(); j++)
    {
      res_degree *mypairs = lev->bin[j];
      for (res_pair *p = mypairs->first; p != NULL; p = p->next)
	if (p->syz_type == SYZ_MINIMAL)
	  {
	    multi_degree(p, deg);
	    result->append(deg);
	    p->minimal_me = nminimals++;
	  }
    }
  P->degree_monoid()->remove(deg);

  return result;
}

Matrix *res_comp::make(int level) const
{
  const FreeModule *F = free_of(level-1);
  const FreeModule *G = free_of(level);
  MatrixConstructor result(F, G, NULL);

  int n = 0;
  if (G == 0) return result.to_matrix();
  res_level *lev = resn[level];
  for (int j=0; j<lev->bin.length(); j++)
    {
      res_degree *mypairs = lev->bin[j];
      for (res_pair *p = mypairs->first; p != NULL; p = p->next)
        result.set_column(n++, R->to_vector(p->syz, F));    
    }
  return result.to_matrix();
}

//////////////////////////////////////////////
//  Minimal resolutions //////////////////////
//////////////////////////////////////////////

void res_comp::reduce_minimal(int x, resterm *&f, array<res_pair *> &elems) const
{
  // Reduce any components of 'f' that correspond to non minimal syzygies.
  const resterm *tm;

  for (int i=x-1; i>=0; i--)
    {
      res_pair *p = elems[i];
      if (p->syz_type == SYZ_NOT_MINIMAL)
	while ((tm = R->component_occurs_in(p->pivot_term->comp, f)) != NULL)
	  {
	    // Subtract the proper multiple to f.  f = ... + c m e_y + ...
	    // and                                 p = ... + d n e_y
	    // where n|m.  So we want to compute f -= c/d m/n p.
	    ring_elem c = K->divide(tm->coeff, p->pivot_term->coeff); // exact division
	    // MES: is the following line actually needed?
	    M->divide(tm->monom, p->pivot_term->monom, MINIMAL_mon);
	    if (p->stripped_syz == NULL)
	      p->stripped_syz = R->strip(p->syz);
	    R->subtract_multiple_to(f, c, MINIMAL_mon, p->stripped_syz);
	  }
    }
}

Matrix *res_comp::make_minimal(int i) const
{
  const FreeModule *F = minimal_free_of(i-1);
  const FreeModule *G = minimal_free_of(i);
  MatrixConstructor result(F, G, NULL);
  if (i < 0 || i > length_limit) return result.to_matrix();
  array<res_pair *> elems;

  res_level *lev = resn[i];
  for (int j=0; j<lev->bin.length(); j++)
    for (res_pair *p = lev->bin[j]->first; p != NULL; p = p->next)
      elems.append(p);

  int thisx = 0;
  for (int x=0; x<elems.length(); x++)
    {
      res_pair *p = elems[x];
      if (p->syz_type == SYZ_MINIMAL)
	{
	  if (p->stripped_syz == NULL)
	    {
	      p->stripped_syz = R->strip(p->syz);
	      reduce_minimal(x,p->stripped_syz, elems);
	    }
	  result.set_column(thisx++, R->to_vector(p->stripped_syz, F, 1));
	}
    }
  return result.to_matrix();
}




#if 0
// // This was commented out earlier than 2002
// void cmd_res1(object &om, object &olength, object &ostrategy)
// {
//   Matrix m = om->cast_to_Matrix();
//   int maxlev = olength->int_of();
//   int strategy = ostrategy->int_of();
//   res_comp *p = new res_comp(m, maxlev, strategy);
//   gStack.insert(p);
// }
#endif

#if 0
// void cmd_res_calc(object &op, object &odeg, object &oargs)
// {
//   res_comp *p = op->cast_to_res_comp();
//   intarray *deg = odeg->intarray_of();
//   intarray *args = oargs->intarray_of();
//   if (args->length() != 6)
//     {
//       gError << "res: expected 5 elements in parameter array";
//       return;
//     }
//   int *d;
//   if (deg->length() == 0)
//     d = NULL;
//   else 
//     d = deg->raw();
// 
//   //args[0] = LengthLimit
//   //args[1] = SyzygyLimit
//   //args[2] = PairLimit
//   //args[3..5] = SyzygyLimit(nSyz, Level, Degree)
//   int llimit = (*args)[0];
//   int syzlimit = (*args)[1];
//   int pairlimit = (*args)[2];
//   int nsyz = (*args)[3];
//   int nlevel = (*args)[4];
//   int ndeg = (*args)[5];
//   gStack.insert(make_object_int(p->calc(d, llimit, 
//   					syzlimit, pairlimit, nsyz,
//   					nlevel, ndeg)));
// }
// void cmd_res_stats(object &op)
// {
//   res_comp *p = op->cast_to_res_comp();
//   p->stats();
// }
// void cmd_betti_pairs(object &op)
// {
//   res_comp *p = op->cast_to_res_comp();
//   object_intarray *result = new object_intarray;
//   p->betti_skeleton(*result->intarray_of());
//   gStack.insert(result);
// }
// void cmd_betti_remaining(object &op)
// {
//   res_comp *p = op->cast_to_res_comp();
//   object_intarray *result = new object_intarray;
//   p->betti_remaining(*result->intarray_of());
//   gStack.insert(result);
// }
// void cmd_betti_minimal(object &op)
// {
//   res_comp *p = op->cast_to_res_comp();
//   object_intarray *result = new object_intarray;
//   p->betti_minimal(*result->intarray_of());
//   gStack.insert(result);
// }
// void cmd_betti_nmonoms(object &op)
// {
//   res_comp *p = op->cast_to_res_comp();
//   object_intarray *result = new object_intarray;
//   p->betti_nmonoms(*result->intarray_of());
//   gStack.insert(result);
// }
// 
// void cmd_res_Nmap(object &op, object &on)
// {
//   res_comp *p = op->cast_to_res_comp();
//   gStack.insert(p->make(on->int_of()));
// }
// 
// void cmd_res_map(object &op, object &on)
// {
//   res_comp *p = op->cast_to_res_comp();
//   gStack.insert(p->make_minimal(on->int_of()));
// }
// 
// void cmd_res_Nmodule(object &op, object &on)
// {
//   res_comp *p = op->cast_to_res_comp();
//   gStack.insert(p->free_of(on->int_of()));
// }
// 
// void cmd_res_module(object &op, object &on)
// {
//   res_comp *p = op->cast_to_res_comp();
//   gStack.insert(p->minimal_free_of(on->int_of()));
// }
// 
// void cmd_res_skeleton(object &op, object &on)
// {
//   res_comp *p = op->cast_to_res_comp();
//   int strategy = on->int_of();
//   p->skeleton(strategy);
// }
// 
// int i_res_cmds()
// {
//   // Resolutions
//   //install(ggres, cmd_res, TY_MATRIX, TY_INT, TY_INT);
//   install(ggcalc, cmd_res_calc, TY_RES_COMP, TY_INTARRAY, TY_INTARRAY);
// 
//   install(ggstats, cmd_res_stats, TY_RES_COMP);
//   install(ggpairs, cmd_betti_pairs, TY_RES_COMP);
//   install(ggremaining, cmd_betti_remaining, TY_RES_COMP);
//   install(ggbetti, cmd_betti_minimal, TY_RES_COMP);
//   install(ggnmonoms, cmd_betti_nmonoms, TY_RES_COMP);
// 
//   install(ggresmap, cmd_res_map, TY_RES_COMP, TY_INT);
//   install(ggresNmap, cmd_res_Nmap, TY_RES_COMP, TY_INT);
//   install(ggresmodule, cmd_res_module, TY_RES_COMP, TY_INT);
//   install(ggresNmodule, cmd_res_Nmodule, TY_RES_COMP, TY_INT);
// 
//   install(ggskeleton, cmd_res_skeleton, TY_RES_COMP, TY_INT);
//   return 1;
// }
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
