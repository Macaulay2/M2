// Copyright 1996 Michael E. Stillman
// Included from 'res2.cc'
#include "matrixcon.hpp"

M2_arrayint res2_comp::betti_skeleton() const
{
  int lo = lodegree;
  int hi = lo+hidegree;
  int len = resn.length()-1;
  int *bettis;
  betti_init(lo,hi,len,bettis);
  for (int lev=0; lev<=len; lev++)
    {
      for (res2_pair *p = resn[lev]->pairs; p != NULL; p = p->next)
	{
	  int d = p->degree;
	  bettis[lev+(len+1)*d]++;
	}
    }
  M2_arrayint result = betti_make(lo,hi,len,bettis);
  deletearray(bettis);
  return result;
}

M2_arrayint res2_comp::betti_remaining() const
{
  int lo = lodegree;
  int hi = lo+hidegree;
  int len = resn.length()-1;
  int *bettis;
  betti_init(lo,hi,len,bettis);
  for (int lev=0; lev<=len; lev++)
    {
      for (res2_pair *p = resn[lev]->pairs; p != NULL; p = p->next)
	{
	  if (p->syz_type != SYZ2_S_PAIR) continue;
	  int d = p->degree;
	  bettis[lev+(len+1)*d]++;
	}
    }
  M2_arrayint result = betti_make(lo,hi,len,bettis);
  deletearray(bettis);
  return result;
}

M2_arrayint res2_comp::betti_minimal() const
    // Negative numbers represent upper bounds
{
  int lo = lodegree;
  int hi = lo+hidegree;
  int len = resn.length()-1;
  int *bettis;
  betti_init(lo,hi,len,bettis);
  for (int lev=0; lev<=len; lev++)
    {
      for (res2_pair *p = resn[lev]->pairs; p != NULL; p = p->next)
	{
	  if (p->syz_type != SYZ2_MINIMAL) continue;
	  int d = p->degree;
	  bettis[lev+(len+1)*d]++;
	}
    }
  M2_arrayint result = betti_make(lo,hi,len,bettis);
  deletearray(bettis);
  return result;
}

M2_arrayint res2_comp::betti_nmonoms() const
{
  int lo = lodegree;
  int hi = lo+hidegree;
  int len = resn.length()-1;
  int *bettis;
  betti_init(lo,hi,len,bettis);
  for (int lev=0; lev<=len; lev++)
    {
      for (res2_pair *p = resn[lev]->pairs; p != NULL; p = p->next)
	{
	  int d = p->degree;
	  bettis[lev+(len+1)*d] += R->n_terms(p->syz);
	}
    }
  M2_arrayint result = betti_make(lo,hi,len,bettis);
  deletearray(bettis);
  return result;
}

M2_arrayint res2_comp::get_betti(int type) const
{
  switch (type) {
  case 0:
    return betti_minimal();
  case 1:
    return betti_skeleton();
  case 2:
    return betti_remaining();
  case 3:
    return betti_nmonoms();
  }
  ERROR("received unknown betti type");
  return 0;
}

void res2_comp::text_out(const res2_pair *p) const
{
  buffer o;
  text_out(o,p);
  emit(o.str());
}
void res2_comp::text_out(buffer &o, const res2_pair *p) const
{
  res2_pair *a, *b;

  a = p->syz->comp;
  if (p->syz->next == NULL)
    b = NULL;
  else
    b = p->syz->next->comp;

  o << p->me << ' ';
  o << p->level << ' ' << p->degree << ' ';
  if (a != NULL)
    o << a->me << ' ' ;
  else o << ". ";
  if (b != NULL) o << b->me << ' ';
  else o << ". ";

  o << p->compare_num << ' ';

  switch (p->syz_type) {
  case SYZ2_S_PAIR:
    o << "PR";
    break;
  case SYZ2_MINIMAL:
    o << "SZ";
    break;
  case SYZ2_NOT_MINIMAL:
    o << "GB";
    o << "(pivot " << p->pivot_term->comp->me << ")";
    break;
  case SYZ2_NOT_NEEDED:
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
//       res2_pair *q = p->next_div;
//       int n = 0;
//       while (q != NULL) { n++; q = q->next_div; }
//       o << "[midiv: " << n << "]";
//     }
#endif
  M->elem_text_out(o, p->syz->monom);
  o << " [" << R->n_terms(p->syz) << "] ";
  if (gbTrace >= 4)
    {
      // Display the vector
      o << " syz: ";
      R->elem_text_out(o, p->syz);
    }
  o << newline;
}

void res2_comp::stats() const
{
  buffer o;
  text_out(o);
  emit(o.str());
}

void res2_comp::text_out(buffer &o) const
{
  o << "--- The total number of pairs in each level/slanted degree -----" << newline;
  M2_arrayint a = betti_skeleton();
  betti_display(o, a);
  //  o << "--- The number of pairs left to compute ------------------------" << newline;
  //  a.shrink(0);
  //  betti_remaining(a);
  //  betti_display(o, a);
  o << "--- (Lower bounds of) the minimal betti numbers ----------" << newline;
  a = betti_minimal();
  betti_display(o, a);
  if (gbTrace >= 1)
    {
      o << "-- #Reduction steps = " << total_reduce_count << "--" 
	   << " ones " << n_ones 
	   << " unique " << n_unique 
	   << " others " << n_others << " ----" << newline;
      o << "--- Number of monomials  ---------------------------------" << newline;
      a = betti_nmonoms();
      betti_display(o, a);
    }

  // If the printlevel is high enough, display each element
  if (gbTrace >= 2)
    for (int lev=0; lev<resn.length(); lev++)
      {
	if (resn[lev]->pairs == NULL) continue;
	o << "---- level " << lev << " ----" << newline;
	for (res2_pair *p = resn[lev]->pairs; p != NULL; p = p->next)
	  text_out(o,p);
      }
}

FreeModule *res2_comp::free_of(int i) const
{
  FreeModule *result;
  result = P->make_Schreyer_FreeModule();
  if (i < 0 || i >= resn.length())
    return result;

  int *deg = degree_monoid()->make_one();
  int n = 0;
  for (res2_pair *p = resn[i]->pairs; p != NULL; p = p->next)
    {
      multi_degree(p, deg);
      result->append_schreyer(deg, p->syz->monom, p->compare_num);
      p->me = n++;
    }
  degree_monoid()->remove(deg);
  return result;
}
FreeModule *res2_comp::minimal_free_of(int i) const
{
  FreeModule *result;
  result = P->make_Schreyer_FreeModule();
  if (i < 0 || i >= resn.length()-1)
    return result;

  int *deg = degree_monoid()->make_one();
  int n = 0;
  for (res2_pair *p = resn[i]->pairs; p != NULL; p = p->next)
    if (p->syz_type == SYZ2_MINIMAL)
      {
	multi_degree(p, deg);
	result->append_schreyer(deg, p->syz->monom, p->compare_num);
	p->me = n++;
      }
  degree_monoid()->remove(deg);
  return result;
}

Matrix *res2_comp::make(int level) const
{
  const FreeModule *F = free_of(level-1);
  const FreeModule *G = free_of(level);
  MatrixConstructor result(F, G, NULL);
  //  Matrix *result = new Matrix(free_of(level-1), free_of(level));

  int n = 0;
  if (G->rank() == 0) return result.to_matrix();
  for (res2_pair *p = resn[level]->pairs; p != NULL; p = p->next)
    result.set_column(n++, R->to_vector(p->syz, F));
  return result.to_matrix();
}

//////////////////////////////////////////////
//  Minimal resolutions //////////////////////
//////////////////////////////////////////////

void res2_comp::reduce_minimal(int x, res2term *&f, 
			       array<res2_pair *> &elems,
			       array<res2term *> &stripped) const
{
  // Reduce any components of 'f' that correspond to non minimal syzygies.
  const res2term *tm;

  for (int i=x-1; i>=0; i--)
    {
      res2_pair *p = elems[i];
      if (p->syz_type == SYZ2_NOT_MINIMAL)
	while ((tm = R->component_occurs_in(p->pivot_term->comp, f)) != NULL)
	  {
	    // Subtract the proper multiple to f.  f = ... + c m e_y + ...
	    // and                                 p = ... + d n e_y
	    // where n|m.  So we want to compute f -= c/d m/n p.
	    ring_elem c = K->divide(tm->coeff, p->pivot_term->coeff); // exact division
	    // MES: is the following line actually needed?
	    M->divide(tm->monom, p->pivot_term->monom, MINIMAL_mon);
	    if (stripped[p->me] == NULL)
	      stripped[p->me] = R->strip(p->syz);
	    R->subtract_multiple_to(f, c, MINIMAL_mon, stripped[p->me]);
	  }
    }
}

Matrix *res2_comp::make_minimal(int i) const
{
  const FreeModule *F = minimal_free_of(i-1);
  const FreeModule *G = minimal_free_of(i);
  MatrixConstructor result(F, G, NULL);
  if (i <= 0 || i >= resn.length()-1) return result.to_matrix();

  array<res2_pair *> elems;
  array<res2term *> stripped;

  int n = 0;
  for (res2_pair *p = resn[i]->pairs; p != NULL; p = p->next)
    {
      p->me = n++;
      elems.append(p);
      stripped.append(static_cast<res2term *>(NULL));
    }

  int thisx = 0;
  for (int x=0; x<elems.length(); x++)
    {
      res2_pair *p = elems[x];
      if (p->syz_type == SYZ2_MINIMAL)
	{
	  if (stripped[p->me] == NULL)
	    {
	      stripped[p->me] = R->strip(p->syz);
	      reduce_minimal(x,stripped[p->me], elems, stripped);
	    }
	  result.set_column(thisx++, R->to_vector(stripped[p->me], F, 1));
	}
    }

  return result.to_matrix();
}

/////////////////////////////
// Minimalization routines //
/////////////////////////////

///////////////////////////////////////////////////////
// pivot -- apply a specific pivot to the resolution //
//                                                   //
// modifies the resolution                           //
///////////////////////////////////////////////////////

#if 0
// Matrix res2_comp::reduce_mod_vars(int level) const
// {
//   // Set all variables to 0, but only take columns marked
//   // as SZ or GB, not NO.  The matrix returned is over K.
// 
//   // Set 'me' values for level 'level' and 'level-1'.
//   
//   FreeModule *rows = K->make_FreeModule();
//   FreeModule *cols = K->make_FreeModule();
//   Matrix result(rows, cols);
//   for (res_pair *p = resn[level]->pairs; p != NULL; p = p->next)
//     {
//       if (p->syz_type == SYZ2_MINIMAL || SYZ2_NOT_MINIMAL)
// 	{
// 	  result[next++] = reduce_mod_vars(result.rows(), p->syz);
// 	}
//     }
//   return result;
// }
// void res2_comp::set_pivots(int level)
// {
//   // Determines the status of each element (SZ, GB, NO)
//   // (given the status of each element of higher level).
//   // Also this sets the pivot_term of each element at this level
//   
//   // m = reduce_mod_vars(level)
//   // now reduce this matrix, using change of basis.
//   // for each column of this matrix, we will set a column to SYZ2_NOT_MINIMAL,
//   // a row to SYZ2_NOT_NEEDED, and the columns pivot_term to the chosen pivot.
// }
// 
// void res2_comp::pivot(int level, int r, int c)
// {
//   res2_pair *p;
// 
//   // First find the specific pivot column
//   for (p = resn[level]->pairs; p!=NULL; p=p->next)
//     if (p->me == c)
//       {
// 	pivot_pair = p;
// 	break;
//       }
// 
//   // Now find the pivot ring element
//   term head;
//   term *f = &head;
//   for (tm = pivot_pair->syz; tm != NULL; tm=tm->next)
//     {
//       if (tm->comp->me == r)
// 	{
// 	  pivot_row = tm->comp;
// 
// 	  // Grab this element
// 	  f->next = P->term(tm->coeff, tm->monom);
// 	  f = f->next;
// 	  M->divide(f->monom, pivot_row->syz->monom, f->monom);
// 	}
//     }
//   f->next = NULL;
//   f = head.next;
// 
//   // Now loop through the columns of the level th matrix,
//   // replacing each column (other than column c) with 
//   // elem(level,i) = f*elem(level,i) - c*v,
//   // where c = elem(level,r,i).
//   for (res2_pair *p = resn[level]->pairs; p!=NULL; p=p->next)
//     {
//       if (p == pivot_pair) continue;
//       res2term *w = p->syz;
//       if (R->get_coefficient(w, pivot_row, g))
// 	{
// 	  P->negate_to(g);
// 	  res2term *h1 = R->mult(f,w);
// 	  res2term *h2 = R->mult(g,pivot_pair->syz);
// 	  R->add_to(h1,h2);
// 	  P->remove(g);
// 	  R->remove(p->syz);
// 	  p->syz = h1;
// 	}
//     }
// 
//   // Remove every occurrence of row 'c' in level 'level+1'.
//   // We save these up, to remove all at once, using 'strip_matrix'.
//   pivot_pair->syz_type = SYZ2_NOT_MINIMAL;
//   pivot_row->syz_type = SYZ2_NOT_NEEDED;
// 
//   // We will never use this column again, so remove it:
//   R->remove(pivot_pair->syz);
//   pivot_pair->syz = NULL;    // This is a bit dangerous, since
// 			     // many routines use this as if it must be nonNULL...
// }
// 
// void res2_comp::strip_matrix(int level)
// {
//   // Remove all rows which are marked as SYZ2_NOT_MINIMAL
//   for (res2_pair *p = resn[level]->pairs; p != NULL; p=p->next)
//     {
//       res2term *f = strip(p->syz);
//       R->remove(p->syz);
//       p->syz = f;
//     }
// }
#endif

#if 0
// void cmd_res2_calc(object &op, object &odeg, object &oargs)
// {
//   res2_comp *p = op->cast_to_res2_comp();
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
//   gStack.insert(make_object_int(p->calc(d, (*args)[0], 
// 					(*args)[1], (*args)[2],
// 					(*args)[3], (*args)[4],
// 					(*args)[5])));
// }
// void cmd_res2_stats(object &op)
// {
//   res2_comp *p = op->cast_to_res2_comp();
//   p->stats();
// }
// void cmd_res2_pairs(object &op)
// {
//   res2_comp *p = op->cast_to_res2_comp();
//   object_intarray *result = new object_intarray;
//   p->betti_skeleton(*result->intarray_of());
//   gStack.insert(result);
// }
// void cmd_res2_remaining(object &op)
// {
//   res2_comp *p = op->cast_to_res2_comp();
//   object_intarray *result = new object_intarray;
//   p->betti_remaining(*result->intarray_of());
//   gStack.insert(result);
// }
// void cmd_res2_minimal(object &op)
// {
//   res2_comp *p = op->cast_to_res2_comp();
//   object_intarray *result = new object_intarray;
//   p->betti_minimal(*result->intarray_of());
//   gStack.insert(result);
// }
// void cmd_res2_nmonoms(object &op)
// {
//   res2_comp *p = op->cast_to_res2_comp();
//   object_intarray *result = new object_intarray;
//   p->betti_nmonoms(*result->intarray_of());
//   gStack.insert(result);
// }
// 
// void cmd_res2_Nmap(object &op, object &on)
// {
//   res2_comp *p = op->cast_to_res2_comp();
//   gStack.insert(p->make(on->int_of()));
// }
// 
// void cmd_res2_map(object &op, object &on)
// {
//   res2_comp *p = op->cast_to_res2_comp();
//   gStack.insert(p->make_minimal(on->int_of()));
// }
// 
// void cmd_res2_Nmodule(object &op, object &on)
// {
//   res2_comp *p = op->cast_to_res2_comp();
//   gStack.insert(p->free_of(on->int_of()));
// }
// 
// void cmd_res2_module(object &op, object &on)
// {
//   res2_comp *p = op->cast_to_res2_comp();
//   gStack.insert(p->minimal_free_of(on->int_of()));
// }
// 
// void cmd_res2_skeleton(object &op, object &on)
// {
//   res2_comp *p = op->cast_to_res2_comp();
//   int strategy = on->int_of();
//   p->skeleton(strategy);
// }
// 
// int i_res2_cmds()
// {
//   // Resolutions
//   //install(ggres, cmd_res2, TY_MATRIX, TY_INT, TY_INT);
//   install(ggcalc, cmd_res2_calc, TY_RES2_COMP, TY_INTARRAY, TY_INTARRAY);
// 
//   install(ggstats, cmd_res2_stats, TY_RES2_COMP);
//   install(ggpairs, cmd_res2_pairs, TY_RES2_COMP);
//   install(ggremaining, cmd_res2_remaining, TY_RES2_COMP);
//   install(ggbetti, cmd_res2_minimal, TY_RES2_COMP);
//   install(ggnmonoms, cmd_res2_nmonoms, TY_RES2_COMP);
// 
//   install(ggresmap, cmd_res2_map, TY_RES2_COMP, TY_INT);
//   install(ggresNmap, cmd_res2_Nmap, TY_RES2_COMP, TY_INT);
//   install(ggresmodule, cmd_res2_module, TY_RES2_COMP, TY_INT);
//   install(ggresNmodule, cmd_res2_Nmodule, TY_RES2_COMP, TY_INT);
// 
//   install(ggskeleton, cmd_res2_skeleton, TY_RES2_COMP, TY_INT);
//   return 1;
// }
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
