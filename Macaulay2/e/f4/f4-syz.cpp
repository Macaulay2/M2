//////////////////////////////////////////
// Syzygy methods of F4GB
// ... used only if(using_syz)
//////////////////////////////////////////

#include <ctime>

#include "f4.hpp"
#include "monsort.hpp"
#include "../freemod.hpp"
#include "../poly.hpp"

static clock_t syz_clock_sort_columns = 0;

////////////////////////////////////////////////////////////////
// Append rows to the syzygy matrix
//
// Idea: module-monomial $m e_i$ is represented by pair 
//   (monom=m*lm(g_i), comp=i), 
// where either g_i = gens[i] if i<gens.size(), 
//       or     g_i = gb[i+gens.size()], otherwise.
// "monom=m*lm(g_i)" forces the already coded module-monomial 
// comparison function to impose 
// Schreyer order. Should be changed eventually.

void F4GB::syz_load_gen(int which)
{
  if (!using_syz) return;
  
  M->copy(gens[which]->f.monoms /*lead monom*/, syz_next_monom);  
  M->set_component(which, syz_next_monom); 

  packed_monomial m = syz_next_monom;
  int newcol = syz_new_column(m); // this inserts a new monomial in syzH
  
  row_elem syz_r;
  syz_r.monom = m;
  syz_r.elem = which; // here info is duplicated: which == M->get_component(m)
  syz_r.len = 1;
  syz_r.coeffs = F4Mem::coefficients.allocate(1);
  static_cast<int *>(syz_r.coeffs)[0] = 0; // this represents 1 in the coefficient field 
  syz_r.comps = F4Mem::components.allocate(1);
  static_cast<int *>(syz_r.comps)[0] = newcol;
  syz->rows.push_back(syz_r); 
}

void F4GB::syz_load_row(packed_monomial monom, int which)
{
  if (!using_syz) return;
  
  M->unchecked_mult(monom, gb[which]->f.monoms /*lead monom*/, syz_next_monom);  
  M->set_component(which+gens.size(), syz_next_monom); 
  
  packed_monomial m = syz_next_monom;
  int newcol = syz_new_column(m); // this inserts a new monomial in syzH

  row_elem syz_r;
  syz_r.monom = m;
  syz_r.elem = M->get_component(m); // .elem is not used at the monoment 
  syz_r.len = 1;
  syz_r.coeffs = F4Mem::coefficients.allocate(1);
  static_cast<int *>(syz_r.coeffs)[0] = 0; // this represents 1 in the coefficient field 
  syz_r.comps = F4Mem::components.allocate(1);
  static_cast<int *>(syz_r.comps)[0] = newcol;
  syz->rows.push_back(syz_r);
}


////////////////////////////////////////////////
// initialization


void F4GB::clear_syz_matrix()
{
  if (!using_syz) return;

  // Clear the rows first
  for (int i=0; i<syz->rows.size(); i++)
    {
      row_elem &r = syz->rows[i];
      F4Mem::coefficients.deallocate((int*&)(r.coeffs));
      F4Mem::components.deallocate(r.comps);
      r.len = 0;
      r.elem = -1;
      r.monom = 0;
    }
  syzH.reset();
  syzB.reset();
  syz->rows.clear(); 
  syz->columns.clear();
}

void F4GB::clear_master_syz()
{
  if (!using_syz) return;

  // Clear the rows first
  for (int i=0; i<master_syz->rows.size(); i++)
    {
      row_elem &r = master_syz->rows[i];
      F4Mem::coefficients.deallocate((int*&)(r.coeffs));
      F4Mem::components.deallocate(r.comps);
      r.len = 0;
      r.elem = -1;
      r.monom = 0;
    }
  master_syz->rows.clear(); 
  master_syz->columns.clear();

  master_syzH.reset();
  master_syzB.reset();
  master_syz_next_monom = master_syzB.reserve(1+M->max_monomial_size());
  master_syz_next_monom++;
}

void F4GB::reset_syz_matrix()
{
  if (!using_syz) return;

  syz_next_monom = syzB.reserve(1+M->max_monomial_size());
  syz_next_monom++;
}

void F4GB::reset_master_syz()
{
  if (!using_syz) return;

  master_syz_next_monom = master_syzB.reserve(1+M->max_monomial_size());
  master_syz_next_monom++;
}



int F4GB::syz_new_column(packed_monomial m)
{
  packed_monomial mm;
  if (syzH.find_or_insert(m, mm)) {// this should not happen
    fprintf(stderr, "syz_new_column: monomial not expected in syzH\n");
    error();
  }
  // the above line insures that
  // m is a packed monomial (with component), 
  // unique via the hash table syzH, syzB.
  syzB.intern(1+M->monomial_size(m));
  syz_next_monom = syzB.reserve(1+M->max_monomial_size());
  syz_next_monom++;

  column_elem c;
  int next_column = syz->columns.size();
  m[-1] = next_column;
  c.monom = m;
  c.head = -2;
  syz->columns.push_back(c);
  return next_column;
}

void F4GB::syz_reorder_columns()
{
  if (!using_syz) return;

  // Same as reorder_columns(), but for syzygy matrix

  int nrows = syz->rows.size();
  int ncols = syz->columns.size();

  // sort the columns

  int *column_order = F4Mem::components.allocate(ncols);
  int *ord = F4Mem::components.allocate(ncols);

  clock_t begin_time = clock();
  for (int i=0; i<ncols; i++)
    {
      column_order[i] = i;
    }

  if (gbTrace >= 2)
    fprintf(stderr, "ncomparisons = ");

  ColumnsSorter C(M, syz);
  QuickSorter<ColumnsSorter>::sort(&C, column_order, ncols);

  if (gbTrace >= 2)
    fprintf(stderr, "%ld, ", C.ncomparisons());
  clock_t end_time = clock();
  syz_clock_sort_columns += end_time - begin_time;
  double nsecs = end_time - begin_time;
  nsecs /= CLOCKS_PER_SEC;
  if (gbTrace >= 2)
    fprintf(stderr, " time = %f\n", nsecs);

  for (int i=0; i<ncols; i++)
    {
      ord[column_order[i]] = i;
    }

  // Now move the columns into position
  coefficient_matrix::column_array newcols;
  newcols.reserve(ncols);
  for (int i=0; i<ncols; i++)
    {
      long newc = column_order[i];
      newcols.push_back(syz->columns[newc]);
    }

  // Now reset the components in each row
  for (int r=0; r<nrows; r++)
    {
      row_elem &row = syz->rows[r];
      for (int i=0; i<row.len; i++)
	{
	  int oldcol = row.comps[i];
	  int newcol = ord[oldcol];
	  row.comps[i] = newcol;
	}
    }

  std::swap(syz->columns, newcols);
  F4Mem::components.deallocate(column_order);
  F4Mem::components.deallocate(ord);
}

////////////////////////////////////////////////
//////// SYZYGY MANIPULATIONS

///////////////////////////////////////////////////
// create dense vector from syz->row[i]
void F4GB::syz_dense_row_fill_from_sparse(int i)
{
  if (!using_syz) return;

  row_elem& r = syz->rows[i];
  KK->dense_row_fill_from_sparse(syz_row, r.len, r.coeffs, r.comps);
}
 
////////////////////////////////////////////////////////////
// record the reduction of current row 
//        with respect to row[pivot] 
//        ( <li> and <lj> are the leading terms 
//          of <i> and <j> )  
void F4GB::syzygy_row_record_reduction(int pivot, int li, int lj)
{
  if (!using_syz) return;

  row_elem& r = syz->rows[pivot];
  int *elems = static_cast<int *>(syz_row.coeffs);
  int *sparseelems = static_cast<int *>(r.coeffs);
  int *comps = static_cast<int *>(r.comps);

  int a;                               
  const CoefficientRingZZp* R = KK->get_coeff_ring();                             
  R->divide(a,li,lj); // a = li/lj 
  for (int i=0; i<r.len; i++)
    R->subtract_multiple(elems[*comps++], a, *sparseelems++);
}

void F4GB::syzygy_row_divide(int i, int c)
{
  if (!using_syz) return;

  const CoefficientRingZZp* R = KK->get_coeff_ring();
  row_elem &r = syz->rows[i];
  f4vec elems = static_cast<int *>(r.coeffs);
  for (int j=0; j<r.len; j++)
    R->divide(elems[j],elems[j],c);
}

void F4GB::syz_dense_row_to_sparse_row(row_elem& s)
{
  if (!using_syz) return;

  const CoefficientRingZZp* Kp = KK->get_coeff_ring();
  int &result_len = s.len;
  F4CoefficientArray &result_sparse = s.coeffs;
  int *&result_comps = s.comps;
  
  // let's be lazy!!!
  int first = 0;
  int last = syz->columns.size()-1; 
  
  int *elems = static_cast<int *>(syz_row.coeffs);
  int len = 0;
  for (int i=first; i<=last; i++)
    if (!Kp->is_zero(elems[i])) len++;
  int *in_sparse = F4Mem::coefficients.allocate(len);
  int *in_comps = F4Mem::components.allocate(len);
  result_len = len;
  result_sparse = in_sparse;
  result_comps = in_comps;
  for (int i=first; i<=last; i++)
    if (!Kp->is_zero(elems[i]))
      {
        *in_sparse++ = elems[i];
        *in_comps++ = i;
        Kp->set_zero(elems[i]);
      }
}

void F4GB::insert_syz(row_elem &r, int g/*=-1*/)
{
  if (!using_syz) return;

  // Insert the syzygy corresponding to r.
  // If g>=0 then the syzygy = -1.gb[g] + r. 

  // At the moment...
  // just appends the syzygy to syz_basis

  long length = r.len + (g<0?0:1);
  long nslots = M->max_monomial_size();
  long nlongs = length * nslots;

  gbelem *result = new gbelem;
  result->f.len = length;

  // If the coeff array is null, then that means the coeffs come from the original array
  // Here we copy it over.
   
  if (g<0) 
    result->f.coeffs = KK->copy_F4CoefficientArray(r.len, r.coeffs);
  else {
    result->f.coeffs = F4Mem::coefficients.allocate(length); 
    int* rcoeffs = (int*)result->f.coeffs;
    int* elems = (int*)r.coeffs;

    // make "-1"
    const CoefficientRingZZp* Kp = KK->get_coeff_ring(); 
    Kp->set_zero(rcoeffs[0]);
    Kp->subtract(rcoeffs[0],rcoeffs[0],0); // "0"=1

    for (int i=0; i<r.len; i++)
      rcoeffs[i+1] = elems[i];    
  }

  result->f.monoms = F4Mem::allocate_monomial_array(nlongs);

  packed_monomial nextmonom = result->f.monoms;
  if (g>=0) // the leading term is (monom=1,comp=g)
    {
      M->one(g, nextmonom);
      nextmonom += nslots;
    }
  for (int i=0; i<r.len; i++)
    {
      packed_monomial m = syz->columns[r.comps[i]].monom;
      int comp = M->get_component(m);
      packed_monomial n =  //lead monom of corresponding gens or gb element 
	(comp < gens.size() ? gens[comp] : gb[comp-gens.size()])->f.monoms;
      M->unchecked_divide(m, n, nextmonom); // m = n*(real monomial)
      nextmonom += nslots;
    }
  F4Mem::components.deallocate(r.comps);
  r.len = 0;
  result->deg = this_degree;
  result->alpha = M->last_exponent(result->f.monoms);
  result->minlevel = ELEM_POSSIBLE_MINGEN; 

  // add another dimension to syzF if a new basis element is inserted
  if (gbTrace>0 && g>=0) {
    const PolynomialRing* R = F->get_ring()->cast_to_PolynomialRing();
    int *deg = R->degree_monoid()->make_one();
    packed_monomial lm = result->f.monoms; // leading monomial
    packed_monomial m = syz->columns[M->get_component(lm)].monom;


    //!!! this is a hack copied from F4toM2Interface::to_M2_vec
    int *exp = newarray_atomic(int, M->n_vars()+1);
    ntuple_word *lexp = newarray_atomic(ntuple_word, M->n_vars()+1);
    long comp;
    M->to_exponent_vector(m, lexp, comp);
    for (int a=0; a<M->n_vars(); a++)
      exp[a] = lexp[a];
    //!!!

    const Monoid* MM = R->getMonoid();
    int* mm = MM->make_one();
    MM->from_expvector(exp,mm);
    
    syzF->append_schreyer(deg, mm, 
			    syz_basis.size() 
			    /* compare# = number of the element in the basis
			     ... or is it minus that??? */);
    KK->get_ring()->degree_monoid()->remove(deg);
    deletearray(exp);
    deletearray(lexp);
  }

  syz_basis.push_back(result);
}

/////////////////////////////////////////////////////////////
// DEBUG routines

void F4GB::show_syz_matrix()
{
  if (!using_syz) return;
  fprintf(stderr, "---- ---- ---- ---- ---- ---- ----\n");
  MutableMatrix *q = F4toM2Interface::to_M2_MutableMatrix(KK,syz,gens,gb);
  buffer o;
  q->text_out(o);
  emit(o.str());
  fprintf(stderr, "---- ---- ---- ---- ---- ---- ----\n");
}

void F4GB::show_syz_basis() const
{
  if (!using_syz) return;

  // Debugging routine
  // Display the array, and all of the internal information in it too.

  const gb_array& g = syz_basis;

#if 0
  // make sure syzF has rank |gens|+|gb|
  if (syzF->rank()==0) 
    for (int i=0; i<gens.size(); i++)
      // ??? what does the next line do ???
      syzF->append_schreyer(gens[i]->f.monoms, gens[i]->f.monoms, i); 
  if (syzF->rank()<gb.size()+gens,size())
    for (int i=syzF->rank(); i<gb.size()+gens.size(); i++)
      // ??? what does the next line do ???
      syzF->append_schreyer(gb[i-gens.size()]->f.monoms, gens[i-gens.size()]->f.monoms, i); 
#endif

  buffer o;
  for (int i=0; i<g.size(); i++)
    {
      vec v = F4toM2Interface::to_M2_vec(KK, M, g[i]->f, syzF);
      o << "element " << i 
	<< " degree " << g[i]->deg 
	<< " alpha " << g[i]->alpha 
	<< newline << "    ";
      syzF->get_ring()->vec_text_out(o, v);
      o << newline;
    }
  emit(o.str());
}
