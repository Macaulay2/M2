#include "linalgGB.hpp"
#include <vector>
#include "../text_io.hpp"

void LinearAlgebraGB::append_row(monomial m, int elem)
{
  /* Make a new row.  Should we also do
     process_row (and therefore process_vector)  here?

  */
  row_elem r;
  r.monom = m;
  r.elem = elem;
  rows.push_back(r);
}

void LinearAlgebraGB::process_row(int r)
{
  /* Do the multiplication, and process the 
     resulting vector.

     ALSO: this is where we improve row[r] = (m,gbelem)...
     search for any (t,newelem)'s for which t divides m have been done in a prev matrix.
       choose the largest t.  now process (m/t, newelem):
         special multiplication routine?  since newelem uses diff monomial encoding.
 */
}

void LinearAlgebraGB::import_vector(vec f)
{
  /* Creates a sparse_matrix row, and appends it to the current
     matrix being made */
}

void LinearAlgebraGB::process_column(int c)
{
  /* If this column has been handled before, return.
     Otherwise, find a GB element whose lead term
     divides this monomial, and either mark this colum
     as not an initial element, OR append a row
  */
}

void LinearAlgebraGB::append_column(monomial m, int elem, bool islead)
{
  /* Make a new column, insert it.  Also put m into the table of all
     monomials */
}

void LinearAlgebraGB::process_s_pair(SPairSet::spair *p)
{
  /*
    3 cases:
    (a) an spair or ringpair
       find the lcm.
       do append_column for this monomial
       do 2 append_row's
    (b) a skew pair
       find the lcm
       do process_vector (?)
    (c) a generator
       either append_row, or process_vector
  */
     
}

void LinearAlgebraGB::set_comparisons()
{
  /* Sort the monomials */
  /* set the comparison values in the current matrix */
  /* Should we also go thru the matrix and set the values? */
}

void LinearAlgebraGB::make_matrix()
{
  /* loop through all spairs, process,
     then while there are any columns to process, do so,
     then process rows.
     Is this the best order to do it in?  Maybe not...
  */

  SPairSet::spair *p;
  while (p = S.get_next_pair())
    {
      // Append left half, append right half
      // remove pair
    }

  for (;;)
    {
      if (next_row_to_process < rows.size())
	process_row(next_row_to_process++);
      else if (next_col_to_process < columns.size())
	process_column(next_col_to_process++);
      else break;
    }

  /* Now sort the monomials */
  set_comparisons();
}

void LinearAlgebraGB::LU_decompose()
{
}

void LinearAlgebraGB::new_GB_elements()
{
  /* After LU decomposition, loop through each
     row of the matrix.  If the corresponding 
     lead term is not in the initial ideal (or, at least,
     wasn't) then insert GB element (and so update spairs, etc,
     but don't do auto_reduce...)

     If instead the lead term is not new, then keep track of this
     information somehow: place ... into a monheap...
  */
}

void LinearAlgebraGB::s_pair_step()
{
  make_matrix();
  LU_decompose();
  new_GB_elements();
}

enum ComputationStatusCode LinearAlgebraGB::computation_is_complete()
{
  // This handles everything but stop_.always, stop_.degree_limit
  if (stop_.basis_element_limit > 0 && gb.size() > stop_.basis_element_limit) 
    return COMP_DONE_GB_LIMIT;
  if (stop_.pair_limit > 0 && n_pairs_computed > stop_.pair_limit)
    return COMP_DONE_PAIR_LIMIT;
  if (stop_.just_min_gens && n_gens_left == 0)
    return COMP_DONE_MIN_GENS;
  if (stop_.subring_limit > 0 && n_subring > stop_.subring_limit)
    return COMP_DONE_SUBRING_LIMIT;
  if (stop_.use_codim_limit)
    {
#warning "compute the codimension"
      int c = 0; // replace this line
      //int c = codim_of_lead_terms();
      if (c >= stop_.codim_limit)
	return COMP_DONE_CODIM;
    }
  return COMP_COMPUTING;
}

void LinearAlgebraGB::start_computation()
{
  int npairs;
  enum ComputationStatusCode is_done = COMP_COMPUTING;

  for (;;)
    {
      if (system_interruptedFlag) 
	{
	  is_done = COMP_INTERRUPTED;
	  break;
	}

      is_done = computation_is_complete();
      if (is_done != COMP_COMPUTING) break;

      this_degree = S.prepare_next_degree(-1, npairs);
      
      if (npairs == 0)
	{
	  is_done = COMP_DONE;
	  break;
	}
      if (stop_.stop_after_degree && this_degree > stop_.degree_limit->array[0])
	{
	  is_done = COMP_DONE_DEGREE_LIMIT;
	  break;
	}
      fprintf(stdout, "DEGREE %d (npairs %d)\n", this_degree, npairs);

      s_pair_step();
    }
  set_status(is_done);
}

LinearAlgebraGB::LinearAlgebraGB(const Matrix *m,
				  M2_bool collect_syz, 
				  int n_rows_to_keep,
				  M2_arrayint gb_weights,
				  int strategy, 
				  M2_bool use_max_degree,
				  int max_degree)
  : S(&H)
{
}

LinearAlgebraGB::~LinearAlgebraGB()
{
#warning "anything to delete?"
}

LinearAlgebraGB * LinearAlgebraGB::create(const Matrix *m, 
				  M2_bool collect_syz, 
				  int n_rows_to_keep,
				  M2_arrayint gb_weights,
				  int strategy, 
				  M2_bool use_max_degree,
				  int max_degree)
{
  buffer o;
  o << "entering LinearAlgebraGB::create" << newline;
  emit(o.str());
  LinearAlgebraGB *result = new LinearAlgebraGB(m,
						collect_syz,
						n_rows_to_keep,
						gb_weights,
						strategy,
						use_max_degree,
						max_degree);
  return result;
}

/*************************
 ** Top level interface **
 *************************/

ComputationOrNull *LinearAlgebraGB::set_hilbert_function(const RingElement *hf)
{
#if 0
  _hf_orig = hf;
  _hf_diff = RingElement::make_raw(hf->get_ring(), ZERO_RINGELEM);
  _use_hilb = true;
  _hilb_new_elems = true;

  return this;
#endif
  return 0;
}

const MatrixOrNull *LinearAlgebraGB::get_gb()
{
#if 0
  minimalize_gb();
  return minimal_gb->get_gb();
#endif
  return 0;
}

const MatrixOrNull *LinearAlgebraGB::get_mingens()
{
#if 0
  MatrixConstructor mat(_F,0);
  for (vector<gbelem *, gc_allocator<gbelem *> >::iterator i = gb.begin(); i != gb.end(); i++)
    if ((*i)->minlevel == ELEM_POSSIBLE_MINGEN)
      mat.append(originalR->translate_gbvector_to_vec(_F, (*i)->g.f));
  return mat.to_matrix();
#endif
  return 0;
}

const MatrixOrNull *LinearAlgebraGB::get_change()
{
#if 0
  minimalize_gb();
  return minimal_gb->get_change();
#endif
  return 0;
}

const MatrixOrNull *LinearAlgebraGB::get_syzygies()
{
#if 0
  // The (non-minimal) syzygy matrix
  MatrixConstructor mat(_Fsyz, 0);
  for (vector<gbvector *, gc_allocator<gbvector *> >::iterator i = _syz.begin(); i != _syz.end(); i++)
    {
      mat.append(originalR->translate_gbvector_to_vec(_Fsyz, *i));
    }
  return mat.to_matrix();
#endif
  return 0;
}

const MatrixOrNull *LinearAlgebraGB::get_initial(int nparts)
{
#if 0
  minimalize_gb();
  return minimal_gb->get_initial(nparts);
#endif
  return 0;
}

const MatrixOrNull *LinearAlgebraGB::matrix_remainder(const Matrix *m)
{
#if 0
  minimalize_gb();
  return minimal_gb->matrix_remainder(m);
#endif
  return 0;
}

void LinearAlgebraGB::matrix_lift(const Matrix *m,
		 MatrixOrNull **result_remainder,
		 MatrixOrNull **result_quotient
		 )
{
#if 0
  minimalize_gb();
  minimal_gb->matrix_lift(m, result_remainder, result_quotient);
#endif
}

int LinearAlgebraGB::contains(const Matrix *m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
#if 0
  minimalize_gb();
  return minimal_gb->contains(m);
#endif
  return 0;
}

int LinearAlgebraGB::complete_thru_degree() const
  // The computation is complete up through this degree.
{
#if 0
  return _complete_thru_this_degree;
#endif
  return 0;
}

void LinearAlgebraGB::text_out(buffer &o)
  /* This displays statistical information, and depends on the
     gbTrace value */
{
  o << "# pairs computed = " << n_pairs_computed << newline;
  if (gbTrace >= 5 && gbTrace % 2 == 1)
    for (unsigned int i=0; i<gb.size(); i++)
      {
	o << i << '\t';
	//	R->gbvector_text_out(o, F, gb[i].f);
	o << newline;
      }
}

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
