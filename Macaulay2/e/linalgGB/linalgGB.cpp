#include "linalgGB.hpp"

void LinearAlgebraGB::append_row(monomial m, int elem)
{
  /* Make a new row.  Should we also do
     process_row (and therefore process_vector)  here?

  */
  rows.push_back();
  row_elem &r = rows[rows.size()-1];
  r.monom = m;
  r.elem = elem;
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

void LinearAlgebraGB::process_vector(POLY f)
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

void LinearAlgebraGB::append_column(monomial m, int gbelem, bool islead)
{
  /* Make a new column, insert it.  Also put m into the table of all
     monomials */
}

void LinearAlgebraGB::process_s_pair(spair p)
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

void LinearAlgebraGB::make_matrix(spair list)
{
  /* loop through all spairs, process,
     then while there are any columns to process, do so,
     then process rows.
     Is this the best order to do it in?  Maybe not...
  */
 
  for (;;)
    {
      if (next_row_to_process < rows.size())
	process_row(next_row_to_process++);
      else if (next_col_to_process < columns.size())
	process_column(next_col_to_process++);
      else break;
    }

  /* Now sort the monomials */
  set_comparisons(C);
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

void LinearAlgebraGB::s_pair_step(C)
{
  make_matrix(C);
  LU_decompose(C);
  new_GB_elements(C);
}

int LinearAlgebraGB::compute(stop_conditions Stop)
{
  int npairs;
  int is_done = COMP_COMPUTING;
  M2_ring_make_current(C->R);

  for (;;)
    {
      system_spincursor();
      if (system_interrupted) 
	{
	  is_done = COMP_INTERRUPTED;
	  break;
	}

      is_done = is_computation_complete(C,Stop);
      if (is_done != COMP_COMPUTING) break;

      if (error())
	{
	  log_error();
	  is_done = COMP_ERROR;
	  break;
	}
      
      /* If we need to move to the next degree, do it. */
      if (C->S->n_in_degree  == 0)
	{
	  npairs = SPairSet_prepare_next_degree(C->S, &C->this_degree);

	  if (npairs == 0)
	    {
	      is_done = COMP_DONE;
	      break;
	    }
	  if (Stop->degree && C->this_degree > Stop->degree_limit)
	    {
	      is_done = COMP_DONE_DEGREE_LIMIT;
	      break;
	    }
	  fprintf(stdout, "DEGREE %d (npairs %d)\n", C->this_degree, npairs);
	}

      s_pair_step();
    }
  return is_done;
}
