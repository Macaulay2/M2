// Copyright 1996 Michael E. Stillman.

#include "det.hpp"
#include "text-io.hpp"
#include "interrupted.hpp"
#include "comb.hpp"

DetComputation::DetComputation(const Matrix *M0,
                               int p0,
                               bool do_exterior0,
                               int strategy0)
    : R(M0->get_ring()),
      M(M0),
      done(false),
      p(p0),
      do_exterior(do_exterior0),
      strategy(strategy0),
      row_set(NULL),
      col_set(NULL),
      this_row(0),
      this_col(0),
      D(0),
      dynamic_cache(0)
{
  if (do_exterior)
    {
      F = M->rows()->exterior(p);
      FreeModule *G = M->cols()->exterior(p);
      int *deg = R->degree_monoid()->make_new(M->degree_shift());
      R->degree_monoid()->power(deg, p, deg);
      result = MatrixConstructor(F, G, deg);
      R->degree_monoid()->remove(deg);
    }
  else
    {
      F = R->make_FreeModule(1);
      result = MatrixConstructor(F, 0);
    }

  // Handle trivial cases
  if (p < 0)
    {
      // In either case, want a zero matrix
      done = true;
      return;
    }
  if (p == 0)
    {
      // We want a single element which is '1'
      if (do_exterior)
        result.set_entry(0, 0, R->one());
      else
        result.append(R->make_vec(0, R->one()));
      done = true;
      return;
    }
  if (p > M->n_rows() || p > M->n_cols())
    {
      // Zero matrix in either case
      done = true;
      return;
    }
  done = false;

  row_set = newarray_atomic(size_t, p);
  col_set = newarray_atomic(size_t, p);

  for (size_t i = 0; i < p; i++)
    {
      row_set[i] = i;
      col_set[i] = i;
    }

  D = newarray(ring_elem *, p);
  for (size_t i = 0; i < p; i++)
    {
      D[i] = newarray(ring_elem, p);
      for (size_t j = 0; j < p; j++) D[i][j] = ZERO_RINGELEM;
    }
  if (strategy == DET_DYNAMIC) {
    dynamic_cache.resize(p);
    make_dynamic_cache();
  }
}

DetComputation::~DetComputation()
{
  freemem(row_set);
  freemem(col_set);

  if (D)
    {
      for (size_t i = 0; i < p; i++) freemem(D[i]);
      freemem(D);
    }
}

void DetComputation::make_dynamic_cache() {
  // Traverse through matrix entries, find nonzero entries
  int nonzero = -1;
  for(int i = 0; i < M->n_rows(); ++i) {
    bool flag = false;
    for(int j = 0; j < M->n_cols(); ++j) {
      if(!R->is_zero(M->elem(i,j))) {
        if(!flag) { flag = true; dynamic_cache[0][++nonzero] = { }; row_lookup[i]=nonzero; }
        dynamic_cache[0][nonzero][{ {i}, {j} }] = M->elem(i,j);
      }
    }
  }
  int n_nonzero_rows = dynamic_cache[0].size();
  for(int minor_size = 1; minor_size < p; ++minor_size) {
    for(int top_row = p-(minor_size+1); top_row <= n_nonzero_rows-minor_size; ++top_row) {
      dynamic_cache[minor_size].insert({ top_row, {} });
      for(const auto& [pp, map]: dynamic_cache[minor_size-1]) {
        if(pp <= top_row) { continue; } // top_row wouldn't be the top row, so skip
        for(auto x: dynamic_cache[0][top_row]) {
          for(const auto& [Didx, Dval]: map) {
            // Check if x and D live on distinct columns
            const std::vector<int>& Dcols = Didx.second;
            int xcol = x.first.second[0];
            auto col_find = find(Dcols.begin(), Dcols.end(), xcol);
            if(col_find != Dcols.end()) { continue; } // xcol found in Dcols, so skip
            // if no skip, compute a term in the cofactor
            ColRowIndices newKey = Didx;
            newKey.first.insert(newKey.first.begin(), x.first.first[0]);
            // Get iterator to future location of xcol
            auto xcol_position = std::upper_bound(newKey.second.begin(), newKey.second.end(), xcol);
            newKey.second.insert(xcol_position, xcol);
            bool negate = ((xcol_position - newKey.second.begin())%2 != 0);
            // Insert, add or negate cofactor term
            auto search = dynamic_cache[minor_size][top_row].find(newKey);
            if(search == dynamic_cache[minor_size][top_row].end()) { // not found
              dynamic_cache[minor_size][top_row].insert({ newKey, R->mult(x.second, Dval) });
              if(negate) { R->negate_to(dynamic_cache[minor_size][top_row][newKey]); }
            }
            else { // found
              if(!negate) { R->add_to(dynamic_cache[minor_size][top_row][newKey], R->mult(x.second, Dval)); }
              else { R->subtract_to(dynamic_cache[minor_size][top_row][newKey], R->mult(x.second, Dval)); }
            }
          }          
        }
      }
    }
  }
}

int DetComputation::step()
// Compute one more determinant of size p.
// increments I and/or J and updates 'dets', 'table'.
{
  if (done) return COMP_DONE;

  ring_elem r;

  if (strategy == DET_BAREISS)
    {
      get_minor(row_set, col_set, p, D);
      r = bareiss_det();
    }
  else if (strategy == DET_DYNAMIC) {
    std::vector<int> row_vec(p), col_vec(p);
    for(int i = 0; i < p; ++i) {
      row_vec[i] = static_cast<int>(row_set[i]);
      col_vec[i] = static_cast<int>(col_set[i]);
    }
    // Find row number
    const Subdeterminant& map = dynamic_cache[p-1][row_lookup[row_vec[0]]];
    auto it = map.find({ row_vec, col_vec });
    if(it != map.end()) { r = it->second; }
    else{ r = ZERO_RINGELEM; }
  }
  else
    r = calc_det(row_set, col_set, p);

  if (!R->is_zero(r))
    {
      if (do_exterior)
        result.set_entry(this_row, this_col, r);
      else
        result.append(R->make_vec(0, r));
    }
  else
    R->remove(r);

  this_row++;
  if (!Subsets::increment(M->n_rows(), p, row_set))
    {
      // Now we increment column
      if (!Subsets::increment(M->n_cols(), p, col_set))
        {
          done = true;
          return COMP_DONE;
        }
      // Now set the row set back to initial value
      this_col++;
      this_row = 0;
      for (size_t i = 0; i < p; i++) row_set[i] = i;
    }
  return COMP_COMPUTING;
}

void DetComputation::clear()
{
  if (do_exterior) return;
  result = MatrixConstructor(F, 0);
}

void DetComputation::set_next_minor(const int *rows, const int *cols)
{
  if (do_exterior) return;
  if (rows != NULL && Subsets::isValid(M->n_rows(), p, rows))
    for (size_t i = 0; i < p; i++) row_set[i] = rows[i];
  else
    for (size_t i = 0; i < p; i++) row_set[i] = i;

  if (cols != NULL && Subsets::isValid(M->n_cols(), p, cols))
    for (size_t i = 0; i < p; i++) col_set[i] = cols[i];
  else
    for (size_t i = 0; i < p; i++) col_set[i] = i;
}

int DetComputation::calc(int nsteps)
{
  for (;;)
    {
      int r = step();
      if (M2_gbTrace >= 3) emit_wrapped(".");
      if (r == COMP_DONE) return COMP_DONE;
      if (--nsteps == 0) return COMP_DONE_STEPS;
      if (system_interrupted()) return COMP_INTERRUPTED;
    }
}

void DetComputation::get_minor(size_t *r, size_t *c, int p0, ring_elem **D0)
{
  for (size_t i = 0; i < p0; i++)
    for (size_t j = 0; j < p0; j++)
      D0[i][j] = M->elem(static_cast<int>(r[i]), static_cast<int>(c[j]));
}

bool DetComputation::get_pivot(ring_elem **D0,
                               size_t r,
                               ring_elem &pivot,
                               size_t &pivot_col)
// Get a non-zero column 0..r in the r th row.
{
  // MES: it would be worthwhile to find a good pivot.
  for (size_t c = 0; c <= r; c++)
    if (!R->is_zero(D0[r][c]))
      {
        pivot_col = c;
        pivot = D0[r][c];
        return true;
      }
  return false;
}

ring_elem DetComputation::detmult(ring_elem f1,
                                  ring_elem g1,
                                  ring_elem f2,
                                  ring_elem g2,
                                  ring_elem d)
{
  ring_elem a = R->mult(f1, g1);
  ring_elem b = R->mult(f2, g2);
  R->subtract_to(a, b);
  if (!R->is_zero(d))
    {
      ring_elem tmp = R->divide(a, d);  // exact division
      R->remove(a);
      a = tmp;
    }
  R->remove(g1);
  return a;
}

void DetComputation::gauss(ring_elem **D0,
                           size_t i,
                           size_t r,
                           size_t pivot_col,
                           ring_elem lastpivot)
{
  ring_elem f = D0[i][pivot_col];
  ring_elem pivot = D0[r][pivot_col];

  for (size_t c = 0; c < pivot_col; c++)
    D0[i][c] = detmult(pivot, D0[i][c], f, D0[r][c], lastpivot);

  for (size_t c = pivot_col + 1; c <= r; c++)
    D0[i][c - 1] = detmult(pivot, D0[i][c], f, D0[r][c], lastpivot);

  R->remove(f);
}

ring_elem DetComputation::bareiss_det()
{
  // Computes the determinant of the p by p matrix D. (dense form).
  int sign = 1;
  size_t pivot_col;

  ring_elem pivot = R->from_long(0);
  ring_elem lastpivot = R->from_long(0);

  for (size_t r = p - 1; r >= 1; --r)
    {
      R->remove(lastpivot);
      lastpivot = pivot;
      if (!get_pivot(D, r, pivot, pivot_col))  // sets pivot_col and pivot
        {
          // Remove the rest of D.
          for (size_t i = 0; i <= r; i++)
            for (size_t j = 0; j <= r; j++) R->remove(D[i][j]);
          R->remove(lastpivot);
          return R->from_long(0);
        }
      for (size_t i = 0; i < r; i++) gauss(D, i, r, pivot_col, lastpivot);

      if (((r + pivot_col) % 2) == 1)
        sign = -sign;  // MES: do I need to rethink this logic?

      for (size_t c = 0; c <= r; c++)
        if (c != pivot_col)
          R->remove(D[r][c]);
        else
          D[r][c] = ZERO_RINGELEM;
    }

  R->remove(pivot);
  R->remove(lastpivot);
  ring_elem r = D[0][0];
  D[0][0] = ZERO_RINGELEM;

  if (sign < 0) R->negate_to(r);

  return r;
}
ring_elem DetComputation::calc_det(size_t *r, size_t *c, int p0)
// Compute the determinant of the minor with rows r[0]..r[p0-1]
// and columns c[0]..c[p0-1].
{
  if (p0 == 1) return M->elem(static_cast<int>(r[0]), static_cast<int>(c[0]));
  ring_elem answer = R->from_long(0);

  int negate = 1;
  for (int i = p0 - 1; i >= 0; i--)
    {
      std::swap(c[i], c[p0 - 1]);
      negate = !negate;
      ring_elem g =
          M->elem(static_cast<int>(r[p0 - 1]), static_cast<int>(c[p0 - 1]));
      if (R->is_zero(g))
        {
          R->remove(g);
          continue;
        }
      ring_elem h = calc_det(r, c, p0 - 1);
      ring_elem gh = R->mult(g, h);
      R->remove(g);
      R->remove(h);
      if (negate)
        R->subtract_to(answer, gh);
      else
        R->add_to(answer, gh);
    }

  // pulling out the columns has disordered c. Fix it.

  size_t temp = c[p0 - 1];
  for (size_t i = p0 - 1; i > 0; i--) c[i] = c[i - 1];
  c[0] = temp;

  return answer;
}

Matrix /* or null */ *Matrix::exterior(int p, int strategy) const
{
  if (strategy == DET_BAREISS && get_ring()->get_precision() > 0)
    {
      ERROR(
          "determinant computations over RR or CC requires Strategy=>Cofactor");
      return 0;
    }
  DetComputation *d = new DetComputation(this, p, 1, strategy);
  d->calc(-1);
  Matrix *result = d->determinants();
  freemem(d);
  return result;
}

Matrix /* or null */ *Matrix::minors(int p, int strategy) const
{
  if (strategy == DET_BAREISS && get_ring()->get_precision() > 0)
    {
      ERROR(
          "determinant computations over RR or CC requires Strategy=>Cofactor");
      return 0;
    }
  DetComputation *d = new DetComputation(this, p, 0, strategy);
  d->calc(-1);
  Matrix *result = d->determinants();
  freemem(d);
  return result;
}

Matrix /* or null */ *Matrix::minors(
    int p,
    int strategy,
    int n_to_compute,             // -1 means all
    M2_arrayintOrNull first_row,  // possibly NULL
    M2_arrayintOrNull first_col   // possibly NULL
    ) const
{
  if (strategy == DET_BAREISS && get_ring()->get_precision() > 0)
    {
      ERROR(
          "determinant computations over RR or CC requires Strategy=>Cofactor");
      return 0;
    }
  if (first_row != 0 || first_col != 0)
    {
      // Make sure these are the correct size, and both are given
      if (first_row == 0 || first_row->len != p)
        {
          ERROR("row index set inappropriate");
          return 0;
        }
      if (first_col == 0 || first_col->len != p)
        {
          ERROR("column index set inappropriate");
          return 0;
        }
    }
  DetComputation *d = new DetComputation(this, p, 0, strategy);
  if (first_row != 0 && first_col != 0)
    d->set_next_minor(first_row->array, first_col->array);
  d->calc(n_to_compute);
  Matrix *result = d->determinants();
  freemem(d);
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
