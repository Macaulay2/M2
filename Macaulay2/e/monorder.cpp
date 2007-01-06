// Copyright 1995 Michael E. Stillman.

#include "monorder.hpp"
#include "text_io.hpp"

void mon_order::set_weights(const int *exp, int *m) const
{
  const int *wts = weights;
  for (int i=0; i<nweights; i++)
    {
      int val = 0;
      for (int j=0; j<n; j++)
	val += *wts++ * exp[j];
      *m++ = val;
    }
}
mon_order::mon_order(mon_order_types t, M2_arrayint d,M2_arrayint wts)
{
  ty = t;

  if (d == 0) 
    n = 0;
  else
    n = d->len;

  if (ty == MO1_TRIVIAL ||
      n == 0 ||
      (wts->len / n) * n != wts->len)
    {
      n = 0;
      ty = MO1_TRIVIAL;
      degs = NULL;
      order = NULL;
      inv_order = NULL;
      inv_degs = NULL;
      nweights = 0;
      weights = 0;
      return;
    }

  if (wts == 0)
    INTERNAL_ERROR("mon_order received null pointer");

  nweights = wts->len / n;
  weights = newarray_atomic(int,wts->len);
  for (unsigned int i=0; i<wts->len; i++)
    weights[i] = wts->array[i];

  degs = newarray_atomic(int,n);
  order = newarray(ptr_to_int,n);
  inv_order = newarray(ptr_to_int,n);
  inv_degs = newarray_atomic(int,n);
  for (int r=0; r<n; r++)
    {
      degs[r] = d->array[r];
      inv_degs[r] = d->array[r];
      assert(d->array[r] > 0); // MES: this should be checked higher up?

      order[r] = newarray_atomic(int,n);
      inv_order[r] = newarray_atomic(int,n);
      for (int c=0; c<n; c++)
	{
	  order[r][c] = 0;
	  inv_order[r][c] = 0;
	}
    }
}

mon_order::~mon_order()
{
  deletearray(degs);
  deletearray(inv_degs);
  for (int i=0; i<n; i++)
    {
      deletearray(order[i]);
      deletearray(inv_order[i]);
    }
  deletearray(order);
  deletearray(inv_order);
  deletearray(weights);
}

void mon_order::text_out(buffer &o) const
{
  int i;

  switch (ty) {
  case MO1_GRLEX:
    o << "grlex";
    break;
  case MO1_RLEX:
    o << "rlex";
    break;
  case MO1_GLEX:
    o << "glex";
    break;
  case MO1_LEX:
    o << "lex";
    break;
  case MO1_ELIM:
    o << "elim";
    break;
  case MO1_WTFCN:
    o << "wts";
    break;
  case MO1_PRODUCT:
    o << "product";
    break;
  case MO1_GENERAL:
    o << "general";
    break;
  case MO1_TRIVIAL:
    o << "trivial";
  }
  o << "[";
  if (n>0) o << degs[0];
  for (i=1; i<n; i++) 
    o << "," << degs[i];
  o << "; ";
  if (n>0) o << inv_degs[0];
  for (i=1; i<n; i++) 
    o << "," << inv_degs[i];
  o << "]" << newline;
  
  o << "monomial order matrix" << newline;
  for (i=0; i<n; i++)
    {
      for (int j=0; j<n; j++)
	{
	  o.put(order[i][j], 4);
	  o << ' ';
	}
      o << newline;
    }
  
  o << "inverse monomial order matrix" << newline;
  for (i=0; i<n; i++)
    {
      for (int j=0; j<n; j++)
	{
	  o.put(inv_order[i][j], 4);
	  o << ' ';
	}
      o << newline;
    }
}

mon_order *mon_order::trivial()
{
  M2_arrayint junk = 0, wts = 0;
  mon_order *result = new mon_order(MO1_TRIVIAL, junk, wts);
  return result;
}

mon_order *mon_order::grlex(M2_arrayint degs, M2_arrayint weights)
{
  int n = degs->len;
  if (n == 0) return trivial();

  int allones = 1;
  for (int i=0; i<n; i++)
    if (degs->array[i] != 1) allones = 0;

  mon_order *result;
  if (allones)
    result = new grlex1_mon_order(degs,weights);
  else
    result = new grlex_mon_order(degs,weights);

  for (int r=0; r<n; r++)
    {
      for (int c=0; c<n-r; c++) result->order[r][c] = degs->array[c];
      
      result->inv_order[r][n-r-1] = 1;
      if (r != 0) result->inv_order[r][n-r] = -1;
    }
  return result;
}
mon_order *mon_order::rlex(M2_arrayint degs, M2_arrayint weights)
{
  // WARNING: this order cannot be used with monoid's
  int n = degs->len;
  if (n == 0) return trivial();

  mon_order *result = new mon_order(MO1_RLEX, degs, weights);
  for (int r=0; r<n; r++)
    {
      result->order[r][r] = -1;
      result->inv_order[r][r] = -1;
      result->inv_degs[r] = 1;
    }
  return result;
}

mon_order *mon_order::glex(M2_arrayint degs, M2_arrayint weights)
{
  int n = degs->len;
  if (n == 0) return trivial();

  mon_order *result = new mon_order(MO1_GLEX, degs, weights);

  int r, c;
  for (c=0; c<n; c++) result->order[0][c] = degs->array[c];
  for (r=1; r<n; r++) result->order[r][r-1] = degs->array[r-1];
  
  result->inv_order[n-1][0] = 1;
  for (c=1; c<n; c++) result->inv_order[n-1][c] = -1;
  for (r=0; r<n-1; r++) result->inv_order[r][r+1] = 1;

  return result;
}
mon_order *mon_order::lex(M2_arrayint degs, M2_arrayint weights)
{
  // WARNING: this order cannot be used with monoid's
  int n = degs->len;
  if (n == 0) return trivial();

  mon_order *result = new mon_order(MO1_LEX, degs, weights);

  for (int r=0; r<n; r++)
    {
      result->order[r][r] = 1;
      result->inv_order[r][r] = 1;
      result->inv_degs[r] = 1;
    }
  return result;
}
mon_order *mon_order::elim(M2_arrayint degs, 
			   unsigned int i, 
			   M2_arrayint weights)
{
  if ((i <= 0) || i >= degs->len) return grlex(degs,weights);

  mon_order *result = new elim_mon_order(degs,i,weights);
  unsigned int n = degs->len;

  unsigned int r, c;
  for (r=0; r<i; r++)
    result->order[0][r] = degs->array[r];

  for (r=1; r<=n-i; r++)
    for (c=0; c<n-i-r+1; c++)
      result->order[r][i+c] = degs->array[i+c];

  for (r=n-i+1; r<n; r++)
    for (c=0; c<n-r; c++)
      result->order[r][c] = degs->array[c];

  // Now set the inverse
  for (r=0; r<i-1; r++) result->inv_order[r][n-1-r] = 1;
  for (r=1; r<i; r++) result->inv_order[r][n-r] = -1;
  result->inv_order[i-1][0] = 1;

  for (r=i; r<n; r++) result->inv_order[r][n-r] = 1;
  for (r=i+1; r<n; r++) result->inv_order[r][n-r+1] = -1;
  return result;
}

mon_order *mon_order::product(M2_arrayint degs, M2_arrayint blocks,
			      M2_arrayint weights)
{
  int r, c;
  unsigned int b;

  int n = degs->len;
  if (n == 0) return trivial();

  int sum = 0;

  for (b=0; b<blocks->len; b++)
    {
      if (blocks->array[b] < 0)
	INTERNAL_ERROR("product order: a block has negative number of variables!");
      sum += blocks->array[b];
    }
  if (sum != n) 
    {
      ERROR("product order: expected same number of variables as degree vectors");
      return NULL;
    }

  mon_order *result = new product_mon_order(degs, blocks, weights);

  int start = 0;
  for (b=0; b<blocks->len; b++)
    {
      int nv = blocks->array[b];
      for (r=0; r<nv; r++)
	for (c=0; c<nv-r; c++)
	  result->order[start+r][start+c] = degs->array[start+c];

      for (r=0; r<nv; r++) result->inv_order[start+r][start+nv-r-1] = 1;
      for (r=1; r<nv; r++) result->inv_order[start+r][start+nv-r] = -1;

      start += nv;
    }
  return result;
}

mon_order *mon_order::product(const mon_order *m1, const mon_order *m2)
{
  emit_line("mon_order::product called");
#if 0
//   intarray degs;
//   degs.copy(m1->n_vars(), m1->inv_degs);
//   degs.copy(m2->n_vars(), m2->inv_degs);
//   intarray weights;
//   mon_order *result = new mon_order(MO1_PRODUCT, degs, weights);
//   int n = degs->len;
//   int n1 = m1->n_vars();
// 
//   int r, c;
//   for (r=0; r<n1; r++)
//     for (c=0; c<n1; c++)
//       {
// 	result->order[r][c] = m1->order[r][c];
// 	result->inv_order[r][c] = m1->inv_order[r][c];
//       }
// 
//   for (r=n1; r<n; r++)
//     for (c=n1; c<n; c++)
//       {
// 	result->order[r][c] = m2->order[r-n1][c-n1];
// 	result->inv_order[r][c] = m2->inv_order[r-n1][c-n1];
//       }
// 
//   return result;
#endif
  INTERNAL_ERROR("mon_order::product called");
  return NULL;
}

mon_order *mon_order::elim_product(const mon_order * /*m1*/, const mon_order * /*m2*/)
{
  INTERNAL_ERROR("mon_order::elim_product called");
  return NULL;
}

mon_order *mon_order::graded_product(const mon_order * /*m1*/, const mon_order * /*m2*/)
{
  INTERNAL_ERROR("mon_order::graded_product called");
  return NULL;
}

mon_order *mon_order::general_order(M2_arrayint degs,
				    M2_arrayint order, 
				    M2_arrayint invorder,
				    M2_arrayint invdegs)
{
  emit_line("general_order called");
#if 0
//   if (degs->len == 0) return trivial();
//   int n = invdegs->len;
//   if (order->len != n*n || invorder->len != n*n || n <= 0)
//     return NULL;
// 
//   intarray weights;
//   mon_order *result = new mon_order(MO1_GENERAL, degs, weights);
//   
//   int r,c;
//   for (r=0; r<n; r++) result->inv_degs[r] = invdegs->array[r];
//   for (r=0; r<n; r++)
//     for (c=0; c<n; c++)
//       {
// 	result->order[r][c] = order->array[n*c+r];
// 	result->inv_order[r][c] = invorder->array[n*c+r];
//       }
//   return result;
#endif
  INTERNAL_ERROR("mon_order::general_order called");
  return NULL;
}

void mon_order::encode(const int *exp, int *m) const
{
  set_weights(exp,m);
  m += nweights;
  for (int i=0; i<n; i++)
    {
      int val = 0;
      int *wt = order[i];
      for (int j=0; j<n; j++)
	val += exp[j] * wt[j];
      m[i] = val;
    }
}

void mon_order::decode(const int *m, int *exp) const
{
  for (int i=0; i<n; i++)
    {
      int val = 0;
      int *wt = inv_order[i];
      for (int j=0; j<n; j++)
	val += m[j] * wt[j];
      exp[i] = val / inv_degs[i];
    }
}

grlex_mon_order::grlex_mon_order(M2_arrayint degs0, M2_arrayint weights0)
: mon_order(MO1_GRLEX, degs0, weights0)
{
}
grlex_mon_order::~grlex_mon_order()
{
}

void grlex_mon_order::text_out(buffer &o) const
{
  o << "grlex[" << degs[0];
  for (int i=1; i<n; i++)
    o << "," << degs[i];
  o << "]";
    
}
void grlex_mon_order::encode(const int *exp, int *m) const
{
  set_weights(exp,m);
  m += nweights;

  int sum = 0;
  for (int i=0; i<n; i++)
    {
      sum += exp[i] * degs[i];
      m[n-i-1] = sum;
    }
}

void grlex_mon_order::decode(const int *m, int *exp) const
{
  exp[0] = m[n-1] / degs[0];
  for (int i=n-1; i>=1; i--)
    exp[i] = (m[n-i-1] - m[n-i]) / degs[i];
}

grlex1_mon_order::grlex1_mon_order(M2_arrayint degs0, M2_arrayint weights0)
: mon_order(MO1_GRLEX, degs0, weights0)
{
}
grlex1_mon_order::~grlex1_mon_order()
{
}

void grlex1_mon_order::text_out(buffer &o) const
{
  o << "grlex1[" << degs[0];
  for (int i=1; i<n; i++)
    o << "," << degs[i];
  o << "]";
}
void grlex1_mon_order::encode(const int *exp, int *m) const
{
  set_weights(exp,m);
  m += nweights;

  int sum = 0;
  for (int i=0; i<n; i++)
    {
      sum += exp[i];
      m[n-i-1] = sum;
    }
}

void grlex1_mon_order::decode(const int *m, int *exp) const
{
  exp[0] = m[n-1];
  for (int i=n-1; i>=1; i--)
    exp[i] = (m[n-i-1] - m[n-i]);
}

//---- Product order of several rev lex blocks ---//

product_mon_order::product_mon_order(M2_arrayint degs0, M2_arrayint blk,
				     M2_arrayint weights0)
: mon_order(MO1_PRODUCT, degs0, weights0),
  nblocks(blk->len),
  blocks(newarray_atomic(int,blk->len))
{
  for (int i=0; i<nblocks; i++)
    blocks[i] = blk->array[i];
}
product_mon_order::~product_mon_order()
{
  deletearray(blocks);
}

void product_mon_order::text_out(buffer &o) const
{
  int i;
  o << "product[" << degs[0];
  for (i=1; i<n; i++)
    o << "," << degs[i];
  o << ", blocks = " << blocks[0];
  for (i=1; i<nblocks; i++)
    o << "," << blocks[i];
  o << "]";
    
}
void product_mon_order::encode(const int *exp, int *m) const
{
  set_weights(exp,m);
  m += nweights;

  const int *d = degs;
  for (int i=0; i<nblocks; i++)
    {
      int sum = 0;
      int r = blocks[i];
      for (int j=0; j<r; j++)
	{
	  sum += exp[j] * d[j];
	  m[r-j-1] = sum;
	}

      exp += r;
      d += r;
      m += r;
    }
}

void product_mon_order::decode(const int *m, int *exp) const
{
  const int *d = degs;
  for (int i=0; i<nblocks; i++)
    {
      int r = blocks[i];
      exp[0] = m[r-1] / d[0];
      for (int j=r-1; j>=1; j--)
	exp[j] = (m[r-j-1] - m[r-j]) / d[j];
      
      exp += r;
      d += r;
      m += r;
    }
}

//---- Elimination order refined by graded rev lex ---//

elim_mon_order::elim_mon_order(M2_arrayint degs0, 
			       unsigned int n0, 
			       M2_arrayint weights0)
: mon_order(MO1_ELIM, degs0, weights0),
  nelim(n0)
{
}
elim_mon_order::~elim_mon_order()
{
}

void elim_mon_order::text_out(buffer &o) const
{
  int i;
  o << "elim[" << degs[0];
  for (i=1; i<n; i++)
    o << "," << degs[i];
  o << ", elim = " << nelim;
  o << "]";
}
void elim_mon_order::encode(const int *exp, int *m) const
{
  set_weights(exp,m);
  m += nweights;

  const int *d = degs;
  int i, sum = 0;

  for (i=0; i<nelim-1; i++)
    {
      sum += exp[i] * d[i];
      m[n-i-1] = sum;
    }
  m[0] = sum + exp[nelim-1] * d[nelim-1];
  sum = 0;
  for (i=nelim; i<n; i++)
    {
      sum += exp[i] * d[i];
      m[n-i] = sum;
    }
}

void elim_mon_order::decode(const int *m, int *exp) const
{
  const int *d = degs;
  int i;

  exp[nelim] = m[n-nelim] / d[nelim];
  for (i=n-1; i>nelim; i--)
    exp[i] = (m[n-i] - m[n-i+1]) / d[i];

  if (nelim == 1) 
    exp[0] = m[0] / degs[0];
  else
    {
      exp[0] = m[n-1] / degs[0];
      for (i=nelim-2; i>=1; i--)
	exp[i] = (m[n-i-1] - m[n-i]) / d[i];
      exp[nelim-1] = (m[0] - m[n-nelim+1]) / d[nelim-1];
    }
}

mon_order * IM2_mon_order::grab_mon_order()
{ 
  mon_order *result = val;
  val = 0;
  if (result == 0)
    ERROR("cannot reuse engine monomial order");
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
