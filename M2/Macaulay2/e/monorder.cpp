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
mon_order::mon_order(mon_order_types t, const intarray &d,const intarray &wts)
: ty(t), n(d.length())
{
  if (ty == MO_TRIVIAL ||
      n == 0 ||
      (wts.length() / n) * n != wts.length())
    {
      ty = MO_TRIVIAL;
      degs = NULL;
      order = NULL;
      inv_order = NULL;
      inv_degs = NULL;
      return;
    }

  nweights = wts.length() / n;
  weights = new int[wts.length()];
  for (int i=0; i<wts.length(); i++)
    weights[i] = wts[i];

  degs = new int[n];
  order = new ptr_to_int[n];
  inv_order = new ptr_to_int[n];
  inv_degs = new int[n];
  for (int r=0; r<n; r++)
    {
      degs[r] = d[r];
      inv_degs[r] = d[r];
      assert(d[r] > 0); // MES: this should be checked higher up?

      order[r] = new int[n];
      inv_order[r] = new int[n];
      for (int c=0; c<n; c++)
	{
	  order[r][c] = 0;
	  inv_order[r][c] = 0;
	}
    }
}

mon_order::~mon_order()
{
  delete [] inv_degs;
  for (int i=0; i<n; i++)
    {
      delete [] order[i];
      delete [] inv_order[i];
    }
  delete [] order;
  delete [] inv_order;
  delete [] weights;
}

void mon_order::text_out(buffer &o) const
{
  int i;

  switch (ty) {
  case MO_GRLEX:
    o << "grlex";
    break;
  case MO_RLEX:
    o << "rlex";
    break;
  case MO_GLEX:
    o << "glex";
    break;
  case MO_LEX:
    o << "lex";
    break;
  case MO_ELIM:
    o << "elim";
    break;
  case MO_WTFCN:
    o << "wts";
    break;
  case MO_PRODUCT:
    o << "product";
    break;
  case MO_GENERAL:
    o << "general";
    break;
  case MO_TRIVIAL:
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
  intarray junk, wts;
  mon_order *result = new mon_order(MO_TRIVIAL, junk, wts);
  return result;
}

mon_order *mon_order::grlex(const intarray &degs, const intarray &weights)
{
  int n = degs.length();
  if (n == 0) return trivial();

  int allones = 1;
  for (int i=0; i<n; i++)
    if (degs[i] != 1) allones = 0;

  mon_order *result;
  if (allones)
    result = new grlex1_mon_order(degs,weights);
  else
    result = new grlex_mon_order(degs,weights);

  for (int r=0; r<n; r++)
    {
      for (int c=0; c<n-r; c++) result->order[r][c] = degs[c];
      
      result->inv_order[r][n-r-1] = 1;
      if (r != 0) result->inv_order[r][n-r] = -1;
    }
  return result;
}
mon_order *mon_order::rlex(const intarray &degs, const intarray &weights)
{
  // WARNING: this order cannot be used with monoid's
  int n = degs.length();
  if (n == 0) return trivial();

  mon_order *result = new mon_order(MO_RLEX, degs, weights);
  for (int r=0; r<n; r++)
    {
      result->order[r][r] = -1;
      result->inv_order[r][r] = -1;
      result->inv_degs[r] = 1;
    }
  return result;
}

mon_order *mon_order::glex(const intarray &degs, const intarray &weights)
{
  int n = degs.length();
  if (n == 0) return trivial();

  mon_order *result = new mon_order(MO_GLEX, degs, weights);

  int r, c;
  for (c=0; c<n; c++) result->order[0][c] = degs[c];
  for (r=1; r<n; r++) result->order[r][r-1] = degs[r-1];
  
  result->inv_order[n-1][0] = 1;
  for (c=1; c<n; c++) result->inv_order[n-1][c] = -1;
  for (r=0; r<n-1; r++) result->inv_order[r][r+1] = 1;

  return result;
}
mon_order *mon_order::lex(const intarray &degs, const intarray &weights)
{
  // WARNING: this order cannot be used with monoid's
  int n = degs.length();
  if (n == 0) return trivial();

  mon_order *result = new mon_order(MO_LEX, degs, weights);

  for (int r=0; r<n; r++)
    {
      result->order[r][r] = 1;
      result->inv_order[r][r] = 1;
      result->inv_degs[r] = 1;
    }
  return result;
}
mon_order *mon_order::elim(const intarray &degs, int i, const intarray &weights)
{
  if ((i <= 0) || i >= degs.length()) return grlex(degs,weights);

  mon_order *result = new elim_mon_order(degs,i,weights);
  int n = degs.length();

  int r, c;
  for (r=0; r<i; r++)
    result->order[0][r] = degs[r];

  for (r=1; r<=n-i; r++)
    for (c=0; c<n-i-r+1; c++)
      result->order[r][i+c] = degs[i+c];

  for (r=n-i+1; r<n; r++)
    for (c=0; c<n-r; c++)
      result->order[r][c] = degs[c];

  // Now set the inverse
  for (r=0; r<i-1; r++) result->inv_order[r][n-1-r] = 1;
  for (r=1; r<i; r++) result->inv_order[r][n-r] = -1;
  result->inv_order[i-1][0] = 1;

  for (r=i; r<n; r++) result->inv_order[r][n-r] = 1;
  for (r=i+1; r<n; r++) result->inv_order[r][n-r+1] = -1;
  return result;
}

mon_order *mon_order::product(const intarray &degs, const intarray &blocks,
			      const intarray &weights)
{
  int r, c, b;

  int n = degs.length();
  if (n == 0) return trivial();

  int sum = 0;

  for (b=0; b<blocks.length(); b++)
    {
      if (blocks[b] < 0) return NULL;
      sum += blocks[b];
    }
  if (sum != n) return NULL;

  mon_order *result = new product_mon_order(degs, blocks, weights);

  int start = 0;
  for (b=0; b<blocks.length(); b++)
    {
      int nv = blocks[b];
      for (r=0; r<nv; r++)
	for (c=0; c<nv-r; c++)
	  result->order[start+r][start+c] = degs[start+c];

      for (r=0; r<nv; r++) result->inv_order[start+r][start+nv-r-1] = 1;
      for (r=1; r<nv; r++) result->inv_order[start+r][start+nv-r] = -1;

      start += nv;
    }
  return result;
}

mon_order *mon_order::product(const mon_order *m1, const mon_order *m2)
{
  emit_line("mon_order::product called");
  intarray degs;
  degs.copy(m1->n_vars(), m1->inv_degs);
  degs.copy(m2->n_vars(), m2->inv_degs);
  intarray weights;
  mon_order *result = new mon_order(MO_PRODUCT, degs, weights);
  int n = degs.length();
  int n1 = m1->n_vars();

  int r, c;
  for (r=0; r<n1; r++)
    for (c=0; c<n1; c++)
      {
	result->order[r][c] = m1->order[r][c];
	result->inv_order[r][c] = m1->inv_order[r][c];
      }

  for (r=n1; r<n; r++)
    for (c=n1; c<n; c++)
      {
	result->order[r][c] = m2->order[r-n1][c-n1];
	result->inv_order[r][c] = m2->inv_order[r-n1][c-n1];
      }

  return result;
}

mon_order *mon_order::elim_product(const mon_order * /*m1*/, const mon_order * /*m2*/)
{
  // MES
  return NULL;
}

mon_order *mon_order::graded_product(const mon_order * /*m1*/, const mon_order * /*m2*/)
{
  // MES
  return NULL;
}

mon_order *mon_order::general_order(const intarray &degs,
				    const intarray &order, 
				    const intarray &invorder,
				    const intarray &invdegs)
{
  emit_line("general_order called");
  if (degs.length() == 0) return trivial();
  int n = invdegs.length();
  if (order.length() != n*n || invorder.length() != n*n || n <= 0)
    return NULL;

  intarray weights;
  mon_order *result = new mon_order(MO_GENERAL, degs, weights);
  
  int r,c;
  for (r=0; r<n; r++) result->inv_degs[r] = invdegs[r];
  for (r=0; r<n; r++)
    for (c=0; c<n; c++)
      {
	result->order[r][c] = order[n*c+r];
	result->inv_order[r][c] = invorder[n*c+r];
      }
  return result;
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

grlex_mon_order::grlex_mon_order(const intarray &degs, const intarray &weights)
: mon_order(MO_GRLEX, degs, weights)
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

grlex1_mon_order::grlex1_mon_order(const intarray &degs, const intarray &weights)
: mon_order(MO_GRLEX, degs, weights)
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

product_mon_order::product_mon_order(const intarray &degs, const intarray &blk,
				     const intarray &weights)
: mon_order(MO_PRODUCT, degs, weights),
  nblocks(blk.length()),
  blocks(new int[blk.length()])
{
  for (int i=0; i<nblocks; i++)
    blocks[i] = blk[i];
}
product_mon_order::~product_mon_order()
{
  delete [] blocks;
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

elim_mon_order::elim_mon_order(const intarray &degs, int n, const intarray &weights)
: mon_order(MO_ELIM, degs, weights),
  nelim(n)
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
