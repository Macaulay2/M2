// Copyright 1998 by Michael Stillman

#include "Emonorder.hpp"

EMonomialOrder::EMonomialOrder()
  : nvars(0),
    ncommvars(0),
    nslots(0),
    componentloc(-1),
    nblocks(0),
    order(NULL),
    n_nc_blocks(0)
{
}

EMonomialOrder::~EMonomialOrder()
{
  for (int i=nblocks-1; i>=0; i--)
    {
      delete [] order[i]->weights;
      delete order[i];
    }
}

/////////////////////////////////////
// Creation of new monomial orders //
/////////////////////////////////////

EMonomialOrder *EMonomialOrder::make()
{
  EMonomialOrder *result = new EMonomialOrder;
  return result;
}

void EMonomialOrder::append_block(int nzeros, mon_order_node *b)
{
  int *wts;
  int i;
  switch (b->typ)
    {
    case MO_LEX:
      lex(b->n, b->isgroup);
      break;
    case MO_REVLEX:
      revlex(b->n, b->isgroup);
      break;
    case MO_WTREVLEX:
      revlexWeights(b->nweights, b->weights, b->isgroup);
      break;
    case MO_WTFCN:
      wts = new int[nzeros+b->nweights];
      for (i=0; i<nzeros; i++)
	wts[i] = 0;
      for (i=nzeros; i<nzeros+b->nweights; i++)
	wts[i] = b->weights[i];
      weightFunction(nzeros + b->nweights, wts);
      delete [] wts;
      break;
    case MO_NC_LEX:
      NClex(b->n);
      break;
    default:
      // MES
      break;
    }
}

EMonomialOrder *EMonomialOrder::clone() const
{
  EMonomialOrder *result = make();
  for (int i=0; i<nblocks; i++)
    result->append_block(0,order[i]);
  result->componentloc = componentloc;
  return result;
}

EMonomialOrder *EMonomialOrder::product(const EMonomialOrder *mo2)
{
  int i;
  int n = this->nvars;
  int ns = this->nslots;
  int nb = this->nblocks;
  mon_order_node **oldblocks = order;

  this->nslots = 0;
  this->order = NULL;
  this->nvars = 0;
  this->nblocks = 0;

  for (i=0; i<nb; i++)
    append_block(0, oldblocks[i]);
  for (i=0; i<mo2->nblocks; i++)
    append_block(n, mo2->order[i]);
  componentloc = ns + mo2->componentloc;
  delete [] oldblocks;
  return this;
}

EMonomialOrder *EMonomialOrder::revlex(int nvars, bool isgroup)
{
  if (nvars == 0) return this;
  mon_order_node *b = new mon_order_node;
  b->typ = MO_REVLEX;
  b->n = nvars;
  b->nslots = nvars;
  b->first_exp = this->nvars;
  b->first_slot = this->nslots;
  b->nweights = 0;
  b->weights = NULL;  // NOT USED.
  b->isgroup = isgroup;
  append_block(b);
  return this;
}

EMonomialOrder *EMonomialOrder::lex(int nvars, bool isgroup)
{
  if (nvars == 0) return this;
  mon_order_node *b = new mon_order_node;
  b->typ = MO_LEX;
  b->n = nvars;
  b->nslots = nvars;
  b->first_exp = this->nvars;
  b->first_slot = this->nslots;
  b->nweights = 0;
  b->weights = NULL;  // NOT USED.
  b->isgroup = isgroup;
  append_block(b);
  return this;
}

EMonomialOrder *EMonomialOrder::revlexWeights(int nvars, const int *wts, bool isgroup)
{
  int i;
  if (nvars == 0) return this;
  for (i=0; i<nvars; i++)
    if (wts[i] <= 0)
      {
	gError << "Weights in monomial order must be positive";
	return this;
      }
  mon_order_node *b = new mon_order_node;
  b->typ = MO_WTREVLEX;
  b->n = nvars;
  b->nslots = nvars;
  b->first_exp = this->nvars;
  b->first_slot = this->nslots;
  b->nweights = nvars;
  b->weights = new int[nvars];
  for (i=0; i<nvars; i++)
    b->weights[i] = wts[i];
  b->isgroup = isgroup;
  append_block(b);
  return this;
}

EMonomialOrder *EMonomialOrder::component()
{
  // Take the last component mentioned.
  componentloc = nslots;
  return this;
}

EMonomialOrder *EMonomialOrder::weightFunction(int nvars, const int *wts)
{
  mon_order_node *b = new mon_order_node;
  b->typ = MO_WTFCN;
  b->n = 0;
  b->nslots = 1;
  b->first_exp = -1;
  b->first_slot = this->nslots;
  b->nweights = nvars;
  b->weights = new int[nvars];
  for (int i=0; i<nvars; i++)
    b->weights[i] = wts[i];
  b->isgroup = false;  // Unused
  append_block(b);
  return this;
}

EMonomialOrder *EMonomialOrder::NClex(int nvars)
{
  if (nvars == 0) return this;
  mon_order_node *b = new mon_order_node;
  b->typ = MO_NC_LEX;
  b->n = nvars;
  b->nslots = 1;
  b->first_exp = this->nvars;
  b->first_slot = this->nslots;
  b->nweights = 0;
  b->weights = NULL;
  b->isgroup = false;  // Current unused.  Should we allow this?
  append_block(b);
  return this;
}

void EMonomialOrder::append_block(mon_order_node *b)
{
  mon_order_node **newblocks = new mon_order_node *[nblocks+1];
  for (int i=0; i<nblocks; i++)
    {
      newblocks[i] = order[i];
      order[i] = NULL;
    }
  newblocks[nblocks] = b;
  this->nblocks++;
  this->nvars += b->n;
  this->ncommvars += b->n;
  this->nslots += b->nslots;
  if (b->typ == MO_NC_LEX)
    {
      this->n_nc_blocks++;
      this->ncommvars -= b->n;
    }

  delete [] order;
  this->order = newblocks;
}

void EMonomialOrder::encode_commutative(const int *exp, int *result_psums) const
{
  int j, wtval;
  int *psum;
  const int *expit;
  for (int i=0; i<nblocks; i++)
    {
      mon_order_node *b = order[i];
      switch (b->typ)
	{
	case MO_LEX:
	  psum = result_psums + b->first_slot;
	  expit = exp + b->first_exp;
	  for (j=0; j<b->n; j++)
	    *psum++ = *expit++;
	  break;

	case MO_REVLEX:
	  psum = result_psums + b->first_slot + b->nslots;
	  expit = exp + b->first_exp;
	  *--psum = *expit++;
	  for (j=1; j<b->n; j++)
	    {
	      --psum;
	      *psum = *expit++ + psum[1];
	    }
	  break;

	case MO_WTREVLEX:
	  psum = result_psums + b->first_slot + b->nslots;
	  expit = exp + b->first_exp;
	  *--psum = b->weights[0] * (*expit++);
	  for (j=1; j<b->nweights; j++)
	    {
	      --psum;
	      *psum = b->weights[j] * (*expit++) + psum[1];
	    }
	  break;

	case MO_WTFCN:
	  wtval = 0;
	  for (j=0; j<b->nweights; j++)
	    wtval += exp[j] * b->weights[j];
	  result_psums[b->first_slot] = wtval;
	  break;

	case MO_NC_LEX:
	  assert(0);
	  break;

	default:
	  // MES
	  break;
	}
    }
}

void EMonomialOrder::encode_noncommutative(const intarray &varexp, int *result) const
{
  // This is a low-level routine: varexp is expected to be an array of length
  // 2*N, sone N.  Also, all commutative variable-exponent pairs go first, then
  // all var-exp pairs for first non-comm block, then second non-comm block, etc.
  //
  // result is expected to be a pointer obtained from MA, containing at least enough 
  // space for this monomial.
  int i, j, wtval, sum, ncvar;

  result++;  // We will put in the length field at the end
  int firstpast = nslots+ncommvars;

  int *commpart = result + nslots;
  for (i=0; i<ncommvars; i++)
    commpart[i] = 0;
  for (ncvar=0; ncvar < varexp.length(); ncvar += 2)
    {
      if (!is_comm_var[varexp[ncvar]]) break;
      commpart[varexp[ncvar]] += commpart[varexp[ncvar+1]];
    }
  // And now ncvar is set for use below.
  const int *expit;
  int *psum;

  for (i=0; i<nblocks; i++)
    {
      mon_order_node *b = order[i];
      switch (b->typ)
	{
	case MO_LEX:
	  psum = result + b->first_slot;
	  expit = commpart + b->first_exp;
	  for (j=0; j<b->n; j++)
	    *psum++ = *expit++;
	  break;

	case MO_REVLEX:
	  psum = result + b->first_slot + b->nslots;
	  expit = commpart + b->first_exp;
	  *--psum = *expit++;
	  for (j=1; j<b->n; j++)
	    {
	      --psum;
	      *psum = *expit++ + psum[1];
	    }
	  break;

	case MO_WTREVLEX:
	  psum = result + b->first_slot + b->nslots;
	  expit = commpart + b->first_exp;
	  *--psum = b->weights[0] * (*expit++);
	  for (j=1; j<b->nweights; j++)
	    {
	      --psum;
	      *psum = b->weights[j] * (*expit++) + psum[1];
	    }
	  break;

	case MO_WTFCN:
	  wtval = 0;
	  for (j=0; j<varexp.length(); j += 2)
	    if (varexp[j] < b->nweights)
	      wtval += varexp[j+1] * b->weights[varexp[j]];
	  result[b->first_slot] = wtval;
	  break;

	case MO_NC_LEX:
	  sum = 0;
	  for ( ; ncvar < varexp.length(); ncvar += 2)
	    if (varexp[ncvar] >= b->first_exp + b->n) break;
	    else {
	      sum += 2;
	      result[firstpast++] = varexp[ncvar];
	      result[firstpast++] = varexp[ncvar+1];
	    }
	  result[b->first_slot] = sum;
	  break;

	default:
	  // MES
	  break;
	}
    }
  // Now we put the length field back in:
  result[-1] = firstpast+1;
}

void EMonomialOrder::decode(const int *psums, int *result_exp) const
{
  int i, j, n;
  const int *psum;
  int firstpast = nslots;  // Only used for noncommutative monomials
  int *expit;
  for (i=0; i<nvars; i++)
    result_exp[i] = 0;
  for (i=0; i<nblocks; i++)
    {
      mon_order_node *b = order[i];
      switch (b->typ)
	{
	case MO_LEX:
	  psum = psums + b->first_slot;
	  expit = result_exp + b->first_exp;
	  for (j=0; j<b->n; j++)
	    *expit++ = *psum++;
	  break;

	case MO_REVLEX:
	  psum = psums + b->first_slot + b->nslots;
	  expit = result_exp + b->first_exp;
	  *expit++ = *--psum;
	  for (j=1; j<b->n; j++)
	    {
	      *expit++ = *psum - psum[1];
	      --psum;
	    }
	  break;

	case MO_WTREVLEX:
	  psum = psums + b->first_slot + b->nslots;
	  expit = result_exp + b->first_exp;
	  *expit++ = (*--psum) / (b->weights[0]);
	  for (j=1; j<b->n; j++)
	    {
	      *expit++ = (*psum - psum[1])/b->weights[j];
	      --psum;
	    }
	  break;

	case MO_WTFCN:
	  break;

	case MO_NC_LEX:
	  // This represents a loss of information...
	  n = psums[b->first_slot];
	  for (j=0; j<n; j++)
	    result_exp[psums[firstpast++]]++;
	  break;

	default:
	  // MES
	  break;
	}
    }
}

EMonomialOrder::mon_order_node* EMonomialOrder::find_block(int v) const
{
  int n_so_far = 0;
  if (v < 0 || v >= nvars) return 0;
  for (int i=0; i<nblocks; i++)
    {
      mon_order_node *b = order[i];
      if (b->n == 0) continue;
      if (n_so_far + b->n > v) return b;
      n_so_far += b->n;
    }
  return 0;  // Should not get here...
}

bool EMonomialOrder::isNonnegativeVariable(int v) const
{
  mon_order_node *b = find_block(v);
  if (b == 0 || !b->isgroup) return true;
  return false;
}

int EMonomialOrder::n_slots(int n) const
  // Number of slots used in the first n blocks in the order.
{
  if (n >= nblocks) return nslots;
  if (n <= 0) return 0;
  return order[nblocks-n-1]->first_slot;
}

void EMonomialOrder::set_noncommutative_parameters(
       int &nncblocks,
       int * &nclengths,
       bool * & isncslots,
       bool * & is_comm)
{
  int i;
  nncblocks = n_nc_blocks;
  nclengths = new int[nncblocks];
  isncslots = new bool[nslots];
  is_comm = new bool[nvars];

  this->is_comm_var = is_comm;  // This will remain valid as long as the monoid is around.

  for (i=0; i<nvars; i++)
    is_comm[i] = true;

  for (i=0; i<nslots; i++)
    isncslots[i] = false;

  int j = 0;
  for (i=0; i<nblocks; i++)
    if (order[i]->typ == MO_NC_LEX)
      {
	int whichslot = order[i]->first_slot;
	nclengths[j++] = whichslot;
	isncslots[whichslot] = true;
	for (int k=0; k<order[i]->n; k++)
	  is_comm[order[i]->first_exp + k] = false;
      }
}
