// Copyright 1997 Michael E. Stillman.

#include "newmonorder.hpp"

new_mon_order::new_mon_order(const int *moncodes)
{
  int i,j;
  nvars = *moncodes++;
  nblocks = *moncodes++;
  nwords = 0;
  order = new mon_order_list[nblocks];
  for (i = nblocks-1; i>= 0; i--)
    {
      order[i].typ = *moncodes++;
      order[i].weights = NULL;
      switch (order[i].typ)
	{
	case NEWMO_LEX8:
	case NEWMO_LEX16:
	case NEWMO_LEX32:
	case NEWMO_LEX64:
	case NEWMO_REVLEX8:
	case NEWMO_REVLEX16:
	case NEWMO_REVLEX32:
	case NEWMO_REVLEX64:
	  order[i].n = *moncodes++;
	  // Need to break these into sections, and set nbits, nwords
	  break;

	case NEWMO_WTFCN:
	  order[i].n = 0;
	  order[i].nslots = 1;
	  order[i].weights = new int[nvars];
	  for (j=0; j<nvars; j++) 
	    order[i].weights[j] = *moncodes++;
	  break;

	case NEWMO_COMPONENT:
	  order[i].n = 0;
	  order[i].nslots = 1;
	  break;

	case NEWMO_GROUP:
	  order[i].n = *moncodes++;
	  order[i].nslots = order[i].n;
	  order[i].ntorsion = *moncodes++;
	  order[i].weights = new int[order[i].ntorsion];
	  for (j=0; j<order[i].ntorsion; j++)
	    order[i].weights[j] = *moncodes++;
	  break;

	case NEWMO_SKEW:
	  order[i].n = *moncodes++;
	  order[i].nslots = order[i].n;
	  break;
	}
      nwords += order[i].nslots;
    }
}

new_mon_order::~new_mon_order()
{
  for (int i=nblocks-1; i>=0; i--)
    delete [] order[i].weights;
}

void new_mon_order::get_monomial_codes(intarray &result) const
{
  int i,j;
  result.shrink(0);
  result.append(nvars);
  result.append(nblocks);
  for (i=nblocks-1; i>=0; i--)
    {
      result.append(order[i].typ);
      switch (order[i].typ) 
	{
	case NEWMO_LEX8:
	case NEWMO_LEX16:
	case NEWMO_LEX32:
	case NEWMO_LEX64:
	  result.append(order[i].n);
	  break;

	case NEWMO_REVLEX8:
	case NEWMO_REVLEX16:
	case NEWMO_REVLEX32:
	case NEWMO_REVLEX64:
	  result.append(order[i].n);
	  break;
	  
	case NEWMO_COMPONENT:
	  break;

	case NEWMO_WTFCN:
	  for (j=0; j<nvars; j++)
	    result.append(order[i].weights[j]);
	  break;

	case NEWMO_GROUP:
	  result.append(order[i].n);
	  result.append(order[i].ntorsion);
	  for (j=0; j<order[i].ntorsion; j++)
	    result.append(order[i].weights[j]);
	  break;

	case NEWMO_SKEW:
	  result.append(order[i].n);
	  break;
	}
    }
}

void new_mon_order::text_out(buffer &o) const
{
  int j;
  o << "[";
  for (int i=nblocks-1; i>=0; i--)
    {
      switch (order[i].typ)
	{
	case NEWMO_LEX8:
	case NEWMO_LEX16:
	case NEWMO_LEX32:
	case NEWMO_LEX64:
	  o << "LEX(" << order[i].n << ")";
	  break;

	case NEWMO_REVLEX8:
	case NEWMO_REVLEX16:
	case NEWMO_REVLEX32:
	case NEWMO_REVLEX64:
	  o << "REVLEX(" << order[i].n << ")";
	  break;
	  
	case NEWMO_COMPONENT:
	  o << "C";
	  break;

	case NEWMO_WTFCN:
	  o << "WT(";
	  for (j=0; j<nvars; j++)
	    {
	      if (j > 0) o << " ";
	      o << order[i].weights[j];
	    }
	  o << ")";
	  break;

	case NEWMO_GROUP:
	  if (order[i].n > order[i].ntorsion)
	    o << "Group(ZZ^" << order[i].n - order[i].ntorsion;
	  if (order[i].ntorsion > 0)
	    for (j=0; j<order[i].ntorsion; j++)
	      {
		if (j > 0) o << " x ";
		o << "ZZ/" << order[i].weights[j] << "ZZ";
	      }
	  o << ")";
	  break;

	case NEWMO_SKEW:
	  o << "Skew(" << order[i].n << ")";
	  break;
	}
      if (i > 1) o << ",";
    }
  o << "]";
}
