// (c) 1994  Michael E. Stillman

#include "array.hpp"

#define ARRAY(T) array< T >

#include "intarray.hpp"
template class ARRAY(int);
template class ARRAY(ARRAY(int));
template class ARRAY(intarray);
template class ARRAY(char *);

#include "object.hpp"
template class ARRAY(object);
template class ARRAY(object_element *);
template class ARRAY(const object_element *);

#include "queue.hpp"
#include "int_bag.hpp"
template class ARRAY(QUEUE(Bag *) *);
template class ARRAY(Bag *);

#include "respoly.hpp"
#include "res.hpp"
template class ARRAY(res_pair *);
template class ARRAY(res_level *);
template class ARRAY(res_degree *);
template class ARRAY(int *);

#include "ringelem.hpp"
template class ARRAY(ring_elem);
template class ARRAY(vec);

#include "freemod.hpp"
template class ARRAY(index_type *);

#include "monideal.hpp"
template class ARRAY(MonomialIdeal);

#include "hermite.hpp"
template class ARRAY(hm_elem *);

#include "gb.hpp"
template class ARRAY(gb_elem *);
template class ARRAY(monideal_pair *);

#include "gbZZ.hpp"
template class ARRAY(GB_elem *);
template class ARRAY(TermIdeal *);

#include "respoly2.hpp"
#include "res2.hpp"
template class ARRAY(res2_pair *);
template class ARRAY(res2_level *);
template class ARRAY(res2term *);

#include "termideal.hpp"
template class ARRAY(tagged_term *);

#include "gbbinom.hpp"
template class ARRAY(binomial_gb_elem *);
