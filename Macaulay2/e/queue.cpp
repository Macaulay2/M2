// (c) 1994  Michael E. Stillman
#include "queue.hpp"

#include "object.hpp"
template class QUEUE(unsigned int);
template class QUEUE(int);
template class QUEUE(object);
#include "int_bag.hpp"
template class QUEUE(int_bag *);

#include "termideal.hpp"
template class QUEUE(tagged_term *);
