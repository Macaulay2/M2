// Copyright 1996  Michael E. Stillman

#include "Espairs.hpp"
#include "text_io.hpp"

stash *es_pair::mystash;
extern int comp_printlevel;

ESPairSet::ESPairSet()
  : heap(0), nelems(0)
{
}

void ESPairSet::insert(es_pair *p)
  // Insert a list of pairs.
{
  while (p != 0)
    {
      es_pair *tmp = p;
      p = p->next;
      nelems++;
      tmp->next = heap;
      heap = tmp;
    }
}

int ESPairSet::next_degree(int &nextdeg) const
  // Returns number to be done in nextdeg.
{
  if (heap == 0) return 0;
  int len = 1;
  nextdeg = heap->degree;
  for (es_pair *p = heap->next; p!=0; p=p->next)
    if (p->degree > nextdeg) 
      continue;
    else if (p->degree < nextdeg)
      {
	len = 1;
	nextdeg = p->degree;
      }
  else
    len++;
  return len;
}

int ESPairSet::get_next_degree(int &deg, es_pair *&elems)
// Return in 'elems' an unsorted list of elements in the lowest degree.
// This lowest degree is set in 'lodeg', and the number of pairs in the
// list is returned.
{
  elems = 0;
  int len = next_degree(deg);
  if (len == 0) return 0;
  es_pair head;
  head.next = heap;
  es_pair *p = &head;
  while (p->next != 0)
    if (p->next->degree != deg)
      p = p->next;
    else
      {
	es_pair *tmp = p->next;
	p->next = tmp->next;
	tmp->next = elems;
	elems = tmp;
      }
  heap = head.next;
  nelems -= len;
  return len;
}

void ESPairSet::stats()
  // Displays some statistics about this set.
{
}



es_pair *ESPairOperations::merge(es_pair *f, es_pair *g) const
{
  if (g == NULL) return f;
  if (f == NULL) return g;
  es_pair head;
  es_pair *result = &head;
  while (1)
    switch (compare(f, g))
      {
      case 1:
	result->next = g;
	result = result->next;
	g = g->next;
	if (g == NULL) 
	  {
	    result->next = f;
	    return head.next;
	  }
	break;
      case -1:
      case 0:
	result->next = f;
	result = result->next;
	f = f->next;
	if (f == NULL) 
	  {
	    result->next = g; 
	    return head.next;
	  }
	break;
      }
}
void ESPairOperations::sort(es_pair *& p) const
{
  if (p == NULL || p->next == NULL) return;
  es_pair *p1 = NULL;
  es_pair *p2 = NULL;
  while (p != NULL)
    {
      es_pair *tmp = p;
      p = p->next;
      tmp->next = p1;
      p1 = tmp;

      if (p == NULL) break;
      tmp = p;
      p = p->next;
      tmp->next = p2;
      p2 = tmp;
    }

  sort(p1);
  sort(p2);
  p = merge(p1, p2);
}


//////////////////////////////////////////////
//// Stats ///////////////////////////////////
//////////////////////////////////////////////
#if 0
void s_pair_set::debug_out(S_pair *q) const
{
  if (q == NULL) return;
  buffer o;
  debug_out(o,q);
  emit(o.str());
}


void s_pair_set::debug_out(buffer &o, S_pair *q) const
{
  if (q->gsyz != NULL) 
    {
      o << "spair ";
      Gsyz->elem_text_out(o, q->gsyz);
    }
  if (q->rsyz != NULL)
    {
      o << " rsyz ";
      Rsyz->elem_text_out(o, q->rsyz);
    }
  o << newline;
}

void s_pair_set::debug_out(buffer &o, gen_pair *s) const
{
  o<< "  gen  ";
  F->elem_text_out(o, s->f);
  o << newline;
}

void s_pair_set::stats()
{
  buffer o;
  s_pair_bunch *p;
  int totalpairs = 0;
  int totalflushed = 0;

  o << "degree  #computed  #flushed" << newline;
  o << "------  ---------  --------" << newline;
  for (int i=0; i<pairs_done.length(); i += 3)
    {
      o << pairs_done[i] << "  " << pairs_done[i+2] << "  " << pairs_done[i+1] << newline;
      totalpairs += pairs_done[i+2];
      totalflushed += pairs_done[i+1];
    }
  o << "------  ---------  --------" << newline;
  o << "total   " << totalpairs << " " << totalflushed << newline << newline;

  o << "pairs/gens remaining" << newline;
  o << "degree  #remaining  #gens" << newline;
  o << "------  ----------  -----" << newline;
  int pairsleft = 0;
  int gensleft = 0;
  for (p = heap; p != NULL; p = p->next)
    {
      o << p->mydeg << "  " << p->nelems << "  " << p->ngens << newline;
      pairsleft += p->nelems;
      gensleft += p->ngens;
    }
  o << "------  ----------  -----" << newline;
  o << "total   " << pairsleft << "  " << gensleft << newline;

  if (comp_printlevel >= 5)
    {
      o << newline;
      for (p = heap; p != NULL; p = p->next)
	{
	  S_pair *s;
	  gen_pair *t;
	  o << "-- degree " << p->mydeg << " ---" << newline;
	  for (s = heap->pairs; s != NULL; s = s->next)
	    debug_out(o, s);
	  o << "-- unsorted -------" << newline;
	  for (s = heap->unsorted_pairs; s != NULL; s = s->next)
	    debug_out(o, s);
	  o << "-- gens -------" << newline;
	  for (t = heap->gens; t != NULL; t = t->next)
	    debug_out(o, t);
	  o << "-- unsorted -------" << newline;
	  for (t = heap->unsorted_gens; t != NULL; t = t->next)
	    debug_out(o, t);
	}
    }
  emit(o.str());
}
#endif
