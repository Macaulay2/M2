// Copyright 1996  Michael E. Stillman

#include "newspair.hpp"
#include "text_io.hpp"


stash *S_pair::mystash;
stash *gen_pair::mystash;
stash *s_pair_bunch::mystash;
stash *s_pair_set::mystash;

extern int comp_printlevel;

gen_pair::gen_pair()
{
}

gen_pair::gen_pair(vec f, vec fsyz)
  : next(NULL),
    f(f),
    fsyz(fsyz)
{
}

S_pair::S_pair()
{
}

S_pair::S_pair(vec gsyz, vec rsyz)
: next(NULL), 
  gsyz(gsyz),
  rsyz(rsyz)
{
}

s_pair_set::s_pair_set(const FreeModule *FF, 
		       const FreeModule *FFsyz,
		       const FreeModule *GGsyz,
		       const FreeModule *RRsyz)
: F(FF), Fsyz(FFsyz), Gsyz(GGsyz), Rsyz(RRsyz), heap(NULL), this_deg(NULL),
  nelems(0), ngens(0), ncomputed(0)
{
}

//////////////////////////////////////////////
//// Destruction /////////////////////////////
//////////////////////////////////////////////

s_pair_set::~s_pair_set()
{
  // Loop through each bunch, and free every element on that list.
  while (heap != NULL)
    {
      s_pair_bunch *p = heap;
      heap = heap->next;
      flush_degree(p);
    }
}

void s_pair_set::remove_pair_list(S_pair *&p)
{
  while (p != NULL)
    {
      S_pair *s = p;
      p = p->next;
      remove_pair(s);
    }
}

void s_pair_set::remove_pair(S_pair *&s)
{
  s->next = NULL;
  Gsyz->remove(s->gsyz);
  Rsyz->remove(s->rsyz);
  delete s;
  s = NULL;
}

void s_pair_set::remove_gen_list(gen_pair *&p)
{
  while (p != NULL)
    {
      gen_pair *s = p;
      p = p->next;
      remove_gen(s);
    }
}

void s_pair_set::remove_gen(gen_pair *&s)
{
  s->next = NULL;
  F->remove(s->f);
  Fsyz->remove(s->fsyz);
  delete s;
  s = NULL;
}

void s_pair_set::flush_degree(s_pair_bunch *&p)
{
  remove_pair_list(p->pairs);
  remove_gen_list(p->gens);
  remove_pair_list(p->unsorted_pairs);
  remove_gen_list(p->unsorted_gens);
  p->next = NULL;
  delete p;
  p = NULL;
}


//////////////////////////////////////////////
//// Sorting /////////////////////////////////
//////////////////////////////////////////////

int s_pair_set::compare(S_pair *f, S_pair *g) const
{
  // Note: we are only comparing elements of the same degree
  if (f->rsyz == NULL)
    {
      if (g->rsyz == NULL)
	return Gsyz->compare(f->gsyz, g->gsyz);
      else 
	return LT;
    }
  else
    {
      if (g->rsyz == NULL)
	return GT;
      else 
	return Rsyz->compare(f->rsyz, g->rsyz);
    }
}

int s_pair_set::compare(gen_pair *f, gen_pair *g) const
{
  // Note: we are only comparing elements of the same degree
  return F->compare(f->f, g->f);
}

S_pair *s_pair_set::merge(S_pair *f, S_pair *g) const
{
  // Sort in ascending degree order, then ascending monomial order
  if (g == NULL) return f;
  if (f == NULL) return g;
  S_pair head;
  S_pair *result = &head;
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
void s_pair_set::sort(S_pair *& p) const
{
  if (p == NULL || p->next == NULL) return;
  S_pair *p1 = NULL;
  S_pair *p2 = NULL;
  while (p != NULL)
    {
      S_pair *tmp = p;
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

gen_pair *s_pair_set::merge(gen_pair *f, gen_pair *g) const
{
  // Sort in ascending degree order, then ascending monomial order
  if (g == NULL) return f;
  if (f == NULL) return g;
  gen_pair head;
  gen_pair *result = &head;
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
void s_pair_set::sort(gen_pair *& p) const
{
  if (p == NULL || p->next == NULL) return;
  gen_pair *p1 = NULL;
  gen_pair *p2 = NULL;
  while (p != NULL)
    {
      gen_pair *tmp = p;
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
//// Insertion ///////////////////////////////
//////////////////////////////////////////////

s_pair_bunch *s_pair_set::get_degree(int d)
{
  s_pair_bunch *p, *q;
  if (heap == NULL || heap->mydeg > d)
    {
      p = new s_pair_bunch(d);
      p->next = heap;
      heap = p;
      return heap;
    }
  if (heap->mydeg == d) return heap;
  for (p = heap; p->next != NULL; p = p->next)
    if (p->next->mydeg > d) break;
    else if (p->next->mydeg == d) return p->next;

  q = new s_pair_bunch(d);
  q->next = p->next;
  p->next = q;
  return q;
}

void s_pair_set::insert_s_pair(vec gsyz, vec rsyz)
{
  int deg;

  if (gsyz != NULL)
    deg = Gsyz->primary_degree(gsyz);
  else if (rsyz != NULL)
    deg = Rsyz->primary_degree(rsyz);
  else 
    return;

  S_pair *p = new S_pair(gsyz,rsyz);
  s_pair_bunch *q = get_degree(deg);
  p->next = q->unsorted_pairs;
  q->unsorted_pairs = p;
  q->nelems++;
  nelems++;
  p = NULL;
}

void s_pair_set::insert_generator(vec f, vec fsyz)
{
  if (f == NULL) return;
  gen_pair *p = new gen_pair(f, fsyz);
  int deg = F->primary_degree(p->f);
  s_pair_bunch *q = get_degree(deg);
  p->next = q->unsorted_gens;
  q->unsorted_gens = p;
  q->ngens++;
  ngens++;
  p = NULL;
}

//////////////////////////////////////////////
//// Removal /////////////////////////////////
//////////////////////////////////////////////

int s_pair_set::next_degree(int &nextdeg)
     // Returns number to be done in nextdeg.
{
  while (heap != NULL && heap->nelems == 0 && heap->ngens == 0)
    {
      // Remove this set
      s_pair_bunch *s = heap;
      heap = heap->next;
      delete s;
    }
  if (heap == NULL) return 0;
  this_deg = heap;
  nextdeg = this_deg->mydeg;
  // Now make sure that these elements are sorted:

  sort(this_deg->unsorted_pairs);
  this_deg->pairs = merge(this_deg->pairs, this_deg->unsorted_pairs);
  this_deg->unsorted_pairs = NULL;

  sort(this_deg->unsorted_gens);
  this_deg->gens = merge(this_deg->gens, this_deg->unsorted_gens);
  this_deg->unsorted_gens = NULL;

  pairs_done.append(nextdeg);	// Degree
  pairs_done.append(0);		// Number flushed
  pairs_done.append(0);		// Number computed
  return this_deg->nelems + this_deg->ngens;
}

bool s_pair_set::next_generator(vec &f, vec &fsyz)
{
  if (this_deg != heap) return false;

  gen_pair *result = this_deg->gens;
  if (result == NULL) return false;
  this_deg->gens = result->next;
  this_deg->ngens--;
  ngens--;

  result->next = NULL;
  pairs_done[pairs_done.length()-1]++;
  ncomputed++;

  f = result->f;
  fsyz = result->fsyz;
  delete result;
  return true;
}

bool s_pair_set::next_s_pair(vec &gsyz, vec &rsyz)
{
  if (this_deg != heap) return false;

  S_pair *result = this_deg->pairs;
  if (result == NULL) return false;
  this_deg->pairs = result->next;
  this_deg->nelems--;
  nelems--;

  result->next = NULL;
  pairs_done[pairs_done.length()-1]++;
  ncomputed++;

  gsyz = result->gsyz;
  rsyz = result->rsyz;
  delete result;
  return true;
}

void s_pair_set::flush_degree()
{
  if (heap != this_deg)
    {
      gError << "attempting to flush wrong pairs!";
      assert(0);
    }
  heap = heap->next;
  pairs_done[pairs_done.length()-2] = this_deg->nelems;
  flush_degree(this_deg);
}

//////////////////////////////////////////////
//// Stats ///////////////////////////////////
//////////////////////////////////////////////

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
