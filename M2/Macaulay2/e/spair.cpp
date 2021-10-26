// Copyright 1996  Michael E. Stillman

#include "spair.hpp"
#include <iostream>

// The following is constant

static const int spair_heap_size[NHEAP] = {4,
                                           16,
                                           64,
                                           256,
                                           1024,
                                           4048,
                                           16384,
                                           65536,
                                           262144,
                                           1048576,
                                           16777216,
                                           268435456};

s_pair_heap::s_pair_heap(const Monoid *MM) : M(MM), top_of_heap(-1), nelems(0)
{
  int i;
  for (i = 0; i < NHEAP; i++)
    {
      heap[i] = NULL;
      n_in_heap[i] = 0;
    }
}

s_pair *s_pair_heap::grab_remaining_pairs()
{
  s_pair head;
  s_pair *inresult = &head;
  for (int i = 0; i < NHEAP; i++)
    if (heap[i])
      {
        inresult->next = heap[i];
        while (inresult->next != 0) inresult = inresult->next;
      }
  return head.next;
}

s_pair_heap::~s_pair_heap()
{
  // The user of this class must insure that all 's_pair's
  // have been removed first.  Thus, we don't need to
  // do anything here.
}
#if 0
// int s_pair_heap::compare(s_pair *f, s_pair *g) const
// {
//   int cmp = f->degree - g->degree;
//   if (cmp < 0) return -1;
//   if (cmp > 0) return 1;
//   cmp = M->compare(f->lcm, g->lcm);
//   if (cmp != 0) return cmp;
//   if (f->first == NULL || g->first == NULL)
//     return 0;
//   cmp = f->first->me - g->first->me;
//   if (cmp > 0) return 1;
//   if (cmp < 0) return -1;
//   return 0;
// }
#endif
int s_pair_heap::compare(s_pair *f, s_pair *g) const
{
  int cmp = f->degree - g->degree;
  if (cmp < 0) return -1;
  if (cmp > 0) return 1;
  int compare_type =
      0;  // MES: res-a2.cpp would change this globally, uugh.  Doesn't seem to
          // be used at all, so I am just commenting this out.
  switch (compare_type)
    {
      case 0:
        cmp = M->compare(f->lcm, g->lcm);
        if (cmp != 0)
          return cmp;  // MES: changed cmp to -cmp, to try out different order
                       // 2/21/00.
        if (f->first == NULL || g->first == NULL) return 0;
        cmp = f->first->me - g->first->me;
        if (cmp > 0) return 1;
        if (cmp < 0) return -1;
        break;
      case 1:
        cmp = M->compare(f->lcm, g->lcm);
        if (cmp != 0)
          return -cmp;  // MES: changed cmp to -cmp, to try out different order
                        // 2/21/00.
        if (f->first == NULL || g->first == NULL) return 0;
        cmp = f->first->me - g->first->me;
        if (cmp > 0) return 1;
        if (cmp < 0) return -1;
        break;
      case 2:
        if (f->first != NULL && g->first != NULL)
          {
            cmp = f->first->me - g->first->me;
            if (cmp < 0) return -1;
            if (cmp > 0) return 1;
          }
        if (f->second != NULL && g->second != NULL)
          {
            cmp = f->second->me - g->second->me;
            if (cmp < 0) return -1;
            if (cmp > 0) return 1;
          }
        cmp = M->compare(f->lcm, g->lcm);
        if (cmp != 0) return cmp;
        if (f->first == NULL || g->first == NULL) return 0;
        cmp = f->first->me - g->first->me;
        if (cmp > 0) return 1;
        if (cmp < 0) return -1;
        break;
      default:
        return -1;
        break;
    }
  return 0;
}

s_pair *s_pair_heap::merge(s_pair *f, s_pair *g) const
{
  // Sort in ascending degree order, then ascending monomial order
  if (g == NULL) return f;
  if (f == NULL) return g;
  s_pair head;
  s_pair *result = &head;
  while (1) switch (compare(f, g))
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

void s_pair_heap::sort_list(s_pair *&p) const
{
  if (p == NULL || p->next == NULL) return;
  s_pair *p1 = NULL;
  s_pair *p2 = NULL;
  while (p != NULL)
    {
      s_pair *tmp = p;
      p = p->next;
      tmp->next = p1;
      p1 = tmp;

      if (p == NULL) break;
      tmp = p;
      p = p->next;
      tmp->next = p2;
      p2 = tmp;
    }

  sort_list(p1);
  sort_list(p2);
  p = merge(p1, p2);
}

void s_pair_heap::insert(s_pair *&p)
{
  heap[0] = merge(p, heap[0]);
  n_in_heap[0]++;
  p = NULL;
  int i = 0;
  while (n_in_heap[i] >= spair_heap_size[i])
    {
      i++;
      if (i >= NHEAP)
        {
          std::cerr << "too many spairs: aborting" << std::endl;
          std::cerr << "n_in_heap[" << i << "]=" << n_in_heap[i - 1]
                    << std::endl;
          abort();
        }
      heap[i] = merge(heap[i - 1], heap[i]);
      n_in_heap[i] += n_in_heap[i - 1];
      heap[i - 1] = NULL;
      n_in_heap[i - 1] = 0;
    }
  if (i > top_of_heap) top_of_heap = i;
  nelems++;
}

void s_pair_heap::insert(s_pair *p, int len)
{
  int i = 0;
  while (len >= spair_heap_size[i]) i++;
  heap[i] = merge(p, heap[i]);
  n_in_heap[i] += len;
  //  std::cerr << "n_in_heap[" << i << "]=" << n_in_heap[i] << std::endl;
  p = NULL;
  while (n_in_heap[i] >= spair_heap_size[i])
    {
      i++;
      heap[i] = merge(heap[i - 1], heap[i]);
      n_in_heap[i] += n_in_heap[i - 1];
      heap[i - 1] = NULL;
      n_in_heap[i - 1] = 0;
    }
  if (i > top_of_heap) top_of_heap = i;
  nelems += len;
}

s_pair *s_pair_heap::remove()
{
  // Find a non-zero element
  if (nelems == 0) return NULL;
  int i, first;
  for (first = 0; first <= top_of_heap; first++)
    if (n_in_heap[first] > 0) break;

  s_pair *smallest = heap[first];
  // Now find the smallest one
  for (i = first + 1; i <= top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      int cmp = compare(smallest, heap[i]);
      if (cmp > 0)
        {
          first = i;
          smallest = heap[first];
        }
    }

  // Now remove this element and return it:
  heap[first] = smallest->next;
  smallest->next = NULL;
  nelems--;

  n_in_heap[first]--;
  if (n_in_heap[top_of_heap] == 0)
    for (i = top_of_heap - 1; i >= 0; i--)
      if (n_in_heap[i] > 0)
        {
          top_of_heap = i;
          break;
        }

  return smallest;
}

void s_pair_heap::put_back(s_pair *&p)
{
  insert(p);
  p = NULL;
}

void s_pair_heap::stats() const {}
void s_pair_heap::text_out(buffer &o) const
{
#ifdef DEVELOPMENT
#warning "should we display anything in spair text_out, stats?"
#endif
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
