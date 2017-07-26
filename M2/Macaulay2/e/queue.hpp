// (c) 1994  Michael E. Stillman
#ifndef _queue_hh_
#define _queue_hh_

#include "newdelete.hpp"
#include <utility>
#include <cassert>
const int block_size = 128;

#define QUEUE(T) queue<T>

template <class T>
class queue;

template <class T>
class queue_block : public our_new_delete
{
  friend class queue<T>;

  T entries[block_size];
  queue_block<T> *next;

  queue_block() : next(NULL) {}
};

template <class T>
class queue : public our_new_delete
{
  int ephemeral;
  queue_block<T> *head;
  int head_i;
  queue_block<T> *tail;
  int tail_i;
  int len;

  void copy(const queue<T> &q);

  void obtain(const queue<T> &qq)
  {
    queue<T> &q = const_cast<queue<T> &>(qq);
    if (q.ephemeral == 0)
      copy(q);
    else
      {
        ephemeral = 1;
        std::swap(head, q.head);
        std::swap(tail, q.tail);
        std::swap(head_i, q.head_i);
        std::swap(tail_i, q.tail_i);
        std::swap(len, q.len);
      }
  }

  void del_list()
  {
    while (head != NULL)
      {
        queue_block<T> *temp = head;
        head = head->next;
        delete temp;
      }
  }

 public:
  queue() : ephemeral(0), head(NULL), head_i(0), tail(NULL), tail_i(0), len(0)
  {
  }
  queue(queue<T> &q) { obtain(q); }
  ~queue() { del_list(); }
  void make_ephemeral() { ephemeral = 1; }
  queue<T> &operator=(const queue<T> &q)
  {
    if (&q != this) obtain(q);
    return *this;
  }

  void insert(const T &elem);
  bool remove(T &elem);
  void flush()
  {
    del_list();
    tail = NULL;
    head_i = tail_i = len = 0;
  }

  int length() const { return len; }
  bool operator==(const queue<T> &) const { return false; }
  bool operator!=(const queue<T> &) const { return true; }
};

template <class T>
void queue<T>::copy(const queue<T> &q)
{
  assert(0);

  if (q.len == 0)
    {
      while (head != NULL)
        {
          queue_block<T> *tmp = head;
          head = head->next;
          delete tmp;
        }
      head_i = tail_i = len = 0;
      tail = NULL;
      return;
    }
}

template <class T>
void queue<T>::insert(const T &elem)
{
  if (len == 0)
    {
      tail = head = new queue_block<T>;
      tail_i = head_i = 0;
    }

  if (tail_i >= block_size)
    {
      tail = tail->next = new queue_block<T>;
      tail_i = 0;
    }

  tail->entries[tail_i] = elem;
  tail_i++;
  len++;
}

template <class T>
bool queue<T>::remove(T &elem)
{
  if (len == 0) return false;

  elem = head->entries[head_i];

  head_i++;
  len--;

  if (head == tail)
    {
      if (head_i == tail_i)
        {
          delete head;
          head = NULL;
          len = head_i = tail_i = 0;
        }
    }
  else if (head_i >= block_size)
    {
      queue_block<T> *temp = head;
      head = head->next;
      delete temp;
      head_i = 0;
    }

  return true;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
