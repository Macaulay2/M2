// (c) 1994  Michael E. Stillman
#ifndef _queue_hh_
#define _queue_hh_

#include "style.hpp"

const int block_size = 128;

#define QUEUE(T) queue<T>

template <class T> class queue;

template<class T>
class queue_block
{
  friend class queue<T>;

  T entries[block_size];
  queue_block<T> *next;

  queue_block() : next(NULL) {}
};

template<class T>
class queue
{
  int             ephemeral;
  queue_block<T> *head;
  int             head_i;
  queue_block<T> *tail;
  int             tail_i;
  int             len;

  void copy(const queue<T> &q);

  void obtain(const queue<T> &qq)
    {
      queue<T> &q = (queue<T> &) qq;
      if (q.ephemeral == 0)
	copy(q);
      else
	{
	  ephemeral = 1;
	  swap(head, q.head);
	  swap(tail, q.tail);
	  swap(head_i, q.head_i);
	  swap(tail_i, q.tail_i);
	  swap(len, q.len);
	}
    }

  void del_list()
    {
      while(head != NULL)
	{
	  queue_block<T> *temp = head;
	  head = head->next;
	  delete temp;
	}
    }
 public:
  queue() : ephemeral(0), head(NULL), head_i(0),
            tail(NULL), tail_i(0), len(0) {}
  queue(queue<T> &q) { obtain(q); }

  ~queue() { del_list(); }

  void make_ephemeral() { ephemeral = 1; }
  
  queue<T> &operator=(const queue<T> &q)
    {
      if (&q != this) 
	obtain(q);
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

#endif


