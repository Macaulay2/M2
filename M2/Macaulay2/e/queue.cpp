// (c) 1994  Michael E. Stillman
#include "queue.hpp"

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

#include "object.hpp"
template class QUEUE(unsigned int);
template class QUEUE(int);
template class QUEUE(object);
#include "int_bag.hpp"
template class QUEUE(int_bag *);

#include "termideal.hpp"
template class QUEUE(mon_term *);
