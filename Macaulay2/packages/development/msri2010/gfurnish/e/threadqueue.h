#ifndef _threadqueue_h_
#define _threadqueue_h_

#include <queue>
#include "mutex.h"

template<class T>
class threadQueue
{
 public:
  void push(T t)
  {
    acquireSpinLock(&m_Spinlock);
    m_Queue.push(t);
    releaseSpinLock(&m_Spinlock);
  }
  T pop()
  { 
    acquireSpinLock(&m_Spinlock); 
    T t = m_Queue.front(); 
    m_Queue.pop(); 
    releaseSpinLock(&m_Spinlock); 
    return t; 
  } 
 protected:
  std::queue<T> m_Queue;
  spinlockStructure m_Spinlock;

};

#endif
