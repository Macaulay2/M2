#ifndef _system_mutex_hpp_
#define _system_mutex_hpp_

#include <pthread.h>
#include <stdlib.h>

class pthreadMutex
{
public:
  pthreadMutex()
  {
    if(pthread_mutex_init(&m_Mutex,NULL))
      abort();
  }
  void lock()
  {
    while(pthread_mutex_lock(&m_Mutex));
  }
  void unlock()
  {
    while(pthread_mutex_unlock(&m_Mutex));
  }
  ~pthreadMutex()
  {
    if(pthread_mutex_destroy(&m_Mutex))
      abort();
  }
  pthread_mutex_t m_Mutex;
};


#endif
