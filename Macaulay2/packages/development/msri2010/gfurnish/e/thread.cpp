#define GC_PTHREADS 1
#include <iostream>
#include "style.hpp"
#include "mem.hpp"
#include "engine.h"
#include "ring.hpp"
#include "threadpool.hpp"
#include "thread.hpp"


void* threadEntryPoint(void* userPtr);

M2Thread::M2Thread(M2ThreadPool* pool, int id):m_ThreadId(id),m_ThreadPool(pool)
{
  
}

void M2Thread::start()
{
  pthread_create(&m_Thread,0,sThreadEntryPoint,this);
}
void M2Thread::join()
{
  void* status;
  pthread_join(m_Thread, &status);
}
void* M2Thread::threadEntryPoint()
{
  setCurrentThread();
  std::cout << "Thread Test" << std::endl;
  std::cout << IM2_Ring_ZZ() << std::endl;
  const Ring* ZZ = IM2_Ring_ZZ();
  buffer buf;
  ZZ->text_out(buf);
  std::cout << buf.str() << std::endl;
  std::cout << this << " " << getCurrentThread() << std::endl;
  for(int i = 0; i < 20000000; ++i)
    {
      ring_elem rz = ZZ->random();
      buffer buf2;
      ZZ->elem_text_out(buf2,rz);
    }
  std::cout << "stress test done" << std::endl;
  return NULL;
}
void posixDestructor(void*) { }
void M2Thread::setCurrentThread()
{
  pthread_setspecific(m_ThreadPool->ThreadKey(),this);
}
M2Thread* M2Thread::getCurrentThread()
{
  return (M2Thread*) pthread_getspecific(m_ThreadPool->ThreadKey());
}
