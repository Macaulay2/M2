#define GC_PTHREADS 1
#include <iostream>
#include "style.hpp"
#include "mem.hpp"
#include "engine.h"
#include "ring.hpp"
#include "threadpool.hpp"
#include "thread.hpp"
#include "mutex.h"
#include "threadqueue.h"

extern "C" {
  extern void interp_process(), interp_process2(), interp_initializeLocalInterpState();
  extern void thread_rawRunSequence(struct threadLocalInterp*,void*);
}

void* outptr;
void rawTestThread(void* inptr)
{
  std::cout << "RAW" << std::endl;
  std::cout << "In " << inptr << std::endl;
  outptr = inptr;
  M2ThreadPool::m_Singleton->test();
  std::cout << "OUT OF RAW" << std::endl;
}

void setCurrentThreadLocalInterp(struct threadLocalInterp* tli)
{
  std::cout << "SET " << tli << std::endl;
  M2Thread::getCurrentThread()->setThreadLocalInterp(tli);
}
struct threadLocalInterp* getCurrentThreadLocalInterp()
{
  struct threadLocalInterp* tli = M2Thread::getCurrentThread()->getThreadLocalInterp();
  std::cout << "TLI " << tli << std::endl;
  return tli;
}
struct threadLocalInterp* getStartupThreadLocalInterp()
{
  struct threadLocalInterp* tli = M2ThreadPool::m_Singleton->startThread()->getThreadLocalInterp();
  return tli;
}

M2Thread::M2Thread(M2ThreadPool* pool, int id):m_ThreadId(id),m_ThreadPool(pool),m_ThreadLocalInterp(NULL)
{
  
}

void M2Thread::start()
{
  pthread_create(&m_Thread,0,sThreadEntryPoint,this);
  std::cout <<"Done starting" << std::endl;
}
void M2Thread::join()
{
  void* status;
  pthread_join(m_Thread, &status);
}
void* M2Thread::threadEntryPoint()
{
  setCurrentThread();
  if(this!=m_ThreadPool->startThread())
    {
      std::cout << getCurrentThreadLocalInterp() << std::endl;
      interp_initializeLocalInterpState();
      std::cout << "GO" << std::endl;
      std::cout << getCurrentThreadLocalInterp() << std::endl;
       std::cout << "out " << outptr << std::endl;
       thread_rawRunSequence(getCurrentThreadLocalInterp(),outptr);
      std::cout << "bar" << std::endl;
    }
  else
    {
      interp_process();
    }
  std::cout << "M" << std::endl;
  return NULL;
}
void posixDestructor(void*) { }
void M2Thread::setCurrentThread()
{
  pthread_setspecific(M2ThreadPool::m_Singleton->ThreadKey(),this);
}
M2Thread* M2Thread::getCurrentThread()
{
  return (M2Thread*) pthread_getspecific(M2ThreadPool::m_Singleton->ThreadKey());
}
