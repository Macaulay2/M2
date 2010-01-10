#define GC_PTHREADS 1
#include <iostream>
#include "style.hpp"
#include "mem.hpp"
#include "engine.h"
#include "ring.hpp"
#include "threadpool.hpp"
#include "thread.hpp"

void pthreadDestructor(void*)
{
}

void rawLaunchThread()
{
  M2ThreadPool* pool = new M2ThreadPool(3);
  pool->createThreads();
  pool->joinThreads(); 
  delete pool;
}

M2ThreadPool::M2ThreadPool(int numThreads):m_NumThreads(numThreads),m_Threads(NULL)
{
  pthread_key_create(&m_ThreadKey,pthreadDestructor);
}
void M2ThreadPool::createThreads()
{
  std::cout << "Creating " << m_NumThreads << " Threads" << std::endl;
  m_Threads = new M2Thread*[m_NumThreads];
  for(int i = 0; i < m_NumThreads; ++i)
    {
      createThread(i);
    }
}
void M2ThreadPool::joinThreads()
{
  for(int i = 0; i < m_NumThreads; ++i)
    {
      m_Threads[i]->join();
    }
}
void M2ThreadPool::createThread(int threadId)
{
  M2Thread* thread = new M2Thread(this, threadId);
  thread->start();
  m_Threads[threadId] = thread;
}
M2ThreadPool::~M2ThreadPool()
{
  pthread_key_delete(m_ThreadKey);
  if(m_Threads)
    delete[] m_Threads;
}
