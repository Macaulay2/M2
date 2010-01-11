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

void startThreadPool(int numPoolThreads)
{
  M2ThreadPool* pool = new M2ThreadPool(numPoolThreads);
  pool->createThreads();
  pool->joinThreads(); 
  delete pool;
}

M2ThreadPool* M2ThreadPool::m_Singleton = NULL;

M2ThreadPool::M2ThreadPool(int numThreads):m_NumPoolThreads(numThreads),m_StartThread(NULL)
{
  m_Singleton = this;
  pthread_key_create(&m_ThreadKey,pthreadDestructor);
  m_StartThread = new M2Thread(this,-1);
}
void M2ThreadPool::createThreads()
{
  m_PoolThreads.resize(m_NumPoolThreads);
  for(int i = 0; i < m_NumPoolThreads; ++i)
    {
      createThread(i);
    }
  m_StartThread->threadEntryPoint();
}
void M2ThreadPool::joinThreads()
{
  for(int i = 0; i < m_NumPoolThreads; ++i)
    {
      m_PoolThreads[i]->join();
    }
}
void M2ThreadPool::createThread(int threadId)
{
  M2Thread* thread = new M2Thread(this, threadId);
  thread->start();
  m_PoolThreads[threadId] = thread;
}
M2ThreadPool::~M2ThreadPool()
{
  pthread_key_delete(m_ThreadKey);
  for(int i = 0; i < m_PoolThreads.size();++i)
    delete m_PoolThreads[i];
}
