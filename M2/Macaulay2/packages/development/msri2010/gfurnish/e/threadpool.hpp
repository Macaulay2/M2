#ifndef _threadpool_hh_
#define _threadpool_hh_

#include <vector>

class M2Thread;

class M2ThreadPool
{
public:
  M2ThreadPool(int numThreads);
  pthread_key_t ThreadKey() { return m_ThreadKey; } 
  void createThreads();
  void joinThreads();
  ~M2ThreadPool();
  M2Thread* startThread() { return m_StartThread; }
  static M2ThreadPool* m_Singleton;
protected:
  friend class M2Thread;
  void createThread(int threadId);
  pthread_key_t m_ThreadKey;
  const int m_NumPoolThreads;
  M2Thread* m_StartThread;
  std::vector<M2Thread*> m_PoolThreads;
};

#endif
