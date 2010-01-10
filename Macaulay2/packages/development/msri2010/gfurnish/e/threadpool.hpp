#ifndef _threadpool_hh_
#define _threadpool_hh_


class M2Thread;

class M2ThreadPool
{
public:
  M2ThreadPool(int numThreads);
  pthread_key_t ThreadKey() { return m_ThreadKey; } 
  void createThreads();
  void joinThreads();
  ~M2ThreadPool();
protected:
  void createThread(int threadId);
  pthread_key_t m_ThreadKey;
  const int m_NumThreads;
  M2Thread** m_Threads;
};

#endif
