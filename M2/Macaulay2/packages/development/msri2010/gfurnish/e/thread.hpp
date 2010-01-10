#ifndef _thread_hh_
#define _thread_hh_


class M2ThreadPool;

class M2Thread
{
public:
  M2Thread(M2ThreadPool* pool, int threadId); 
  const int ThreadId() { return m_ThreadId; }
  void start();
  static void* sThreadEntryPoint(void* ptr) { return ((M2Thread*)ptr)->threadEntryPoint(); }
  void join();
  M2Thread* getCurrentThread();

protected:
  void* threadEntryPoint();
  void setCurrentThread();
  const int m_ThreadId;
  pthread_t m_Thread;
  M2ThreadPool* const m_ThreadPool;
};

#endif
