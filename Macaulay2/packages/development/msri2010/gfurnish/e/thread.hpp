#ifndef _thread_hh_
#define _thread_hh_


class M2ThreadPool;

struct threadLocalInterp;

class M2Thread
{
public:
  M2Thread(M2ThreadPool* pool, int threadId); 
  const int ThreadId() { return m_ThreadId; }
  void start();
  static void* sThreadEntryPoint(void* ptr) { return ((M2Thread*)ptr)->threadEntryPoint(); }
  void* threadEntryPoint();
  void join();
  static M2Thread* getCurrentThread();
  void setThreadLocalInterp(struct threadLocalInterp* interp) { m_ThreadLocalInterp = interp; }
  struct threadLocalInterp* getThreadLocalInterp() { return m_ThreadLocalInterp; }
protected:
  void setCurrentThread();
  const int m_ThreadId;
  pthread_t m_Thread;
  M2ThreadPool* const m_ThreadPool;
  struct threadLocalInterp* m_ThreadLocalInterp;
};

#endif
