#ifndef _system_supervisorinterface_h_
#define _system_supervisorinterface_h_

    #ifndef _REENTRANT
      #define _REENTRANT
    #endif
    #include <pthread.h>
    #define GC_THREADS
    #include <gc/gc.h>

typedef void* (*ThreadTaskFunctionPtr)(void*);

#ifdef __cplusplus
extern "C" {
#endif

  struct ThreadSupervisor;
  struct ThreadTask;
  extern struct ThreadSupervisor threadSupervisor;
  struct parse_ThreadCellBody_struct;
  extern void addThreadBody(pthread_t thread, struct parse_ThreadCellBody_struct* body);
  extern void addThread(pthread_t thread);
  extern void delThread(pthread_t thread);
  extern void* waitOnTask(struct ThreadTask* task);
  extern void addCancelTask(struct ThreadTask* task, struct ThreadTask* cancel);
  extern void addStartTask(struct ThreadTask* task, struct ThreadTask* start);
  extern void addDependency(struct ThreadTask* task, struct ThreadTask* start);
  extern void pushTask(struct ThreadTask* task);
  extern void initializeThreadSupervisor(int numThreads);
  extern struct ThreadTask* createThreadTask(const char* name, ThreadTaskFunctionPtr func, void* userData, int timeLimitExists, time_t timeLimitSeconds);
  extern void TS_Add_ThreadLocal(void* refno, const char* name) { }
     
     
#ifdef __cplusplus
}
#endif



#endif
