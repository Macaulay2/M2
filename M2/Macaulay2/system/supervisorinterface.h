#ifndef _system_supervisorinterface_h_
#define _system_supervisorinterface_h_

    #ifndef _REENTRANT
      #define _REENTRANT
    #endif
    #include <pthread.h>
    #define GC_THREADS
    #include <gc/gc.h>

typedef void* (*ThreadTaskFunctionPtr)(void*);

#define GETSPECIFICTHREADLOCAL
#ifdef GETSPECIFICTHREADLOCAL
#define THREADLOCAL(x,typ) (*((typ*)TS_Get_Local(x##_id)))
#define THREADLOCALDECL(typ,x) int x##_id
#define THREADLOCALINIT(x) TS_Add_ThreadLocal(&x##_id,#x)
#else
#define THREADLOCAL(x,typ) x
#define THREADLOCALDECL(typ,x) typ x
#define THREADLOCALINIT(x) x
#endif


#ifdef __cplusplus
extern "C" {
#endif

  struct ThreadSupervisor;
  struct ThreadTask;
  extern struct ThreadSupervisor* threadSupervisor;
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
#ifdef GETSPECIFICTHREADLOCAL
  extern void TS_Add_ThreadLocal(int* refno, const char* name);
  static void** TS_Get_LocalArray() {  return (void**)pthread_getspecific(*(pthread_key_t*)threadSupervisor); }
  static void** TS_Get_Local(int refno) { return &TS_Get_LocalArray()[refno]; }
#endif
  
     
#ifdef __cplusplus
}
#endif



#endif
