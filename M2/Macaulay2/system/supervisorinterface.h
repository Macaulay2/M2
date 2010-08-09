#ifndef _system_supervisorinterface_h_
#define _system_supervisorinterface_h_

typedef void* (*ThreadTaskFunctionPtr)(void*);

#ifdef __cplusplus
extern "C" {
#endif

  struct ThreadSupervisor;
  struct ThreadTask;
  extern struct ThreadSupervisor threadSupervisor;
  extern void addThreadBody(pthread_t thread, parse_ThreadCellBody body);
  extern void addThread(pthread_t thread);
  extern void delThread(pthread_t thread);
  extern void* waitOnTask(struct ThreadTask* task);
  extern void addCancelTask(struct ThreadTask* task, struct ThreadTask* cancel);
  extern void addStartTask(struct ThreadTask* task, struct ThreadTask* start);
  extern void addDependency(struct ThreadTask* task, struct ThreadTask* start);
  extern void pushTask(struct ThreadTask* task);
  extern void initializeThreadSupervisor(int numThreads);
  struct ThreadTask* createThreadTask(const char* name, ThreadTaskFunctionPtr func, void* userData, int timeLimitExists, time_t timeLimitSeconds);
#ifdef __cplusplus
}
#endif



#endif
