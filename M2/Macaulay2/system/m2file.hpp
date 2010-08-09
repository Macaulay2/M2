
#ifndef _system_m2file_h_
#define _system_m2file_h_

    /* gc doc tells us to include pthread.h before gc.h */
    #ifdef GC_MALLOC
      #error "gc.h already included"
    #endif
    #ifndef _REENTRANT
      #define _REENTRANT
    #endif
    #include <pthread.h>
    #define GC_THREADS
    #include <gc/gc.h>

#include <gc/gc_cpp.h>

#include <map>




typedef struct stdio0_fileOutputSyncState_struct * stdio0_fileOutputSyncState;
struct M2FileThreadState
{
  //State of the file output for the given thread
  stdio0_fileOutputSyncState syncState;
};
struct M2File
{
public:
  M2File(stdio0_fileOutputSyncState fileUnsyncState);
  ~M2File();
  //current thread output mode.  0 is unsync, 1 is sync, 2 is thread exclusive
  int currentThreadMode; 
  //list of thread states for thread exclusive mode 
  std::map<pthread_t,struct M2FileThreadState*> threadStates;
  //sync state for unsync or sync mode
  stdio0_fileOutputSyncState unsyncState;
  //Mutex for guarding map & internals of file
  pthread_mutex_t mapMutex;
  //For exclusive mode, the thread that currently owns io
  //For sync mode, the thread that currently owns the mutex
  pthread_t exclusiveOwner;
  //condition variable for waiting to acquire exclusive ownership
  pthread_cond_t exclusiveChangeCondition;
  //number of times exclusiveOwner acquired
  size_t  recurseCount;
  //Function to wait for exclusiveOwner to be current thread.
  void waitExclusiveThread(size_t recurseCounter);
  //Function to wait for exclusiveOwner to be released and then acquire it
  void waitExclusiveThreadAcquire(size_t recurseCoutner); 
  //Function to release recurseCount and if == 0 set exclusiveOwner to -1
  void releaseExclusiveThreadCount(size_t recurseCounter);
  //Set new exclusiveOwner
  void setExclusiveOwner(pthread_t newExclusiveOwner, size_t recurseCounter=0);
};

#include "m2fileinterface.h"

#endif
