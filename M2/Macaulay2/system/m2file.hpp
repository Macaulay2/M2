
#ifndef _system_m2file_h_
#define _system_m2file_h_

#include "mutexclass.hpp"

#include <M2/gc-include.h>
#include <gc/gc_cpp.h>

#include <map>




typedef struct stdio0_fileOutputSyncState_struct * stdio0_fileOutputSyncState;
/*
This is a structure that holds the thread specific state for a given file in toplevel M2.
The basic idea is to encapsulate all state for things like Nets and 2D stdio in this thread state.
Then threads can use stdio without running all over each other.  This is especially critical for 2D IO
*/
struct M2FileThreadState
{
  //State of the file output for the given thread
  stdio0_fileOutputSyncState syncState;
};
struct M2File
{
public:
  /**
     Constructor for a M2file for a given unsynchronized state.
  **/
  M2File(stdio0_fileOutputSyncState fileUnsyncState);
  ~M2File();
  //current thread output mode.  0 is unsync, 1 is sync, 2 is thread exclusive
  int currentThreadMode; 
  //list of thread states for thread exclusive mode 
  std::map<pthread_t,struct M2FileThreadState*> threadStates;
  //sync state for unsync or sync mode
  stdio0_fileOutputSyncState unsyncState;
  //Mutex for guarding map & internals of file
  pthreadMutex m_MapMutex;
  //For exclusive mode, the thread that currently owns io
  //For sync mode, the thread that currently owns the mutex
  pthread_t exclusiveOwner;
  //condition variable for waiting to acquire exclusive ownership
  pthread_cond_t exclusiveChangeCondition;
  //number of times exclusiveOwner acquired
  size_t  recurseCount;
  //exclusive recurse count
  size_t exclusiveRecurseCount;
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
