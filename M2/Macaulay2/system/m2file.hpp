
#ifndef _system_m2file_h_
#define _system_m2file_h_

#include "mutexclass.hpp"
#include "gc_std.hpp"

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
  gc_map(pthread_t,struct M2FileThreadState*) threadStates;
  //sync state for unsync or sync mode
  stdio0_fileOutputSyncState unsyncState;
  //Mutex for guarding map & internals of file
  pthreadMutex m_MapMutex;
  //For exclusive mode, the thread that currently owns io
  //For sync mode, the thread that currently owns the mutex (no, actually unsyncState)
  pthread_t syncOrExclOwner;
  //condition variable for waiting to acquire ownership
  pthread_cond_t ownerChangeCondition;
  //number of times syncOrExclOwner acquired
  size_t  recurseCount;
  //exclusive recurse count
  size_t exclusiveRecurseCount;
  //Function to wait for exclusive owner to be current thread.
  void waitExclusiveThread(size_t exclusiveRecurseDelta);
  //Function to wait for syncOrExclOwner to be released and then acquire it
  void waitThreadAcquire(size_t recurseDelta);
  //Function to release recurseCount and if == 0 set syncOrExclOwner to 0
  void releaseThreadCount(size_t recurseDelta);
  //Set new syncOrExclOwner
  void setOwnerThread(pthread_t newOwner, size_t recurseCounter=0);
};

#include "m2fileinterface.h"

#endif
