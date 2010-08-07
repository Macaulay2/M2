#ifndef _system_m2file_h_
#define _system_m2file_h_

#include <pthread.h>
#include <map>

#include <gc/gc_cpp.h>


typedef struct stdio0_fileOutputSyncState_struct * stdio0_fileOutputSyncState;
struct M2FileThreadState
{
  //State of the file output for the given thread
  stdio0_fileOutputSyncState syncState;
};
struct M2File : public gc_cleanup
{
public:
  M2File(stdio0_fileOutputSyncState fileUnsyncState);
  //current thread output mode.  0 is unsync, 1 is sync, 2 is thread exclusive
  int currentThreadMode; 
  //list of thread states for thread exclusive mode 
  std::map<pthread_t,struct M2FileThreadState> threadStates;
  //sync state for unsync or sync mode
  stdio0_fileOutputSyncState unsyncState;
  //Mutex for guarding Map 
  pthread_mutex_t mapMutex;
  //Mutex for synced output -- only used in mode 1
  pthread_mutex_t outputMutex;
};

#include "m2fileinterface.h"

#endif
