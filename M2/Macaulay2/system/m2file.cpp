#include "m2file.hpp"
#include "pthread-methods.hpp"
#include <iostream>
#include <mutex>
using std::lock_guard;

extern "C"
{
extern stdio0_fileOutputSyncState stdio0_newDefaultFileOutputSyncState();
};

M2File::M2File(stdio0_fileOutputSyncState fileUnsyncState):
    currentThreadMode(0),
    unsyncState(fileUnsyncState),
    ownerChangeCondition(PTHREAD_COND_INITIALIZER),
    recurseCount(0),
    exclusiveRecurseCount(0)
{
  clearThread(syncOrExclOwner);
}
M2File::~M2File()
{
  //for good practice, explicitly delete the filethreadstate if the file is deleted
  //this should be taken care of by GC though.
  for(gc_map(pthread_t,struct M2FileThreadState*)::iterator it = threadStates.begin(); it!=threadStates.end(); ++it)
    {
      delete it->second;
    }
}


void M2File::waitExclusiveThread(size_t exclusiveRecurseDelta)
{
  //acquire a lock on the thread map
  lock_guard<pthreadMutex> lock(m_MapMutex);
  while(1)
    {
      //recursive mutex part -- if the current owner is self, return. 
      //Note that there is always a current owner in exclusive mode so this works
      if(syncOrExclOwner==pthread_self())
	{
	  exclusiveRecurseCount+=exclusiveRecurseDelta;
	  if(exclusiveRecurseCount==0)
	    {
	    pthread_cond_broadcast(&ownerChangeCondition);
	    //this is to cover the case where we switch from 2 to 1 and need to set no exclusive owner upon exclusive finish
	    if(currentThreadMode!=2)
		clearThread(syncOrExclOwner);
	    }
	  return;
	}
      //this handles the case where the user shifts from exclusive to synchronized mode
      if(currentThreadMode!=2)
	  return;	// TODO: used to not unlock m_MapMutex if currentThreadMode==0 (buggy?)
      //otherwise wait for the exclusive thread to be changed
      pthread_cond_wait(&ownerChangeCondition,&m_MapMutex.m_Mutex);
    }
}

void M2File::waitThreadAcquire(size_t recurseDelta)
{
  lock_guard<pthreadMutex> lock(m_MapMutex);
  while(1)
    {
      //if we have recently switched from exclusive to synchronized mode and the exclusive mode isn't done, wait.
      if(exclusiveRecurseCount && syncOrExclOwner!=pthread_self())
	{
	}
      else if(syncOrExclOwner==0)//if the exclusive/synchronized owner isn't in use, take ownership of it.
	{
	  syncOrExclOwner=pthread_self();
	  recurseCount=recurseDelta;
	  pthread_cond_broadcast(&ownerChangeCondition);
	  return;
	}
      else if(syncOrExclOwner==pthread_self())//if it is in use, check to make sure it is this thread, otherwise wait
	{
	  recurseCount+=recurseDelta;
	  return;
	}
      pthread_cond_wait(&ownerChangeCondition,&m_MapMutex.m_Mutex);
    }
}

void M2File::setOwnerThread(pthread_t newOwner, size_t recurseCounter)
{
  //change the current owner of the thread.
  //note this does no sanity checking.
  //should it?
  lock_guard<pthreadMutex> lock(m_MapMutex);
  recurseCount=recurseCounter;
  syncOrExclOwner = newOwner;
  pthread_cond_broadcast(&ownerChangeCondition);
}

void M2File::releaseThreadCount(size_t recurseDelta)
{
  lock_guard<pthreadMutex> lock(m_MapMutex);
  recurseCount-=recurseDelta;
  if(!recurseCount)
    {
      clearThread(syncOrExclOwner);
      pthread_cond_broadcast(&ownerChangeCondition);
    }
}

extern "C"
{

  int M2File_GetThreadMode(M2File* file) { return file->currentThreadMode; }

  void M2File_SetThreadMode(M2File* file, int threadMode)
  {
    if(file->currentThreadMode==2)
      {
	file->currentThreadMode = threadMode; 
	//if changing out of thread 2 mode, zero the exclusive owner if the current exclusive thread is done
	if(threadMode!=2)
	  {
	    if(file->exclusiveRecurseCount==0)
	      clearThread(file->syncOrExclOwner);	// TODO: needs to lock m_MapMutex !?
	    pthread_cond_broadcast(&file->ownerChangeCondition);
	  }
      }
    else
      {
	//if switching into exclusive mode, the current owner becomes the thread that did the switching by default
	if(threadMode==2)
	  file->setOwnerThread(pthread_self(),0);
	file->currentThreadMode = threadMode; 
      }
  }

  struct M2File* M2File_New(stdio0_fileOutputSyncState fileUnsyncState) { return new (GC) M2File(fileUnsyncState); } 
  stdio0_fileOutputSyncState M2File_UnsyncState(M2File* file) { return file->unsyncState; } 
  stdio0_fileOutputSyncState M2File_GetState(struct M2File* file)
  {
    //if unsync mode just return unsync state
    if(file->currentThreadMode==0)
      { 
	return file->unsyncState;
      }
    //if sync mode, acquire lock
    else if(file->currentThreadMode==1)
      {
	file->waitThreadAcquire(1);
	return file->unsyncState;
      }
    else // if(file->currentThreadMode==2)
      {
	//get thread id
	pthread_t localId = pthread_self();
	lock_guard<pthreadMutex> lock(file->m_MapMutex);
	//try to find thread id in thread states map
	gc_map(pthread_t, struct M2FileThreadState*)::iterator it = file->threadStates.find(localId);
	stdio0_fileOutputSyncState foss = NULL;
	if(it!=file->threadStates.end())
	  {
	    //case: thread id found in map, so just return associated thread state
	    foss = it->second->syncState;
	  }
	else
	  {
	    //thread has not used this file yet.
	    //create a new state.
	    foss = stdio0_newDefaultFileOutputSyncState();
	    struct M2FileThreadState* state = new (GC) M2FileThreadState();
	    state->syncState = foss;
	    file->threadStates[localId]=state;
	  }
	return foss;
      }
  }
  void M2File_ReleaseState(struct M2File* file)
  {
    //in unsync mode there is nothing to release
    if(file->currentThreadMode==0)
      {
      }
    //in sync mode, release the recursive mutex
    else if(file->currentThreadMode==1)
      {
	file->releaseThreadCount(1);
      }
    else if(file->currentThreadMode==2)
      {
	//get thread id
	pthread_t localId = pthread_self();
	lock_guard<pthreadMutex> lock(file->m_MapMutex);
	//try to find thread id in thread states map
	gc_map(pthread_t, struct M2FileThreadState*)::iterator it = file->threadStates.find(localId);
	if(it!=file->threadStates.end())
	  {
	    //not necessary to do anything here
	  }
	else
	  {
	    //this shouldn't happen, and if it does it is *clearly* an error and will lead to deadlock or memory corruption at some point
	    abort();
	  }
	file->recurseCount-=1;
      }
  }
  void M2File_StartInput(struct M2File* file)
  {
    //for any mode besides thread exclusive we may ignore this function call
    if(file->currentThreadMode!=2 && !file->exclusiveRecurseCount)
      return;
    file->waitExclusiveThread(1);
  }
  void M2File_EndInput(struct M2File* file)
  {
    //for any mode besides thread exclusive we may ignore this function call
    if(file->currentThreadMode!=2 && !file->exclusiveRecurseCount)
      return;
    file->waitExclusiveThread(-1);
  }
  void M2File_StartOutput(struct M2File* file)
  {
    //for any mode besides thread exclusive we may ignore this function call
    if(file->currentThreadMode!=2 && !file->exclusiveRecurseCount)
      return;
    file->waitExclusiveThread(1);
  }
  void M2File_EndOutput(struct M2File* file)
  {
    //for any mode besides thread exclusive we may ignore this function call  
    if(file->currentThreadMode!=2 && !file->exclusiveRecurseCount)
      return;
    file->waitExclusiveThread(-1);
  }

};
