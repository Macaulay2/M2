#include "m2file.hpp"
#include "pthread-methods.hpp"
#include <iostream>

extern "C"
{
extern stdio0_fileOutputSyncState stdio0_newDefaultFileOutputSyncState();
};

M2File::M2File(stdio0_fileOutputSyncState fileUnsyncState):
    currentThreadMode(0),
    unsyncState(fileUnsyncState),
    exclusiveChangeCondition(PTHREAD_COND_INITIALIZER),
    recurseCount(0),
    exclusiveRecurseCount(0)
{
  clearThread(exclusiveOwner);
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


void M2File::waitExclusiveThread(size_t recurseCounter)
{
  //acquire a lock on the thread map
  m_MapMutex.lock();
  while(1)
    {
      //recursive mutex part -- if the current owner is self, return. 
      //Note that there is always a current owner in exclusive mode so this works
      if(exclusiveOwner==pthread_self())
	{
	  exclusiveRecurseCount+=recurseCounter;
	  if(exclusiveRecurseCount==0)
	    {
	    pthread_cond_broadcast(&exclusiveChangeCondition);	
	    //this is to cover the case where we switch from 2 to 1 and need to set no exclusive owner upon exclusive finish
	    if(currentThreadMode!=2)
	      {
		clearThread(exclusiveOwner);
	      }
	    }
	  m_MapMutex.unlock();
	  return;
	}
      //this handles the case where the user shifts from exclusive to synchronized mode
      if(currentThreadMode!=2)
	{
	  if(currentThreadMode==1)
	    {
	      m_MapMutex.unlock();
	      return;
	    }
	  return;
	}
      //otherwise wait for the exclusive thread to be changed
      pthread_cond_wait(&exclusiveChangeCondition,&m_MapMutex.m_Mutex);  
    }
  m_MapMutex.unlock();
}

void M2File::waitExclusiveThreadAcquire(size_t recurseCounter)
{
  m_MapMutex.lock();
  while(1)
    {
      //if we have recently switched from exclusive to synchronized mode and the exclusive mode isn't done, wait.
      if(exclusiveRecurseCount && exclusiveOwner!=pthread_self())
	{
	}
      else if(exclusiveOwner==0)//if the exclusive/synchronized owner isn't in use, take ownership of it.
	{
	  exclusiveOwner=pthread_self();
	  recurseCount=recurseCounter;
	  m_MapMutex.unlock();
	  pthread_cond_broadcast(&exclusiveChangeCondition);
	  return;
	}
      else if(exclusiveOwner==pthread_self())//if it is in use, check to make sure it is this thread, otherwise wait
	{
	  recurseCount+=recurseCounter;
	  m_MapMutex.unlock();
	  return;
	}
      pthread_cond_wait(&exclusiveChangeCondition,&m_MapMutex.m_Mutex);
      
    }
  m_MapMutex.unlock();

}

void M2File::setExclusiveOwner(pthread_t newExclusiveOwner, size_t recurseCounter)
{
  //change the current owner of the thread.
  //note this does no sanity checking.
  //should it?
  m_MapMutex.lock();
  recurseCount=recurseCounter;
  exclusiveOwner = newExclusiveOwner;
  pthread_cond_broadcast(&exclusiveChangeCondition);
  m_MapMutex.unlock();
}

void M2File::releaseExclusiveThreadCount(size_t recurseCounter)
{
  m_MapMutex.lock();
  recurseCount-=recurseCounter;
  if(!recurseCount)
    {
      clearThread(exclusiveOwner);
      pthread_cond_broadcast(&exclusiveChangeCondition);
    }
  else
    {
    }
  m_MapMutex.unlock();
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
	      clearThread(file->exclusiveOwner);
	    pthread_cond_broadcast(&file->exclusiveChangeCondition);
	  }
      }
    else
      {
	//if switching into exclusive mode, the current owner becomes the thread that did the switching by default
	if(threadMode==2)
	  file->setExclusiveOwner(pthread_self(),0);
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
	file->waitExclusiveThreadAcquire(1);
	return file->unsyncState;
      }
    else // if(file->currentThreadMode==2)
      {
	//get thread id
	pthread_t localId = pthread_self();
	file->m_MapMutex.lock();
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
	file->m_MapMutex.unlock();
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
	file->releaseExclusiveThreadCount(1);
      }
    else if(file->currentThreadMode==2)
      {
	//get thread id
	pthread_t localId = pthread_self();
	file->m_MapMutex.lock();
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
	file->m_MapMutex.unlock();


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
    //for any mode besides thread excllusive we may ignore this function call
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
