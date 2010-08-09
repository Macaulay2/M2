#include "m2file.hpp"

extern "C"
{
extern stdio0_fileOutputSyncState stdio0_newDefaultFileOutputSyncState();
};

M2File::M2File(stdio0_fileOutputSyncState fileUnsyncState):
  currentThreadMode(0),unsyncState(fileUnsyncState),exclusiveOwner(0),recurseCount(0)
{
  pthread_mutex_init(&mapMutex,NULL);

}
M2File::~M2File()
{
  for(std::map<pthread_t,struct M2FileThreadState*>::iterator it = threadStates.begin(); it!=threadStates.end(); ++it)
    {
      delete it->second;
    }
}


void M2File::waitExclusiveThread(size_t recurseCounter)
{
  pthread_mutex_lock(&mapMutex);
  while(1)
    {
      if(exclusiveOwner==pthread_self())
	{
	  recurseCounter+=recurseCounter;
	  pthread_mutex_unlock(&mapMutex);
	  return;
	}
      pthread_cond_wait(&exclusiveChangeCondition,&mapMutex);     
    }
  pthread_mutex_unlock(&mapMutex);
}

void M2File::waitExclusiveThreadAcquire(size_t recurseCounter)
{
  pthread_mutex_lock(&mapMutex);
  while(1)
    {
      if(exclusiveOwner==0)
	{
	  exclusiveOwner=pthread_self();
	  recurseCount=recurseCounter;
	  pthread_mutex_unlock(&mapMutex);
	  pthread_cond_broadcast(&exclusiveChangeCondition);
	  return;
	}
      if(exclusiveOwner==pthread_self())
	{
	  recurseCount+=recurseCounter;
	  pthread_mutex_unlock(&mapMutex);
	  return;
	}
      pthread_cond_wait(&exclusiveChangeCondition,&mapMutex);
      
    }
  pthread_mutex_unlock(&mapMutex);

}

void M2File::setExclusiveOwner(pthread_t newExclusiveOwner, size_t recurseCounter)
{
  pthread_mutex_lock(&mapMutex);
  recurseCount=recurseCounter;
  exclusiveOwner = newExclusiveOwner;
  pthread_cond_broadcast(&exclusiveChangeCondition);
  pthread_mutex_unlock(&mapMutex);
}

void M2File::releaseExclusiveThreadCount(size_t recurseCounter)
{
  pthread_mutex_lock(&mapMutex);
  recurseCount-=recurseCounter;
  if(!recurseCount)
    {
      exclusiveOwner=0;
      pthread_cond_broadcast(&exclusiveChangeCondition);
    }
  pthread_mutex_unlock(&mapMutex);
}

extern "C"
{

  int M2File_GetThreadMode(M2File* file) { return file->currentThreadMode; }

  void M2File_SetThreadMode(M2File* file, int threadMode) { file->currentThreadMode = threadMode; }

  struct M2File* M2File_New(stdio0_fileOutputSyncState fileUnsyncState) { return new M2File(fileUnsyncState); } 
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
    else if(file->currentThreadMode==2)
      {
	//get thread id
	pthread_t localId = pthread_self();
	pthread_mutex_lock(&file->mapMutex);
	//try to find thread id in thread states map
	std::map<pthread_t, struct M2FileThreadState*>::iterator it = file->threadStates.find(localId);
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
	    struct M2FileThreadState* state = new M2FileThreadState();
	    state->syncState = foss;
	    file->threadStates[localId]=state;
	  }
	pthread_mutex_unlock(&file->mapMutex);
	return foss;
      }
  }
  void M2File_ReleaseState(struct M2File* file)
  {
    if(file->currentThreadMode==0)
      {
      }
    else if(file->currentThreadMode==1)
      {
	file->releaseExclusiveThreadCount(1);
      }
    else if(file->currentThreadMode==2)
      {
	//get thread id
	pthread_t localId = pthread_self();
	pthread_mutex_lock(&file->mapMutex);
	//try to find thread id in thread states map
	std::map<pthread_t, struct M2FileThreadState*>::iterator it = file->threadStates.find(localId);
	if(it!=file->threadStates.end())
	  {
	    //not necessary to do anything here
	  }
	else
	  {
	    //this shouldn't happen, but we can safely ignore it if it does.  
	  }
	pthread_mutex_unlock(&file->mapMutex);


      }
  }
  void M2File_StartInput(struct M2File* file)
  {
    //for any mode besides thread excllusive we may ignore this function call
    if(file->currentThreadMode!=2)
      return;
    file->waitExclusiveThread(1);
  }
  void M2File_EndInput(struct M2File* file)
  {
    //for any mode besides thread excllusive we may ignore this function call
    if(file->currentThreadMode!=2)
      return;
    file->waitExclusiveThread(-1);
  }
  void M2File_StartOutput(struct M2File* file)
  {
    //for any mode besides thread excllusive we may ignore this function call
    if(file->currentThreadMode!=2)
      return;
    file->waitExclusiveThread(1);
  }
  void M2File_EndOutput(struct M2File* file)
  {
    //for any mode besides thread excllusive we may ignore this function call  
    if(file->currentThreadMode!=2)
      return;
    file->waitExclusiveThread(1);
  }

};
