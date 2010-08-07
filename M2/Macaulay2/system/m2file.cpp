#include "m2file.hpp"

extern stdio0_fileOutputSyncState stdio0_newDefaultFileOutputSyncState();

M2File::M2File(stdio0_fileOutputSyncState fileUnsyncState):
  currentThreadMode(0),unsyncState(fileUnsyncState)
{
  pthread_mutex_init(&mapMutex,NULL);
  pthread_mutex_init(&outputMutex,NULL);
}

extern "C"
{

  int M2File_GetThreadMode(M2File* file) { return file->currentThreadMode; }

  void M2File_SetThreadMode(M2File* file, int threadMode) { file->currentThreadMode = threadMode; }

  struct M2File* M2File_New(stdio0_fileOutputSyncState fileUnsyncState) { return new M2File(fileUnsyncState); } 
  stdio0_fileOutputSyncState M2File_UnsyncState(M2File* file) { return file->unsyncState; } 
  stdio0_fileOutputSyncState M2File_GetState(struct M2File* file)
  {
    if(file->currentThreadMode==0)
      {
	return file->unsyncState;
      }
    else if(file->currentThreadMode==1)
      {
	pthread_mutex_lock(&file->outputMutex);
	return file->unsyncState;
      }
    else if(file->currentThreadMode==2)
      {
	pthread_t localId = pthread_self();
	pthread_mutex_lock(&file->mapMutex);
	
	pthread_mutex_unlock(&file->mapMutex);
      }
  }
  void M2File_ReleaseState(struct M2File* file)
  {
    if(file->currentThreadMode==0)
      {
      }
    else if(file->currentThreadMode==1)
      {
	pthread_mutex_unlock(&file->outputMutex);
      }
    else if(file->currentThreadMode==2)
      {
	pthread_t localId = pthread_self();
	pthread_mutex_lock(&file->mapMutex);
	
	pthread_mutex_unlock(&file->mapMutex);


      }
  }

};
