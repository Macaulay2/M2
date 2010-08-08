#ifndef _system_supervisor_h_
#define _system_supervisor_h_

#include <pthread.h>
#include <list>
#include <map>

typedef struct parse_ThreadCellBody_struct * parse_ThreadCellBody;


//not garbage collected
struct ThreadSupervisorInformation
{
  //Id for thread
  pthread_t m_ThreadId;
  //body for the thread (remember this is a pointer)
  parse_ThreadCellBody m_Body;
};

struct ThreadSupervisor
{
  ThreadSupervisor();
  ~ThreadSupervisor();
  //Target number of threads to have running at once.
  int m_TargetNumThreads;
  //map between pthread id's and thread information structures
  std::map<pthread_t, struct ThreadSupervisorInformation*> m_ThreadMap;
  pthread_mutex_t m_Mutex;
};

typedef void* (*ThreadTaskFunctionPtr)(void*);

struct ThreadTask
{
  //Name of task -- NULL for not used.
  const char* m_Name;
  //function to call
  ThreadTaskFunctionPtr m_Func;
  //data to pass into function
  void* m_UserData;
  void* m_Result;
  //tasks to cancel upon completion
  std::list<ThreadTask*> m_CancelTasks;
  //tasks to start upon completion
  std::list<ThreadTask*> m_StartTasks;
};

#include "supervisorinterface.h"

#endif
