#ifndef _system_supervisor_h_
#define _system_supervisor_h_

#include <pthread.h>
#include <list>
#include <map>

typedef struct parse_ThreadCellBody_struct * parse_ThreadCellBody;

typedef void* (*ThreadTaskFunctionPtr)(void*);

//not garbage collected
struct ThreadTask
{
  //Name of task -- NULL for not used.
  const char* m_Name;
  //function to call
  ThreadTaskFunctionPtr m_Func;
  //data to pass into function
  void* m_UserData;
  //result of task
  void* m_Result;
  //is the task done
  bool m_Done;
  //has the task started
  bool m_Started;
  //tasks to cancel upon completion
  std::list<ThreadTask*> m_CancelTasks;
  //tasks to start upon completion
  std::list<ThreadTask*> m_StartTasks;
};


//not garbage collected
struct ThreadSupervisorInformation
{
  //Id for thread
  pthread_t m_ThreadId;
  //body for the thread (remember this is a pointer)
  parse_ThreadCellBody m_Body;
  //Currently running task
  struct ThreadTask* m_Task;
};

//singleton -- not garbage collected
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

#include "supervisorinterface.h"

#endif
