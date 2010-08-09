#ifndef _system_supervisor_h_
#define _system_supervisor_h_

#include <pthread.h>
#include <set>
#include <map>
#include <list>

typedef struct parse_ThreadCellBody_struct * parse_ThreadCellBody;

typedef void* (*ThreadTaskFunctionPtr)(void*);

//not garbage collected
struct ThreadTask
{
  ThreadTask(const char* name, ThreadTaskFunctionPtr func, void* userData, bool timeLimit, time_t timeLimitSeconds);
  ~ThreadTask();
  ///Name of task -- NULL for not used.
  const char* m_Name;
  ///function to call
  ThreadTaskFunctionPtr m_Func;
  ///data to pass into function
  void* m_UserData;
  ///result of task
  void* m_Result;
  ///is the task done
  bool m_Done;
  ///has the task started
  bool m_Started;
  ///Should the task keep running
  bool m_KeepRunning;
  ///tasks to cancel upon completion
  std::set<ThreadTask*> m_CancelTasks;
  ///tasks to start upon completion
  std::set<ThreadTask*> m_StartTasks;
  ///Is there a time limit for this task
  bool m_TimeLimit;
  ///Time limit in seconds for this task.  
  time_t m_Seconds;
  ///Dependencies that must be satisfied in order to start
  std::set<ThreadTask*> m_Dependencies;
  ///Dependencies that have been finished
  std::set<ThreadTask*> m_FinishedDependencies;
  ///Mutex for accessing task
  pthread_mutex_t m_Mutex;
  ///run task
  void run();

};


//not garbage collected
struct ThreadSupervisorInformation
{
  ///Id for thread
  pthread_t m_ThreadId;
  ///body for the thread (remember this is a pointer)
  parse_ThreadCellBody m_Body;
  ///Currently running task
  struct ThreadTask* m_Task;
};


class SupervisorThread
{
public:
  SupervisorThread();
  pthread_t ThreadId() { return m_ThreadId; }
  void start();
  void shutdown() { m_KeepRunning = false; }
  static void* threadEntryPoint(void* st) { ((SupervisorThread*)st)->threadEntryPoint(); }
 protected:
  void threadEntryPoint(); 
  pthread_t m_ThreadId;
  volatile bool m_KeepRunning;
};


//singleton -- not garbage collected
struct ThreadSupervisor
{
  ThreadSupervisor(int targetNumThreads);
  ~ThreadSupervisor();
  void _i_startTask(struct ThreadTask* task, struct ThreadTask* launcher);
  void _i_cancelTask(struct ThreadTask* task);
  void _i_finished(struct ThreadTask* task);
  struct ThreadTask* getTask();
  ///Target number of threads to have running at once.
  int m_TargetNumThreads;
  ///map between pthread id's and thread information structures
  std::map<pthread_t, struct ThreadSupervisorInformation*> m_ThreadMap;
  ///list of ready to go tasks
  std::list<ThreadTask*> m_ReadyTasks;
  ///list of running tasks
  std::list<ThreadTask*> m_RunningTasks;
  ///list of tasks waiting for dependencies
  std::list<ThreadTask*> m_WaitingTasks;
  ///list of tasks that are finished
  std::list<ThreadTask*> m_FinishedTasks;
  ///list of canceled
  std::list<ThreadTask*> m_CanceledTasks;
  ///mutex for accessing lists
  pthread_mutex_t m_Mutex;
  ///new task waiting
  pthread_cond_t m_TaskWaitingCondition;
  ///list of supervisor threads
  std::list<SupervisorThread*> m_Threads;
  ///initialize
  void initialize();
};

#include "supervisorinterface.h"

#endif
