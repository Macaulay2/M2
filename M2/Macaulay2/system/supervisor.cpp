#include "supervisor.hpp"

#include <stdlib.h>
#include <assert.h>

#include <iostream>
#include <chrono>
#include <thread>

// The maximum number of concurrent threads
const static unsigned int numCores = std::thread::hardware_concurrency();
// We allocate between 4 and 16 threads initially, to save trouble with memory allocation.
const static int maxNumThreads = (numCores < 4) ? 4 : (16 < numCores ? 16 : numCores);

// The number of compute-bound threads allowed at any given time should be the number of cores and pseudocores.
// There may be I/O bound threads, such as the main interpreter thread.  So a good thing to set currentAllowedThreads to is the
// number of cores plus the expected number of I/O bound threads.
static int currentAllowedThreads = 2;


// The thread that the interpreter runs in.
pthread_t interpThread;

extern "C" {

  // TODO: replace with STL linked list traversal
  extern void reverse_run(struct FUNCTION_CELL *list)
  {
    if(list != NULL) {
      reverse_run(list->next);
      (*list->func)();
    }
  }

  extern void setInterpThread()
  {
    interpThread=pthread_self();
  }
  extern int tryGlobalInterrupt()
  {
    if(interpThread==pthread_self()) {
      return 0;
    } else {
      pthread_kill(interpThread,SIGINT);
      return -1;
    }
  }
  extern int tryGlobalAlarm()
  {
    if(interpThread==pthread_self()) {
      return 0;
    } else {
      pthread_kill(interpThread,SIGALRM);
      return -1;
    }
  }
  extern int tryGlobalTrace()
  {
    if(interpThread==pthread_self()) {
      return 0;
    } else {
      pthread_kill(interpThread,SIGUSR1);
      return -1;
    }
  }

  THREADLOCALDECL(struct atomic_field, interrupts_interruptedFlag);
  THREADLOCALDECL(struct atomic_field, interrupts_exceptionFlag);
  struct ThreadSupervisor* threadSupervisor = 0 ;
  void initializeThreadSupervisor()
  {
    if(NULL==threadSupervisor)
      threadSupervisor = new (GC) ThreadSupervisor(maxNumThreads);
    assert(threadSupervisor);
    threadSupervisor->m_TargetNumThreads=maxNumThreads;
    threadSupervisor->initialize();
  }
  void pushTask(struct ThreadTask* task)
  {
    threadSupervisor->m_Mutex.lock();
    task->m_Mutex.lock();
    if(task->m_ReadyToRun)
      {
	task->m_Mutex.unlock();
	return;
      }
    if(task->m_Dependencies.empty())
      {
	task->m_ReadyToRun=true;
	threadSupervisor->m_ReadyTasks.push_back(task);
      }
    else
      {
	threadSupervisor->m_WaitingTasks.push_back(task);
      }
    pthread_cond_signal(&threadSupervisor->m_TaskWaitingCondition);
    task->m_Mutex.unlock();
    threadSupervisor->m_Mutex.unlock();
  }

  void delThread(pthread_t thread)
  {
    threadSupervisor->m_Mutex.lock();
    gc_map(pthread_t, struct ThreadSupervisorInformation*)::iterator it = threadSupervisor->m_ThreadMap.find(thread);
    if(it!=threadSupervisor->m_ThreadMap.end())
      {
	threadSupervisor->m_ThreadMap.erase(it);
      }
    threadSupervisor->m_Mutex.unlock();
  }
  void addCancelTask(struct ThreadTask* task, struct ThreadTask* cancel)
  {
    task->m_Mutex.lock();
    task->m_CancelTasks.insert(cancel);
    task->m_Mutex.unlock();
  }
  void addStartTask(struct ThreadTask* task, struct ThreadTask* start)
  {
    task->m_Mutex.lock();
    task->m_StartTasks.insert(start);
    task->m_Mutex.unlock();
  }
  void addDependency(struct ThreadTask* task, struct ThreadTask* dependency)
  {
    dependency->m_Mutex.lock();
    dependency->m_StartTasks.insert(task);
    dependency->m_Mutex.unlock(); 
    task->m_Mutex.lock();
    task->m_Dependencies.insert(dependency);
    task->m_Mutex.unlock();
  }
  int taskDone(struct ThreadTask* task)
  {
    return task && task->m_Done;
  }
  int taskStarted(struct ThreadTask* task)
  {
    return task && task->m_Started;
  }
  void* taskResult(struct ThreadTask* task)
  {
    return task->m_Result;
  }
  int taskKeepRunning(struct ThreadTask* task)
  {
    return task->m_KeepRunning;
  }
  int taskRunning(struct ThreadTask* task)
  {
    return task->m_Running;
  }
  void taskInterrupt(struct ThreadTask* task)
  {
    threadSupervisor->_i_cancelTask(task);
  }

  struct ThreadTask* createThreadTask(const char* name, ThreadTaskFunctionPtr func, void* userData, int timeLimitExists, time_t timeLimitSeconds, int isM2Task)
  {
    return new (GC) ThreadTask(name,func,userData,(bool)timeLimitExists,timeLimitSeconds,isM2Task);
  }
  void* waitOnTask(struct ThreadTask* task)
  {
    return task->waitOn();
  }
  void** TS_Get_LocalArray() {  return (void**)pthread_getspecific(*(pthread_key_t*)threadSupervisor); }
  void** TS_Get_Local(int refno) { return &TS_Get_LocalArray()[refno]; }
  void TS_Add_ThreadLocal(int* refno, const char* name)
  {
    if(NULL == threadSupervisor)
      threadSupervisor = new (GC) ThreadSupervisor(maxNumThreads);
    assert(threadSupervisor);
    threadSupervisor->m_Mutex.lock();
    if(threadSupervisor->m_ThreadLocalIdPtrSet.find(refno)!=threadSupervisor->m_ThreadLocalIdPtrSet.end())
      {
	threadSupervisor->m_Mutex.unlock();
	return;
      }
    threadSupervisor->m_ThreadLocalIdPtrSet.insert(refno);
    int ref = threadSupervisor->m_ThreadLocalIdCounter++;
    if(ref>threadSupervisor->s_MaxThreadLocalIdCounter)
     abort();
    *refno = ref;
    threadSupervisor->m_Mutex.unlock();
  }
  int getAllowableThreads()
  {
    return currentAllowedThreads;
  }
  int getMaxAllowableThreads()
  {
    return maxNumThreads;
  }
  void setAllowableThreads(int numThreads)
  {
    currentAllowedThreads=numThreads;
  }
};

ThreadTask::ThreadTask(const char* name, ThreadTaskFunctionPtr func, void* userData, bool timeLimit, time_t timeLimitSeconds, bool isM2Task):
  m_Name(name),m_Func(func),m_UserData(userData),m_Result(NULL),m_IsM2Task(isM2Task),m_Done(false),m_Started(false),m_KeepRunning(true),m_ReadyToRun(false),m_Running(false),m_TimeLimit(timeLimit),m_Seconds(timeLimitSeconds),m_CurrentThread(NULL)
{
   if(pthread_cond_init(&m_FinishCondition,NULL))
    abort();
}
ThreadTask::~ThreadTask() {}
void* ThreadTask::waitOn()
{
  m_Mutex.lock();
  while(!m_Done && m_KeepRunning)
    {
      pthread_cond_wait(&m_FinishCondition,&m_Mutex.m_Mutex);
    }
  assert(m_Done || !m_KeepRunning);
  void* ret = m_Result;
  m_Mutex.unlock();
  return ret;
}

void staticThreadLocalInit()
{
  THREADLOCALINIT(interrupts_exceptionFlag);
  THREADLOCALINIT(interrupts_interruptedFlag);
  //Make ABSOLUTELY sure that the exception and interrupt flags are not set.
  //This memory should be initialized to zero, but doesn't seem to be on all systems.
  AO_store(&THREADLOCAL(interrupts_interruptedFlag,struct atomic_field).field,0);
  AO_store(&THREADLOCAL(interrupts_exceptionFlag,struct atomic_field).field,0);
}


ThreadSupervisor::ThreadSupervisor(int targetNumThreads):
  m_TargetNumThreads(targetNumThreads),m_ThreadLocalIdCounter(1)
{
  threadSupervisor=this;
  //if using get specific, create the thread specific key for the thread local memory block
  #ifdef GETSPECIFICTHREADLOCAL
  //create key, abort on error
  if(pthread_key_create(&m_ThreadSpecificKey,NULL))
    abort();
  //create new thread local memory block
  m_LocalThreadMemory = new (GC) void*[ThreadSupervisor::s_MaxThreadLocalIdCounter];
  //make really really sure it is zero
  memset(m_LocalThreadMemory,0,sizeof(void*)*ThreadSupervisor::s_MaxThreadLocalIdCounter);
  //set memory block location for main thread.  Main thread doesn't do anything, so not really used.
  //however, there are plenty of initializations in it anyway.
  if(pthread_setspecific(threadSupervisor->m_ThreadSpecificKey,m_LocalThreadMemory))
    abort();
  #endif
  //if not using get specific no initialization is necessary
  //initialize task waiting condition
  if(pthread_cond_init(&m_TaskWaitingCondition,NULL))
    abort();
  //force everything to get done just in case there is some weird GC issue.
  AO_compiler_barrier();
  //once everything is done initialize statics
  staticThreadLocalInit();
}

ThreadSupervisor::~ThreadSupervisor()
{
#ifdef GETSPECIFICTHREADLOCAL
  //if using get specific delete key that was used for thread specific memory
  if(pthread_key_delete(m_ThreadSpecificKey))
    abort();
#endif
}
void ThreadSupervisor::initialize()
{
  //initialize premade threads
  for(int i = 0; i < m_TargetNumThreads; ++i)
    {
      SupervisorThread* thread = new (GC) SupervisorThread(i);
      //critical -- we MUST push back before we start.
      m_Threads.push_back(thread);
      thread->start();
    }
}
void ThreadSupervisor::_i_finished(struct ThreadTask* task)
{
  m_Mutex.lock();
  m_RunningTasks.remove(task);
  if(task->m_KeepRunning)
    {
      m_FinishedTasks.push_back(task);
    }
  else
    {
      m_CanceledTasks.push_back(task);
    }
  if(pthread_cond_broadcast(&task->m_FinishCondition))
    abort();  
  m_Mutex.unlock();
}
void ThreadSupervisor::_i_startTask(struct ThreadTask* task, struct ThreadTask* launcher)
{
  m_Mutex.lock();
  task->m_Mutex.lock();
  if(task->m_ReadyToRun)
    {
      task->m_Mutex.unlock();
      m_Mutex.unlock();
      return;
    }
  if(!task->m_Dependencies.empty())
    {
      if(!launcher)
	{
	  task->m_Mutex.unlock();
	  m_Mutex.unlock();
	  return;
	}
      gc_set(struct ThreadTask*)::iterator it = task->m_Dependencies.find(launcher);
      if(it!=task->m_Dependencies.end())
	{
	  task->m_Dependencies.erase(launcher);
	  task->m_FinishedDependencies.insert(launcher);
	}
      if(!task->m_Dependencies.empty())
	{
	  task->m_Mutex.unlock();
	  m_Mutex.unlock();
	  return;
	}
    }
  m_WaitingTasks.remove(task);
  task->m_ReadyToRun=true;
  m_ReadyTasks.push_back(task);
  if(pthread_cond_signal(&m_TaskWaitingCondition))
    abort();
  task->m_Mutex.unlock();
  m_Mutex.unlock();
}
void ThreadSupervisor::_i_cancelTask(struct ThreadTask* task)
{
  m_Mutex.lock();
  task->m_Mutex.lock();
  if(task->m_CurrentThread)
    {
      AO_store(&task->m_CurrentThread->m_Interrupt->field,true);
      AO_store(&task->m_CurrentThread->m_Exception->field,true);
    }
  task->m_KeepRunning=false;
  task->m_Mutex.unlock();
  m_Mutex.unlock();
}
struct ThreadTask* ThreadSupervisor::getTask()
{
  m_Mutex.lock();
  while(m_ReadyTasks.empty())
    {
      //This exists in case pthread cond wait returns due to a signal/etc
    RESTART:
      if(pthread_cond_wait(&m_TaskWaitingCondition,&m_Mutex.m_Mutex))
	goto RESTART;
    }
  if(m_ReadyTasks.empty())
    goto RESTART;
  struct ThreadTask* task = m_ReadyTasks.front();
  m_ReadyTasks.pop_front();
  m_RunningTasks.push_back(task);
  m_Mutex.unlock();
  return task;
}
void ThreadTask::run(SupervisorThread* thread)
{
  m_Mutex.lock();
  if(!m_KeepRunning)
    {
      //if this is set it means the task was requested to terminate.
      threadSupervisor->_i_finished(this);
      m_Mutex.unlock();
      return;
    }
  m_CurrentThread=thread;
  m_Started = true;
  m_Running=true;
  m_Mutex.unlock();
  m_Result = m_Func(m_UserData);
  m_Mutex.lock();
  m_Running=false;
  if(!m_KeepRunning)
    {
      m_Mutex.unlock();
      return;
    }
  m_Done = true;
  m_CurrentThread=NULL;
  threadSupervisor->_i_finished(this);
  //cancel stuff_
  for(gc_set(ThreadTask*)::iterator it = m_CancelTasks.begin(); it!=m_CancelTasks.end(); ++it)
    threadSupervisor->_i_cancelTask(*it);
  //start stuff
  for(gc_set(ThreadTask*)::iterator it = m_StartTasks.begin(); it!=m_StartTasks.end(); ++it)
    threadSupervisor->_i_startTask(*it,this);
  m_Mutex.unlock();
}

SupervisorThread::SupervisorThread(int localThreadId):m_KeepRunning(true),m_LocalThreadId(localThreadId)
{
  m_ThreadLocal = new (GC) void*[ThreadSupervisor::s_MaxThreadLocalIdCounter];
  AO_compiler_barrier();
}
void SupervisorThread::start()
{
  const size_t min_stackSize = 8 * 1024 * 1024;
  size_t stackSize = 0;
  pthread_attr_t stackSizeAttribute;
  if (pthread_attr_init(&stackSizeAttribute)) 
    abort();
  if (pthread_attr_getstacksize(&stackSizeAttribute, &stackSize))
    abort();
  if (stackSize < min_stackSize && pthread_attr_setstacksize (&stackSizeAttribute, min_stackSize))
    abort();
  #define StackSizeParameter &stackSizeAttribute
  if (pthread_create(&m_ThreadId,StackSizeParameter,SupervisorThread::threadEntryPoint,this))
    perror("pthread_create: failed"), abort();
}
void SupervisorThread::threadEntryPoint()
{
  #ifdef GETSPECIFICTHREADLOCAL
  if(pthread_setspecific(threadSupervisor->m_ThreadSpecificKey,m_ThreadLocal))
    abort();
  #endif
  m_Interrupt=&THREADLOCAL(interrupts_interruptedFlag,struct atomic_field);
  m_Exception=&THREADLOCAL(interrupts_exceptionFlag,struct atomic_field);
  reverse_run(thread_prepare_list);// re-initialize any thread local variables
  while(m_KeepRunning)
    {
      if(currentAllowedThreads<=m_LocalThreadId)
	{
	  using namespace std::chrono_literals;
	  std::this_thread::sleep_for(1s);
	  continue;
	}
      AO_store(&m_Interrupt->field,false);
      struct ThreadTask* task = threadSupervisor->getTask();
      task->run(this);
    }
}
