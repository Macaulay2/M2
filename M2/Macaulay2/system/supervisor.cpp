#include "supervisor.hpp"

#include <stdlib.h>
#include <assert.h>

#include <atomic>
#include <iostream>
#include <chrono>
#include <thread>
#include <mutex>
using std::lock_guard;

// The maximum number of concurrent threads
const static unsigned int numCores = std::thread::hardware_concurrency();
// We allocate between 5 and 17 threads initially, to save trouble with memory allocation.
const static int maxNumThreads = ((numCores < 4) ? 4 : (16 < numCores ? 16 : numCores)) + 1;

// The number of compute-bound threads allowed at any given time should be the number of cores and pseudocores.
// There may be I/O bound threads, such as the main interpreter thread.  So a good thing to set currentAllowedThreads to is the
// number of cores plus the expected number of I/O bound threads.
static atomic_int currentAllowedThreads(5);


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
    lock_guard<pthreadMutex> supLock(threadSupervisor->m_Mutex);
    lock_guard<pthreadMutex> lock(task->m_Mutex);
    if(task->m_ReadyToRun)
	return;
    if(task->m_Dependencies.empty())
      {
	task->m_ReadyToRun=true;
	threadSupervisor->m_ReadyTasks.push_back(task);
	pthread_cond_signal(&threadSupervisor->m_TaskReadyToRunCondition);
      }
    else
      {
	threadSupervisor->m_WaitingTasks.push_back(task);
      }
  }

  void delThread(pthread_t thread)
  {
    lock_guard<pthreadMutex> supLock(threadSupervisor->m_Mutex);
    gc_map(pthread_t, struct ThreadSupervisorInformation*)::iterator it = threadSupervisor->m_ThreadMap.find(thread);
    if(it!=threadSupervisor->m_ThreadMap.end())
      {
	threadSupervisor->m_ThreadMap.erase(it);
      }
  }
  void addCancelTask(struct ThreadTask* task, struct ThreadTask* cancel)
  {
    lock_guard<pthreadMutex> lock(task->m_Mutex);
    task->m_CancelTasks.insert(cancel);
  }
  void addStartTask(struct ThreadTask* task, struct ThreadTask* start)
  {
    lock_guard<pthreadMutex> lock(task->m_Mutex);
    task->m_StartTasks.insert(start);
  }
  void addDependency(struct ThreadTask* task, struct ThreadTask* dependency)
  {
    dependency->m_Mutex.lock();
    dependency->m_StartTasks.insert(task);
    dependency->m_Mutex.unlock(); 
    lock_guard<pthreadMutex> lock(task->m_Mutex);
    task->m_Dependencies.insert(dependency);
  }
  int taskDone(struct ThreadTask* task)
  {
    if (! task)	return false;
    lock_guard<pthreadMutex> lock(task->m_Mutex);
	// synchronizes-with the task's thread, avoids data races
    return task->m_Done;
  }
  int taskStarted(struct ThreadTask* task)
  {
    if (! task)	return false;
    lock_guard<pthreadMutex> lock(task->m_Mutex);
	// synchronizes-with the task's thread, avoids data races
    return task->m_Started;
  }
  void* taskResult(struct ThreadTask* task)
  {
    lock_guard<pthreadMutex> lock(task->m_Mutex);
	// synchronizes-with the task's thread, avoids data races
    return task->m_Result;
  }
  int taskKeepRunning(struct ThreadTask* task)
  {
    lock_guard<pthreadMutex> lock(task->m_Mutex);
	// synchronizes-with the task's thread, avoids data races
    return task->m_KeepRunning;
  }
  int taskRunning(struct ThreadTask* task)
  {
    lock_guard<pthreadMutex> lock(task->m_Mutex);
	// synchronizes-with the task's thread, avoids data races
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
    lock_guard<pthreadMutex> supLock(threadSupervisor->m_Mutex);
    if(threadSupervisor->m_ThreadLocalIdPtrSet.find(refno)!=threadSupervisor->m_ThreadLocalIdPtrSet.end())
	return;
    threadSupervisor->m_ThreadLocalIdPtrSet.insert(refno);
    int ref = threadSupervisor->m_ThreadLocalIdCounter++;
    if(ref>threadSupervisor->s_MaxThreadLocalIdCounter)
     abort();
    *refno = ref;
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
  lock_guard<pthreadMutex> lock(m_Mutex);
  while(!m_Done && m_KeepRunning)
    {
      pthread_cond_wait(&m_FinishCondition,&m_Mutex.m_Mutex);
    }
  return m_Result;
}

void staticThreadLocalInit()
{
  THREADLOCALINIT(interrupts_exceptionFlag);
  THREADLOCALINIT(interrupts_interruptedFlag);
  //Make ABSOLUTELY sure that the exception and interrupt flags are not set.
  //This memory should be initialized to zero, but doesn't seem to be on all systems.
  atomic_store(&THREADLOCAL(interrupts_interruptedFlag,struct atomic_field).field,0);
  atomic_store(&THREADLOCAL(interrupts_exceptionFlag,struct atomic_field).field,0);
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
  if(pthread_cond_init(&m_TaskReadyToRunCondition,NULL))
    abort();
  //force everything to get done just in case there is some weird GC issue.
  std::atomic_signal_fence(std::memory_order_seq_cst);
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
// done or canceled; task's mutex is locked by caller
void ThreadSupervisor::_i_finished(struct ThreadTask* task)
{
  lock_guard<pthreadMutex> supLock(m_Mutex);
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
}
void ThreadSupervisor::_i_startTask(struct ThreadTask* task, struct ThreadTask* launcher)
{
  lock_guard<pthreadMutex> supLock(m_Mutex);
  lock_guard<pthreadMutex> lock(task->m_Mutex);
  if(task->m_ReadyToRun)
      return;
  if(!task->m_Dependencies.empty())
    {
      if(!launcher)
	  return;
      gc_set(struct ThreadTask*)::iterator it = task->m_Dependencies.find(launcher);
      if(it!=task->m_Dependencies.end())
	{
	  task->m_Dependencies.erase(launcher);
	  task->m_FinishedDependencies.insert(launcher);
	}
      if(!task->m_Dependencies.empty())
	  return;
    }
  m_WaitingTasks.remove(task);
  task->m_ReadyToRun=true;
  m_ReadyTasks.push_back(task);
  if(pthread_cond_signal(&m_TaskReadyToRunCondition))
    abort();
}
void ThreadSupervisor::_i_cancelTask(struct ThreadTask* task)
{
  lock_guard<pthreadMutex> supLock(m_Mutex);
  lock_guard<pthreadMutex> lock(task->m_Mutex);
  if(task->m_CurrentThread)
    {
      atomic_store(&task->m_CurrentThread->m_Interrupt->field, 1);
      atomic_store(&task->m_CurrentThread->m_Exception->field, 1);
    }
  task->m_KeepRunning=false;
}
struct ThreadTask* ThreadSupervisor::getTask()
{
  lock_guard<pthreadMutex> supLock(m_Mutex);
  while(m_ReadyTasks.empty())
    {
      //This exists in case pthread cond wait returns due to a signal/etc
    RESTART:
      if(pthread_cond_wait(&m_TaskReadyToRunCondition,&m_Mutex.m_Mutex))
	goto RESTART;
    }
  if(m_ReadyTasks.empty())
    goto RESTART;
  struct ThreadTask* task = m_ReadyTasks.front();
  m_ReadyTasks.pop_front();
  m_RunningTasks.push_back(task);
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
  lock_guard<pthreadMutex> lock(m_Mutex);
  m_Running=false;
  threadSupervisor->_i_finished(this);
  if(!m_KeepRunning)
      return;
  m_Done = true;
  m_CurrentThread=NULL;
  //cancel stuff_
  for(gc_set(ThreadTask*)::iterator it = m_CancelTasks.begin(); it!=m_CancelTasks.end(); ++it)
    threadSupervisor->_i_cancelTask(*it);
  //start stuff
  for(gc_set(ThreadTask*)::iterator it = m_StartTasks.begin(); it!=m_StartTasks.end(); ++it)
    threadSupervisor->_i_startTask(*it,this);
}

SupervisorThread::SupervisorThread(int localThreadId):m_KeepRunning(true),m_LocalThreadId(localThreadId)
{
  m_ThreadLocal = new (GC) void*[ThreadSupervisor::s_MaxThreadLocalIdCounter];
  std::atomic_signal_fence(std::memory_order_seq_cst);
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
      atomic_store(&m_Interrupt->field, 0);
      struct ThreadTask* task = threadSupervisor->getTask();
      task->run(this);
    }
}
