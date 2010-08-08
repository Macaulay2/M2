#include "supervisor.hpp"


extern "C" {
  struct ThreadSupervisor threadSupervisor;

  void addThreadBody(pthread_t thread, parse_ThreadCellBody body)
  {
    pthread_mutex_lock(&threadSupervisor.m_Mutex);
    std::map<pthread_t, struct ThreadSupervisorInformation*>::iterator it = threadSupervisor.m_ThreadMap.find(thread);
    if(it==threadSupervisor.m_ThreadMap.end())
      {
	struct ThreadSupervisorInformation* tsi = new ThreadSupervisorInformation();
	tsi->m_ThreadId = thread;
	tsi->m_Body = body;
	threadSupervisor.m_ThreadMap[thread]=tsi;
      }
    else
      {
	it->second->m_Body=body;
      }
    pthread_mutex_unlock(&threadSupervisor.m_Mutex);
  }
  void addThread(pthread_t thread)
  {
    pthread_mutex_lock(&threadSupervisor.m_Mutex);
    std::map<pthread_t, struct ThreadSupervisorInformation*>::iterator it = threadSupervisor.m_ThreadMap.find(thread);
    if(it==threadSupervisor.m_ThreadMap.end())
      {
	struct ThreadSupervisorInformation* tsi = new ThreadSupervisorInformation();
	tsi->m_ThreadId = thread;
	tsi->m_Body = NULL;
	threadSupervisor.m_ThreadMap[thread]=tsi;
      }
    pthread_mutex_unlock(&threadSupervisor.m_Mutex);
  }
  void delThread(pthread_t thread)
  {
    pthread_mutex_lock(&threadSupervisor.m_Mutex);
    std::map<pthread_t, struct ThreadSupervisorInformation*>::iterator it = threadSupervisor.m_ThreadMap.find(thread);
    if(it!=threadSupervisor.m_ThreadMap.end())
      {
	threadSupervisor.m_ThreadMap.erase(it);
      }
    pthread_mutex_unlock(&threadSupervisor.m_Mutex);
  }
  
};


ThreadSupervisor::ThreadSupervisor()
{
  pthread_mutex_init(&m_Mutex,NULL);
}

ThreadSupervisor::~ThreadSupervisor()
{
}
