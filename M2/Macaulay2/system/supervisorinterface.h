#ifndef _system_supervisorinterface_h_
#define _system_supervisorinterface_h_


#ifdef __cplusplus
extern "C" {
#endif

  struct ThreadSupervisor;
  extern struct ThreadSupervisor threadSupervisor;
  extern void addThreadBody(pthread_t thread, parse_ThreadCellBody body);
  extern void addThread(pthread_t thread);
  extern void delThread(pthread_t thread);

#ifdef __cplusplus
}
#endif



#endif
