#ifndef _mutex_h_
#define _mutex_h_
#include <pthread.h>

#define INTELPAUSE
#if defined(__GNUC__)
struct spinlockStructure
{
  volatile int m_MutexInt;
};

void initializeSpinlock(struct spinlockStructure* sls)
{
  sls->m_MutexInt = 0;
}
void acquireSpinLock(struct spinlockStructure* sls)
{
  int res = __sync_lock_test_and_set(&sls->m_MutexInt,1);
  while(res==1)
    {
      #ifdef INTELPAUSE
      __asm__("pause");
      #endif
      res = __sync_lock_test_and_set(&sls->m_MutexInt,1);
    }
}
void releaseSpinLock(struct spinlockStructure* sls)
{
  sls->m_MutexInt = 0;
}
#endif

#endif
