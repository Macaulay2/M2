#ifndef _mutex_h_
#define _mutex_h_
#include <pthread.h>
#undef ERROR			/* undo mingw64 damage */
#include <../include/M2/config.h>
#ifdef __cplusplus
extern "C" {
#endif

struct spinlockStructure
{
  volatile int m_MutexInt;
};
typedef struct spinlockStructure spinLock;
  static const spinLock uninitializedSpinLock = {0};
static inline void initializeSpinLock(struct spinlockStructure* sls)
{
  __sync_lock_release(&sls->m_MutexInt);
}
static inline void acquireSpinLock(struct spinlockStructure* sls)
{
  int res = __sync_lock_test_and_set(&sls->m_MutexInt,1);
  while(res==1)
    {
      #if (defined(ARCH_X86) || defined(ARCH_X86_64)) && defined(__GNUC__)
      __asm__("pause");
      #endif
      res = __sync_lock_test_and_set(&sls->m_MutexInt,1);
    }
}
static inline void releaseSpinLock(struct spinlockStructure* sls)
{
  __sync_lock_release(&sls->m_MutexInt);
}

#ifdef __cplusplus
};
#endif

#endif
