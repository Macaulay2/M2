#include <pthread.h>
#include <stdatomic.h>
#include <stdio.h>

atomic_int acnt;
int cnt;

void adding()
{
  for(int i=0; i<10000; i++)
    {
      acnt++;
      cnt++;
    }
  pthread_exit(NULL);
}
