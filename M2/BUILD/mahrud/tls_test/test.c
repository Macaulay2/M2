#include <pthread.h>
#include <stdio.h>
#include <threads.h>

thread_local unsigned int tlcounter = 0;
unsigned int counter = 0;
pthread_mutex_t cout_mutex;

void incr(const char name)
{
  ++counter;
  ++tlcounter;
  pthread_mutex_lock(&cout_mutex);
  printf("Value for %c:\t%d\tvs %d\n", name, counter, tlcounter);
  pthread_mutex_unlock(&cout_mutex);
  pthread_exit(NULL);
}
