#include "supervisor.hpp"
#include "supervisorinterface.h"
#include <iostream>
#include <stdlib.h>
#include <M2/config.h>
#ifdef HAVE_UNISTD_H
// "sleep" is declared here:
#include <unistd.h>
#endif

static volatile bool finished[2000][2000];

struct tuple
{
  int x, y;
};

static void* TS_Test1_Func(void* vtup)
{
  struct tuple* tup = (struct tuple*) vtup;
  for(int j = 0; j < tup->y; ++j)
    if(!finished[tup->x][j])
      abort();
  finished[tup->x][tup->y]=1;
  return NULL;
}

static int TS_Test1()
{
  struct ThreadTask* tasks[200][200];
  for(int i = 0; i < 200; ++i)
    {
      for(int j = 0; j < 20; ++j)
	{
	  struct tuple* tup = new tuple();
	  tup->x = i;
	  tup->y = j;
	  tasks[i][j] = createThreadTask("Test",TS_Test1_Func,tup,0,0,0);
	}
    }
  for(int i = 20-1; i>=0; --i)
    for(int j = 20-1; j>=0; --j)
      {
	for(int m = 0; m < j; ++m)
	  {
	    addDependency(tasks[i][j],tasks[i][m]);
	  }
	pushTask(tasks[i][j]);
      }
  waitOnTask(tasks[20-1][20-1]);
  return 0;
}

static volatile bool canceled = false;
static volatile bool started=false;

static void* TS_Test2_Func1(void* vtup)
{
  started=true;
  while(!AO_load(&THREADLOCAL(interrupts_interruptedFlag,struct atomic_field).field))
    {
     sleep(0);
     }
   canceled = true;
   return NULL;
}

static void* TS_Test2_Func2(void* vtup)
 {
  return NULL;
 }

static int TS_Test2()
 {
   for(int i = 0; i < 1; ++i)
     {
       canceled=false;
       started=false;
       ThreadTask* task1 = createThreadTask("Task1",TS_Test2_Func1,NULL,0,0,0);
       ThreadTask* task2 = createThreadTask("Task2",TS_Test2_Func2,NULL,0,0,0);
       addCancelTask(task2,task1);
       pushTask(task1);
       pushTask(task2);
       waitOnTask(task1);
       assert(canceled || !started);
     }
   return 0;
 }

extern "C" {
 void TS_Test()
 {
   TS_Test1();
   TS_Test2();
}
}
