#include "supervisor.hpp"
#include "supervisorinterface.h"
#include <iostream>
#include <stdlib.h>

static bool finished[2000][2000];

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
	  tasks[i][j] = createThreadTask("Test",TS_Test1_Func,tup,0,0);
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
}

int TS_Test()
{
  TS_Test1();
}
