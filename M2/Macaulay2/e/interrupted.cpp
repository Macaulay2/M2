#include "M2/config.h"
#include <iostream>
#ifdef WITH_MPI
#include <mpi.h>
#endif
#include "../system/supervisor.hpp"
#include "../system/supervisorinterface.h"
#define interrupted() \
  test_Field(THREADLOCAL(interrupts_interruptedFlag, struct atomic_field))

bool system_interrupted() {
#ifdef WITH_MPI
  const int MPI_INTERRUPT_TAG = 3210;
  int flag = 0;
  const int master = 0;
  MPI_Iprobe(master, MPI_INTERRUPT_TAG, MPI_COMM_WORLD, &flag, MPI_STATUS_IGNORE);
  if (flag) {
    // Get the rank of the process
    int world_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
    std::cout << "MPI_INTERRUPT_TAG received by " << world_rank << std::endl;
    // set the flag
    tryGlobalInterrupt();
    store_Field(THREADLOCAL(interrupts_interruptedFlag, struct atomic_field),true);
    // pop the MPI message
    MPI_Recv(nullptr, 0, MPI_INT, master, MPI_INTERRUPT_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }
#endif
  return interrupted();
}
