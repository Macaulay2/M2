#include <errno.h>
#ifndef ENOSYS
#define ENOSYS 0
#endif
void *getprotobyname() { errno = ENOSYS; return 0; }
