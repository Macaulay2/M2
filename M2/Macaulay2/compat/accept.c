#include <errno.h>
#ifndef ENOSYS
#define ENOSYS 0
#endif
int accept() { errno = ENOSYS; return -1; }
