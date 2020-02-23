#include <errno.h>
#ifndef ENOSYS
#define ENOSYS 0
#endif
int socket() { errno = ENOSYS; return -1; }
