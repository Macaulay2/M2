#include <errno.h>
#ifndef ENOSYS
#define ENOSYS 0
#endif
int setsockopt() { errno = ENOSYS; return -1; }
