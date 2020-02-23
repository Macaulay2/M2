#include <errno.h>
#ifndef ENOSYS
#define ENOSYS 0
#endif
int bind() { errno = ENOSYS; return -1; }
