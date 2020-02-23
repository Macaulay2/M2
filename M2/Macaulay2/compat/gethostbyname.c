#include <errno.h>
#ifndef ENOSYS
#define ENOSYS 0
#endif
void *gethostbyname() { errno = ENOSYS; return (void *)0; }
