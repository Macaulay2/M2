// (c) 1997 Michael E. Stillman

#ifndef _error_hh_
#define _error_hh_

void ERROR(char *s);
void ERROR(int n);
bool error();
char *error_message();
void clear_error();

#endif
