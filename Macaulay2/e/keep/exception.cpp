#include "exception.hh"
#include <stdio.h>
#include <string.h>

Exception::Exception()
{
  nm[0] = '\0';
}

Exception::Exception(const Exception &c)
{
  strcpy(nm, c.nm);
}

void Exception::append(const char *str)
{
  strcat(nm, str);
}

void Exception::prepend(const char *str)
{
  char buf[512];
  strcpy(buf, nm);
  strcpy(nm, str);
  strcat(nm, buf);
}

void Exception::append(int x)
{
  sprintf(nm+strlen(nm), "%d", x);
}

ostream &operator<<(ostream &o, Exception &except)
{ 
  return o << except.nm;
}

