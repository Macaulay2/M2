#ifndef __EXCEPTION_HH
#define __EXCEPTION_HH

#include <iostream.h>

class Exception
{
protected:
  char nm[512];
public:
  Exception();
  Exception(const Exception &c);

  void append(const char *str);
  void append(int x);
  void prepend(const char *str);
  
  friend ostream &operator<<(ostream &o, Exception &except);
};

class Internal_Exception : public Exception
{
public:
  Internal_Exception(const char *desc)
    {
      append("Internal error (bug): "); 
      append(desc);
    }
};

class Not_Implemented_Exception : public Exception
{
public:
  Not_Implemented_Exception(const char *desc)
    {
      append("Not yet implemented: "); 
      append(desc);
    }
};

#endif
