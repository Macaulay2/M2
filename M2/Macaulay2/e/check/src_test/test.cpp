// test.cpp

#include "test.h"
#include <iostream>
#include <typeinfo>     // Visual Studio requires /GR""

#ifdef _MSC_VER
//Allow return-less mains:
#pragma warning(disable: 4541)
#endif

using namespace std;

void Test::do_test(bool cond, const std::string& lbl,
                   const char* fname, long lineno)
{
    if (!cond)
        do_fail(lbl, fname, lineno);
    else
        _succeed();
}

void Test::do_fail(const std::string& lbl,
                   const char* fname, long lineno)
{
    ++m_nFail;
    if (m_osptr)
    {
      *m_osptr << fname << ":" << lineno << ": failure in " << typeid(*this).name()
	       << "\n";
#if 0
        *m_osptr << typeid(*this).name()
                             << "failure: (" << lbl << ") , "
                                 << fname
                 << " (line " << lineno << ")\n";
#endif
    }
}

long Test::report() const
{
    if (m_osptr)
        {
            *m_osptr << "Test \"" 
                         << typeid(*this).name() << "\":\n"
                     << "\tPassed: " << m_nPass
                     << "\tFailed: " << m_nFail
                     << endl;
        }
    return m_nFail;
}

