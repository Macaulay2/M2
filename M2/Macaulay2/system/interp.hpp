#ifndef _INTERP_H_
#define _INTERP_H_
#if defined(__cplusplus)
#include <string>
#include "supervisorinterface.h"
#include "typedefs.hpp"

class M2CPP_Interperter;
/***
	Singleton for M2CPP Interperter.
	This really should not be a singleton, but while we transition code it will be.
***/
extern M2CPP_Interperter M2CPP_InterperterSingleton;
extern __thread M2CPP_InterperterLocal* t_InterperterLocal;
/***
	The M2CPP_Interperter contains the global state for a Macaulay2 interperter.
	The state local to a given thread is stored in the M2CPP_InterperterLocal class.
	It is necessary to set up the local state when first executing in a new thread by calling initalizeLocalState().
	The M2CPP_Interperter is currently set up as a singleton, but there is no reason that multiple ones could not eventually exist.
	However M2CPP_InterperterLocal is designed around the assumption that there is exactly one M2CPP_InterperterLocal per thread.

	Binding is the process of converting from the lexed trees and assigning frames & frame indicies to each variable/operator.
	Binding is accomplished in M2CPP_InterperterLocal.  
	It is considered a local process because multiple threads may be binding at once (if they are parsing code at the same time).  
	Binding is both reinterant and thread safe, but binding non-global variables will not change non-thread-local data.
	Once code is ready to be executed it does not need to be rebound.
	
	Frames should be thought of as the equivalent of stack frames except M2 has them for scopes.  
	There are global frames, thread local frames and dynamic frames.  Each frame consists of a sequence of pointers.  
	These pointers are indexed by the frame index that is associated with a variable.
	Enlarging frames is not currently thread safe.
	Both the enlarge function and thread frame size have race conditions present that can cause data corruption.
***/
class M2CPP_Interperter
{
public:
	/***
		Get the current M2CPP Interperter in some well defined way.
		Currently returns the global interperter.
		Eventually will be instance local.
	***/
	static M2CPP_Interperter* gsp() { return &M2CPP_InterperterSingleton; }
	/***
		Initialize any local state for the interperter.
	***/
	void initializeLocalState();
	/***
		Get the current local state for this interperter.
	***/
	M2CPP_InterperterLocal* glp() { return t_InterperterLocal; }
};
/***
   Create a new M2 object of type T and return it as type R
***/
template<class R, class T> inline R M2CPP_NewObject()
{
	R r = reinterpret_cast<R>(GC_MALLOC(sizeof(T)));
	return r;
}
/***
   Create a new M2 object of type T and return it as type R
***/
template<class R, class T> inline R M2CPP_NewObject(int typecode)
{
	R r = reinterpret_cast<R>(GC_MALLOC(sizeof(T)));
	r->type_ = typecode;
	return r;
}
/***
   Create a new M2 object of type T with array of type I and length arrayLength and return it as type R
***/
template<class R, class T, class I> inline R M2CPP_NewArray(size_t arrayLength)
{
	R r = reinterpret_cast<R>(GC_MALLOC(sizeof(T)+(arrayLength-1)*sizeof(I)));
	r->len = arrayLength;
	return r;
}
/***
   Create a new M2 string with the given name.
   @param string Null terminated string.  This does not perform bounds checking.
***/
extern M2_string M2CPP_NewString(const char* string);
/***
   Create a new constant M2 string with the given name.
   This is allowed to pool strings or perform other optimizations assuming constantness.
   @param string Null terminated string.  This does not perform bounds checking.
***/
extern M2_string M2CPP_NewConstString(const char* string);
/***
   Create a new M2 string with the given name.
   @param string STL string.
***/
extern M2_string M2CPP_NewString(const std::string& str);
#endif

#endif
