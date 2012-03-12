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
	static M2CPP_InterperterLocal* glp() { return t_InterperterLocal; }
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
/***
	Create a new M2 sequence with the given length.
	@param length Length of the sequence.
	@return Sequence, not null.
***/
extern parse_Sequence M2CPP_NewSequence(int length);
/***
	Create a new M2 sequence with the given length initialized to value.
	@param length Length of the sequence.
	@return Sequence, not null.
***/
extern parse_Sequence M2CPP_NewSequence(int length, parse_Expr value);
/***
	Resize a new M2 sequence by possibly creating a new one.
	Truncate or set to null any remaining values.
	@param s Sequence
	@param len Length of new sequence.
	@return New or existing sequence, not null.
***/
extern parse_Sequence M2CPP_ResizeSequence(parse_Sequence s, int length);
/***
	Enlarge the sequence by doubling it.
	@param s Sequence.
	@return New sequence, not null of length = length(s)*2.
***/
extern parse_Sequence M2CPP_EnlargeSequence(parse_Sequence s);
/***
	Create a new list from the sequence, resizing as needed.
	Truncate or set to null any remaining values.
	@param s Sequence, not null.
	@param len Length of list.
	@return New list, not null.
***/
extern parse_Expr M2CPP_NewList(parse_Sequence s, int length);
/***
	Perform safety checks in debug mode.
	@param e Expr, not null.
***/
inline void M2CPP_PerformAssertions(parse_Expr e)
{
	assert(e!=NULL);
	assert(e->type_ < numTypeCodes);
}
/***
	Return true if the expr is an error, false otherwise.
	@param e Expr, not null.
***/
inline bool M2CPP_IsError(parse_Expr e)
{
	M2CPP_PerformAssertions(e);
	return e->type_ == Error_typecode;
}
/***
	Return true if e has given typecode, false otherwise.
	@param e Expr, not null.
***/
inline bool M2CPP_IsTypeExact(parse_Expr e, int typecode)
{
	M2CPP_PerformAssertions(e);
	return e->type_==typecode;
}
/***
	Return true if e has given typecode, false otherwise.
	@param c Code, not null.
***/
inline bool M2CPP_IsTypeExact(parse_Code e, int typecode)
{
	return e->type_==typecode;
}
/***
	Use this function when an erroneous condition has been detected.
	This would frequently be used after a !M2CPP_IsTypeExact statement.
	@param e Expr, not null.
	@param errorMessage error message, not null.
	@return e if error, else common_printErrorMessageE(errorMessage)
***/
extern parse_Expr M2CPP_ErrorOrErrorMessage(parse_Expr e, M2_string errorMessage);
/***
	Use this function when an erroneous condition has been detected.
	This would frequently be used after a !M2CPP_IsTypeExact statement.
	@param e Expr, not null.
	@param errorMessage Error message, not null.
	@return e if error, else common_printErrorMessageE(errorMessage)
***/
extern parse_Expr M2CPP_ErrorOrErrorMessage(parse_Expr e, const char* errorMessage);
/***
	Equivalent to common_printErrorMessageE(i,M2CPP_NewConstString(errorMessage))
	@param i Expr that caused error.
	@param errorMessage Error mesage, not null.
	@return Expr, not null.
***/
extern parse_Expr M2CPP_PrintErrorMessageE(parse_Expr i, const char* errorMessage);
/***
	Return typecode of expr.
	@param e Expr, not null.
***/
inline int M2CPP_Type(parse_Expr e)
{
	return e->type_;
}
/***
	Create a new ZZCell with value of x.
	@return ZZCell, not null.
***/
extern parse_Expr M2CPP_ZZCell(int x);
/***
	Create a new frame
***/
extern parse_Frame M2CPP_NewFrame(parse_Frame outerFrame, int frameId, int frameSize, bool notRecycleable);
#endif
#endif
