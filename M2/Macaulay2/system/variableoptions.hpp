#pragma once
#include <string>
#include <assert.h>
#include "typedefs.hpp"
#include "../e/newdelete.hpp"
/***
	This is a class for passing options for variable/function assignment.
	New parameters can easily be added here without introducing exceedingly long function argument lists.
***/
class VariableOptions : public our_new_delete
{
public:
	/***
		This assumes global scope.
		@param name Name of the variable, not null.
	***/
	VariableOptions(M2_string name):m_Name(name),m_IsThread(false),m_IsProtected(false),m_PackageName(NULL),m_DictionaryClosure(NULL)
	{
		assert(name);
	}
	/***
		@param name Name of the variable, not null
		@param thread True for thread local variable, false for global variable.
		@param isProtected True if the variable should be protected from change, false otherwise.
	***/
	VariableOptions(M2_string name, bool isThread, bool isProtected):m_Name(name),m_IsThread(isThread),m_IsProtected(isProtected),m_PackageName(NULL),m_DictionaryClosure(NULL)
	{
		assert(name);
	}
	/***
		@param name Name of the variable, not null.
	***/
    VariableOptions(const std::string& name);
	/***
		@param name Name of the variable, not null.
		@param thread True for thread local variable, false for global variable.
		@param isProtected True if the variable should be protected from change, false otherwise.
	***/
    VariableOptions(const std::string& name, bool isThread, bool isProtected);
	/***
		Return name of the variable, not null.
	***/
	M2_string name() const { return m_Name; }
	/***
		Return name of package.
		@return M2_string, possibly NULL.
	***/
	M2_string packageName() const { return m_PackageName; }
	/***
		Dictionary closure that represents local dictionary.
		@return parse_DictionaryClosure, possibly NULL.
	***/
	parse_DictionaryClosure dictionaryClosure() const { return m_DictionaryClosure; }
	/***
		Return true if this is thread local variable, false otherwise.
	***/
	bool isThread() const { return m_IsThread; }
	/***
		Return true if this variable is protected, false otherwise.
	***/
	bool isProtected() const { return m_IsProtected; }
protected:
	/***
		Name of variable.
	***/
	M2_string m_Name;
	/***
		Name of package. May be null.
	***/
	M2_string m_PackageName;
	/***
		Dictionary closure that represents local dictionary for this assignment.
		May be NULL.  If not null, we set the variable in this scope or fail.
	***/
	parse_DictionaryClosure m_DictionaryClosure;
	/***
		True if this is thread local variable, false otherwise.
	***/
	bool m_IsThread;
	/***
		True if this variable is protected, false otherwise.
	***/
	bool m_IsProtected;
};
