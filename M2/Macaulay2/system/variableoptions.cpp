#include "../platform/platform.h"
#include "variableoptions.hpp"
#include "interp.hpp"
VariableOptions::VariableOptions(const std::string& name, bool isThread, bool isProtected):m_IsThread(isThread),m_IsProtected(isProtected),m_PackageName(NULL),m_DictionaryClosure(NULL)
{
	m_Name = M2CPP_NewString(name);
}
