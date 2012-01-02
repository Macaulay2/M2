#include "../platform/platform.h"
#include "interp.hpp"
#include "interplocal.hpp"
#include <M2-exports.h>
#include <interp-exports.h>
#include <cstring>
#include <sstream>
#include <iostream>

M2CPP_Interperter M2CPP_InterperterSingleton;
__thread M2CPP_InterperterLocal* t_InterperterLocal = NULL;

M2_string M2CPP_NewString(const std::string& str)
{
	return M2CPP_NewString(str.c_str());
}

M2_string M2CPP_NewConstString(const char* string)
{
	return M2CPP_NewString(string);
}

M2_string M2CPP_NewString(const char* string)
{
	size_t slen = strlen(string);
	M2_string tmp = (M2_string) GC_MALLOC_ATOMIC(sizeof(struct M2_string_struct) + (slen - 1)*sizeof(char));
	tmp->len = static_cast<int>(slen);
	memcpy(tmp->array, string, slen);
	return tmp;
} 

void M2CPP_Interperter::initializeLocalState()
{
	if(NULL == t_InterperterLocal)
	{
		t_InterperterLocal = new M2CPP_InterperterLocal();
	}
}
