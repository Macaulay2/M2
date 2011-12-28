#include "../platform/platform.h"
#include "interp.hpp"
#include <M2-exports.h>
#include <cstring>

M2_string M2CPP_NewString(std::string str)
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
