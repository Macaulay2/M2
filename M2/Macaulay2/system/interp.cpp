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
parse_Expr M2CPP_NewList(parse_Sequence s, int length)
{
	if(length == 0)
		return basic_list(expr_emptySequence);
	else if(length==s->len)
		return basic_list(r);
	parse_Sequence rs = M2CPP_ResizeSequence(s,length);
	return basic_list(rs);
}
parse_Expr M2CPP_ErrorOrErrorMesage(parse_Expr e, M2_string errorMessage)
{
	if(M2CPP_IsError(e))
		return e;
	else
		return common_printErrorMessageE(i,errorMessage);
}
parse_Expr M2CPP_ErrorOrErrorMesage(parse_Expr e, const char* errorMessage)
{
	if(M2CPP_IsError(e))
		return e;
	else
		return M2CPP_PrintErrorMessageE(e,errorMessage);
}
parse_Expr M2CPP_PrintErrorMessageE(parse_Expr i, const char* errorMessage)
{
	return common_printErrorMessageE(i, M2CPP_NewString(errorMessage));
}
parse_Expr M2CPP_ZZCell(int x)
{
	gmp_ZZcell zzc = M2CPP_NewObject<gmp_ZZcell,struct gmp_ZZcell_struct>(ZZcell_typecode);
	zzc->v = gmp_toInteger(x);
	return reinterpret_cast<parse_Expr>(zzc);
}
parse_Frame M2CPP_NewFrame(parse_Frame outerFrame, int frameId, int frameSize, bool notRecycleable)
{
	parse_Frame frame = M2CPP_NewObject<parse_Frame, struct parse_Frame_struct>();
	frame->outerFrame = outerFrame;
	frame->frameID = frameId;
	frame->valuesUsed = framesize;
	frame->notrecyclable = notRecycleable;
	frame->values = M2CPP_NewSequence(frameSize,parse_nullE);
	return frame;
}
void M2CPP_Interperter::initializeLocalState()
{
	if(NULL == t_InterperterLocal)
	{
		t_InterperterLocal = new M2CPP_InterperterLocal();
	}
}
