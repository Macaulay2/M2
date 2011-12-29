#include "../platform/platform.h"
#include "interp.hpp"
#include <M2-exports.h>
#include <interp-exports.h>
#include <cstring>

M2CPP_Interperter M2CPP_InterperterSingleton;


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

void interp_process() { M2CPP_Interperter::gsp()->interp_process(); }
parse_Expr interp_value(parse_Expr e) { return M2CPP_Interperter::gsp()->value(e); }
parse_Expr interp_readeval(parse_TokenFile file,char returnLastvalue,char returnIfError) 
{ return M2CPP_Interperter::gsp()->readeval(file,returnLastvalue,returnIfError); }
parse_Expr interp_readeval3(parse_TokenFile file,char printout,parse_DictionaryClosure dc,char returnLastvalue,char stopIfBreakReturnContinue,char returnIfError)
{ return M2CPP_Interperter::gsp()->readeval3(file,printout,dc,returnLastvalue,stopIfBreakReturnContinue,returnIfError); }
parse_Expr interp_readeval4(parse_TokenFile file,char printout,parse_Dictionary dictionary,char returnLastvalue,char stopIfBreakReturnContinue,char returnIfError)
{ return M2CPP_Interperter::gsp()->readeval4(file,printout,dictionary,returnLastvalue,stopIfBreakReturnContinue,returnIfError); }

parse_Frame* M2CPP_Interperter::localFrame() { return reinterpret_cast<parse_Frame*>(TS_Get_Local(expr_localFrame_id)); }
char* M2CPP_Interperter::stopIfError() { return reinterpret_cast<char*>(TS_Get_Local(tokens_stopIfError_id)); }
int& M2CPP_Interperter::debugLevel() { return *reinterpret_cast<int*>(TS_Get_Local(expr_debugLevel_id)); }
errio_BasicFile* M2CPP_Interperter::M2_stderr() { return reinterpret_cast<errio_BasicFile*>(TS_Get_Local(errio_stderr_id)); }
struct atomic_field* M2CPP_Interperter::interruptedFlag() { return reinterpret_cast<struct atomic_field*>(TS_Get_Local(interrupts_interruptedFlag_id)); }
char* M2CPP_Interperter::interruptPending() { return reinterpret_cast<char*>(TS_Get_Local(interrupts_interruptPending_id)); }


/***
	Return value from program for normal exit.
***/
static const int normalExit = 0;
/***
	Return value from program for exit on error.
***/
static const int errorExit = 1;
/***
	Return value from program for exit on interupt.
***/
static const int interruptExit = 2;
/***
	Return value from program for internal error on trying to exit.
***/
static const int failedExitExit = 3;
 

parse_Expr M2CPP_Interperter::readeval4(parse_TokenFile file,char printout,parse_Dictionary dictionary,char returnLastvalue,char stopIfBreakReturnContinue,char returnIfError){
	parse_Expr lastvalue = parse_nullE;
	parse_Expr mode = actors5_topLevelMode;
	parse_Expr modeBeforePrint = basic_list_6(mode, reinterpret_cast<parse_Expr>(interp_BeforePrint));
	parse_Expr modeNoPrint = basic_list_6(mode, reinterpret_cast<parse_Expr>(interp_NoPrint));
	parse_Expr modePrint = basic_list_6(mode, reinterpret_cast<parse_Expr>(interp_Print));
	parse_Expr modeAfterNoPrint = basic_list_6(mode, reinterpret_cast<parse_Expr>(interp_AfterNoPrint));
	parse_Expr modeAfterPrint = basic_list_6(mode, reinterpret_cast<parse_Expr>(interp_AfterPrint));
	bool bumpLineNumber = true;
	bool promptWanted = false;
	bool issuePrompt = false;
	parse_Error lasterrmsg = interp_dummyError;
	while (true)
	{
		//top of loop
		if (bumpLineNumber) 
		{
			//bump line number
			if (printout) 
				actors5_setLineNumber(stdio_lineNumber + 1);
			bumpLineNumber = false;
		}
		if (promptWanted) 
		{
			//prompt wanted
			//this forces a prompt at the beginning of the next line.
			interp_previousLineNumber = (- 1);
			promptWanted = 0;
		}
		if (issuePrompt) 
		{
			//issue prompt
			interp_previousLineNumber = (- 1);
			stdio_less_less__2(stdio_stdIO, file->posFile->file->prompt());
			issuePrompt = 0;
		}
		interrupts_clearAllFlags();
		parse_Token u = parser_peektoken(file, 1);
		*interruptPending() = false;
		interrupts_determineExceptionFlag();
		if (u == lex_errorToken || (load_Field(*interruptedFlag()) != (AO_t)0))
		{
			parser_gettoken(file, 1);
			if ((load_Field((*((struct atomic_field*)TS_Get_Local(interrupts_interruptedFlag_id)))) != ((AO_t)0))) 
			{
				//token read interrupted
				tokens_clearFileError(file);
				store_Field((*((struct atomic_field*)TS_Get_Local(interrupts_interruptedFlag_id))),((AO_t)0));
				interrupts_determineExceptionFlag();
				promptWanted = 1;
				issuePrompt = 1;
			}
			else
			{
				//token read error
				promptWanted = 1;
				if (tokens_fileError(file)) 
				{
					return expr_buildErrorPacket(tokens_fileErrorMessage(file));
				}
				if (*stopIfError() || returnIfError)
				{
					return expr_buildErrorPacket(M2CPP_NewConstString("--backtrace: token read error--"));
				}
			}
		}
		else
		{
			parse_Word t = u->word;
			if (t == lex_wordEOF)
			{
				//EOF token, returning.
				if (returnLastvalue) 
					return lastvalue;
				else
					return parse_nullE;
			}
			else
			{
				if (t == lex_NewlineW)
				{
					//newline token, discarding.
					parser_gettoken(file, 1);
				}
				else
				{
					if (t == lex_wordEOC)
					{
						//end-of-cell token, discarding
						parser_gettoken(file, 1);
					}
					else
					{
						interp_previousLineNumber = stdio_lineNumber;
						promptWanted = 1;
						//ordinary token, ready to parse
						parse_ParseTree parsed = parser_parse(file, binding_SemicolonW->parse->precedence, 1);
						if (parsed == parser_errorTree)
						{
							if (load_Field(*interruptedFlag()) != (AO_t)0) 
							{
								//parsing interrupted
								tokens_clearFileError(file);
								store_Field(*interruptedFlag(),(AO_t)0);
								interrupts_determineExceptionFlag();
								promptWanted = 1;
							}
							else
							{
								//error during parsing.
								if (tokens_fileError(file)) 
								{
									return expr_buildErrorPacket(tokens_fileErrorMessage(file));
								}
								if (*stopIfError() || returnIfError)
								{
									return expr_buildErrorPacket(M2CPP_NewConstString("--backtrace: parse error--"));
								}
							}
						}
						else
						{
							///parsing successful
							bumpLineNumber = 1;
							//get the token that terminated the parsing of the expression
							//it has parsing precedence at most that of the semicolon
							//so it is end of file, end of cell, newline, semicolon, or one of the right parentheses : ) ] }
							//that explains the next error message
							parse_Token s = parser_gettoken(file, 1);
							if (!(s->word == binding_SemicolonW || s->word == lex_NewlineW || s->word == lex_wordEOC || s->word == lex_wordEOF))
							{
								M2_string msg = strings_plus_(M2CPP_NewConstString("syntax error: unmatched "), s->word->name);
								tokens_printErrorMessage_1(s, msg);
								if (*stopIfError() || returnIfError)
								{
									parse_Error tmp__33 = M2CPP_NewObject<parse_Error,struct parse_Error_struct>(Error_typecode);
									tmp__33->position = tokens_position(s);
									tmp__33->message = msg;
									tmp__33->value = parse_nullE;
									tmp__33->printed = 0;
									tmp__33->frame = expr_dummyFrame;
									return reinterpret_cast<parse_Expr>(tmp__33);
								}
							}
							else
							{
								//assigns scope to tokens, lookup symbols, returns false iff an error occured
								if (binding_localBind(parsed, dictionary)) 
								{
									parse_Code f_1 = convertr_convert(parsed);
									parse_Expr be = interp_runmethod(interp_BeforeEval, parse_nullE);
									if (be == 0) invalidNullPointer(__FILE__,__LINE__,-1);
									switch (be->type_) {;
									case Error_typecode:
										{
											parse_Error err_1 = reinterpret_cast<parse_Error>(be);
											err_1 = interp_update(err_1, M2CPP_NewConstString("before eval"), f_1);
											if (*stopIfError() || returnIfError)
											{
												return reinterpret_cast<parse_Expr>(err_1);
											}
											lasterrmsg = err_1;
											break;
										}
									default:
										checkTypeValidity(be->type_,__FILE__,__LINE__);
										break;
									};
									lastvalue = evaluate_evalexcept(f_1);
									if (reinterpret_cast<void *>(lastvalue) == reinterpret_cast<void *>(interp_endInput))
									{
										return parse_nullE;
									}
									if (lastvalue == 0) invalidNullPointer(__FILE__,__LINE__,-1);
									switch (lastvalue->type_) {;
									case Error_typecode:
										{
											parse_Error err_2 = reinterpret_cast<parse_Error>(lastvalue);
											if (err_2->message == tokens_returnMessage || err_2->message == tokens_continueMessage || err_2->message == tokens_continueMessageWithArg || err_2->message == tokens_stepMessage || err_2->message == tokens_stepMessageWithArg || err_2->message == tokens_breakMessage || err_2->message == tokens_throwMessage) 
											{
												if (stopIfBreakReturnContinue) 
													return lastvalue;
											}
											if (err_2->message == tokens_unwindMessage) 
												lastvalue = parse_nullE;
											else
											{
												if (!(err_2->printed)) 
													tokens_printErrorMessage(err_2);
												if (*stopIfError() || returnIfError)
													return lastvalue;
											}
											lasterrmsg = err_2;
											break;
										}
									default:
										checkTypeValidity(lastvalue->type_,__FILE__,__LINE__);
										if (printout) 
										{
											if (mode != actors5_topLevelMode)
											{
												mode = actors5_topLevelMode;
												modeBeforePrint = basic_list_6(mode, reinterpret_cast<parse_Expr>(interp_BeforePrint));
												modeNoPrint = basic_list_6(mode, reinterpret_cast<parse_Expr>(interp_NoPrint));
												modePrint = basic_list_6(mode, reinterpret_cast<parse_Expr>(interp_Print));
												modeAfterNoPrint = basic_list_6(mode, reinterpret_cast<parse_Expr>(interp_AfterNoPrint));
												modeAfterPrint = basic_list_6(mode, reinterpret_cast<parse_Expr>(interp_AfterPrint));
											}
											//result of after eval replaces lastvalue unless error, in which case null replaces it.
											parse_Expr g_1 = interp_runmethod(interp_AfterEval, lastvalue);
											if (g_1 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
											switch (g_1->type_) {;
											case Error_typecode:
												{
													parse_Error err_3 = reinterpret_cast<parse_Error>(g_1);
													err_3 = interp_update(err_3, M2CPP_NewConstString("after eval"), f_1);
													if (*stopIfError() || returnIfError)
														return reinterpret_cast<parse_Expr>(err_3);
													lastvalue = parse_nullE;
													lasterrmsg = err_3;
													break;
												}
											default:
												checkTypeValidity(g_1->type_,__FILE__,__LINE__);
												lastvalue = g_1;
												break;
											};
											parse_Expr printvalue = parse_nullE;
											g_1 = interp_runmethod_1(modeBeforePrint, lastvalue);
											//result of before print is printed unless error.
											if (g_1 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
											switch (g_1->type_) {
											case Error_typecode:
												{
													parse_Error err_4 = reinterpret_cast<parse_Error>(g_1);
													err_4 = interp_update(err_4, M2CPP_NewConstString("before print"), f_1);
													if (*stopIfError() || returnIfError)
														return reinterpret_cast<parse_Expr>(err_4);
													lasterrmsg = err_4;
													break;
												}
											default:
												checkTypeValidity(g_1->type_,__FILE__,__LINE__);
												printvalue = g_1;
												break;
											};
											parse_Expr tmp__54;
											if (s->word == binding_SemicolonW)
											{
												tmp__54 = modeNoPrint;
											}
											else
											{
												tmp__54 = modePrint;
											}
											//result of print is ignored
											g_1 = interp_runmethod_1(tmp__54, printvalue);
											if (g_1 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
											switch (g_1->type_) {
											case Error_typecode:
												{
													parse_Error err_5 = reinterpret_cast<parse_Error>(g_1);
													err_5 = interp_update(err_5, M2CPP_NewConstString("at print"), f_1);
													if (*stopIfError() || returnIfError)
														return reinterpret_cast<parse_Expr>(err_5);
													lasterrmsg = err_5;
													break;
												}
											default:
												checkTypeValidity(g_1->type_,__FILE__,__LINE__);
												break;
											};
											parse_Expr tmp__58;
											if (s->word == binding_SemicolonW)
											{
												tmp__58 = modeAfterNoPrint;
											}
											else
											{
												tmp__58 = modeAfterPrint;
											}
											//result of after print is ignored.
											g_1 = interp_runmethod_1(tmp__58, lastvalue);
											if (g_1 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
											switch (g_1->type_) {;
											case Error_typecode:
												{
													parse_Error err_6 = reinterpret_cast<parse_Error>(g_1);
													err_6 = interp_update(err_6, M2CPP_NewConstString("after print"), f_1);
													if (*stopIfError() || returnIfError)
														return reinterpret_cast<parse_Expr>(err_6);
													lasterrmsg = err_6;
													break;
												}
											default:
												checkTypeValidity(g_1->type_,__FILE__,__LINE__);
												break;
											};
										}
										break;
									};
								}
								else
								{
									// an error occured in local bind.
									if (tokens_isatty(file)) 
										tokens_flush(file);
									else
									{
										if (lasterrmsg != interp_dummyError)
										{
											return reinterpret_cast<parse_Expr>(lasterrmsg);
										}
										else
										{
											return expr_buildErrorPacket(M2CPP_NewConstString("error occurred in parsing"));
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
}


parse_Expr M2CPP_Interperter::readeval3(parse_TokenFile file,char printout,parse_DictionaryClosure dc,char returnLastvalue,char stopIfBreakReturnContinue,char returnIfError)
{
	parse_Frame saveLocalFrame = *reinterpret_cast<parse_Frame*>(TS_Get_Local(expr_localFrame_id));
	(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = dc->frame;
	parse_Expr savecf = expr_getGlobalVariable(interp_currentFileName);
	parse_Expr savecd = expr_getGlobalVariable(interp_currentFileDirectory);
	parse_PosFile savepf = interp_currentPosFile;
	expr_setGlobalVariable(interp_currentFileName, util_toExpr_4(file->posFile->file->filename));
	expr_setGlobalVariable(interp_currentFileDirectory, util_toExpr_4(interp_dirname(file->posFile->file->filename)));
	interp_currentPosFile = file->posFile;
	parse_Expr ret = interp_readeval4(file, printout, dc->dictionary, returnLastvalue, stopIfBreakReturnContinue, returnIfError);
	expr_setGlobalVariable(interp_currentFileDirectory, savecd);
	expr_setGlobalVariable(interp_currentFileName, savecf);
	interp_currentPosFile = savepf;
	(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame;
	return ret;
}

parse_Expr M2CPP_Interperter::readeval(parse_TokenFile file,char returnLastvalue,char returnIfError)
{
	parse_Expr savefe = expr_getGlobalVariable(interp_fileExitHooks);
	expr_setGlobalVariable(interp_fileExitHooks, basic_emptyList);
	bool printout = 0;
	parse_Expr ret = readeval3(file, printout, actors5_newStaticLocalDictionaryClosure(file->posFile->file->filename), returnLastvalue, 0, returnIfError);
	if (ret == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	parse_Expr haderror;
	switch (ret->type_) {
	case Error_typecode:
		haderror = parse_True;
		break;
	default:
		checkTypeValidity(ret->type_,__FILE__,__LINE__);
		haderror = parse_False;
		break;
	};
	parse_Error lastexiterror = interp_dummyError;
	parse_Expr hook = expr_getGlobalVariable(interp_fileExitHooks);
	if (hook == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (hook->type_) {
	case List_typecode:
		{
			parse_List x = reinterpret_cast<parse_List>(hook);
			parse_Sequence sequence = x->v;
			for(int i = 0; i < sequence->len; ++i)
			{
				parse_Expr r = evaluate_applyEE(sequence->array[i], haderror);
				if (r == 0) invalidNullPointer(__FILE__,__LINE__,-1);
				switch (r->type_) {
				case Error_typecode:
					{
						parse_Error err = reinterpret_cast<parse_Error>(r);
						if ((lastexiterror != interp_dummyError) && !(lastexiterror->printed)) 
						{
							tokens_printErrorMessage(lastexiterror);
						}
						lastexiterror = err;
					}
					break;
				default:
					checkTypeValidity(r->type_,__FILE__,__LINE__);
					break;
				}
			}
			break;
		}
	default:
		checkTypeValidity(hook->type_,__FILE__,__LINE__);
		break;
	};
	expr_setGlobalVariable(interp_fileExitHooks, savefe);
	if (lastexiterror != interp_dummyError) 
	{
		return reinterpret_cast<parse_Expr>(lastexiterror);
	}
	else
	{
		return ret;
	}
}


parse_TokenFile M2CPP_Interperter::stringTokenFile(M2_string name,M2_string contents)
{
	parse_TokenFile ret = M2CPP_NewObject<parse_TokenFile,struct parse_TokenFile_struct>(TokenFile_typecode);
	ret->posFile = stdiop_makePosFile(stdio_newFile(name, 0, 0, M2CPP_NewConstString(""), 0, (- 1), (- 1), 0, 1, (- 1), 0, contents, 0, contents->len, 1, 0, stdio_noprompt, stdio_noprompt, 0, 1, 0, 0, 0, (- 1), 0, M2CPP_NewConstString(""), 0, 0, 0, nets_dummyNetList, 0, (- 1), 0, 0));
	ret->nexttoken = NULL;
	return ret;
}

parse_Expr M2CPP_Interperter::value(parse_Expr e)
{
	if (e == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (e->type_) {
	case SymbolClosure_typecode:
		{
			parse_SymbolClosure q = reinterpret_cast<parse_SymbolClosure>(e);
			parse_Sequence vals = q->frame->values;
			int i = q->symbol->frameindex;
			if (i < vals->len)
			{
				if (i < 0 || i >= vals->len) fatalarrayindex(i,vals->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/interp.dd",506,40);
				return vals->array[i];
			}
			else
			{
				return parse_nullE;
			}
			assert(0);
			break;
		}
	case CodeClosure_typecode:
		{
			parse_CodeClosure c = reinterpret_cast<parse_CodeClosure>(e);
			return evaluate_eval_1(c->frame, c->code);
		}
	case stringCell_typecode:
		{
			M2_stringCell s = ((M2_stringCell)e);
			parse_Expr savecs = expr_getGlobalVariable(interp_currentString);
			expr_setGlobalVariable(interp_currentString, e);
			parse_Expr r = interp_readeval(stringTokenFile(M2CPP_NewConstString("interp_currentString"), strings_plus_(s->v, M2_newline)), 1, 1);
			expr_setGlobalVariable(interp_currentString, savecs);
			if (r == 0) invalidNullPointer(__FILE__,__LINE__,-1);
			switch (r->type_) {;
			case Error_typecode:
				{
					parse_Error err = reinterpret_cast<parse_Error>(r);
					if(err->message == tokens_returnMessage || err->message == tokens_continueMessage || err->message == tokens_continueMessageWithArg || err->message == tokens_stepMessage || err->message == tokens_stepMessageWithArg || err->message == tokens_breakMessage)
					{
						if (err->value == parse_dummyExpr)
						{
							return parse_nullE;
						}
						else
						{
							return err->value;
						}
					}
					else
					{
						return r;
					}
					break;
				}
			default:
				checkTypeValidity(r->type_,__FILE__,__LINE__);
				return r;
				break;
			};
			break;
		}
	default:
		checkTypeValidity(e->type_,__FILE__,__LINE__);
		return expr_WrongArg_1(1, M2CPP_NewConstString("a string, a symbol, or pseudocode"));
	};
}


void M2CPP_Interperter::exit(parse_Error err){
	int ret;
	if (strings_equal_equal_equal_(err->message, tokens_interruptMessage)) 
	{
		ret = interruptExit;
	}
	else
	{
		ret = errorExit;
	}
	system_exit(ret);
}

void M2CPP_Interperter::interp_process()
{
	*localFrame() = expr_globalFrame;
	//legacy from dump data.
	interp_previousLineNumber = (- 1);
	stdio_stdIO->inisatty = (0 != isatty(0));
	stdio_stdIO->echo = !(0 != isatty(0));
	stdio_stdIO->outisatty = (0 != isatty(1));
	stdio_stdError->outisatty = (0 != isatty(2));
	//legacy from load data.
	actors5_setstopIfError(0);
	actors5_sethandleInterrupts(1);
	system_everytimeRun();
	parse_Expr ret = interp_readeval(stringTokenFile(actors5_startupFile, actors5_startupString), 0, 0);
	if (ret == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (ret->type_) {
	case Error_typecode:
		{
			//just in case.
			parse_Error err = reinterpret_cast<parse_Error>(ret);
			if (!err->printed)
			{
				tokens_printError(err);
			}
			if (*stopIfError())
			{
				//unlikely to happen.
				exit(err);
			}
			else
			 {
				 //give a prompt for debugging.
				 if (!(interp_topLevel())) 
				 {
					 exit(err);
				 }
			 }
			break;
		}
	case ZZcell_typecode:
		{
			gmp_ZZcell n_1 = reinterpret_cast<gmp_ZZcell>(ret);
			if (gmp_isInt_1(n_1)) 
			{
				//try to exit the user's way.
				value(util_toExpr_4(strings_plus_(M2CPP_NewConstString("exit "), strings1_tostring_2(gmp_toInt_1(n_1)))));
			}
			break;
		}
	default:
		checkTypeValidity(ret->type_,__FILE__,__LINE__);
		break;
	};
	//try to exit the user's way.
	value(util_toExpr_4(M2CPP_NewConstString("exit 0")));
	//if that doesn't work hard fail.
	system_exit(failedExitExit);
}
