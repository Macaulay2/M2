#include "../platform/platform.h"
#include "interplocal.hpp"
#include "interp.hpp"
#include <M2-exports.h>
#include <interp-exports.h>
#include <cstring>
#include <sstream>
#include <iostream>

extern "C" {
	void interp_process()
	{ M2CPP_Interperter::gsp()->glp()->interp_process(); }
	parse_Expr interp_value(parse_Expr e)
	{ return M2CPP_Interperter::gsp()->glp()->value(e); }
	parse_Expr interp_readeval(parse_TokenFile file,char returnLastvalue,char returnIfError) 
	{ return M2CPP_Interperter::gsp()->glp()->readeval(file,returnLastvalue,returnIfError); }
	parse_Expr interp_readeval3(parse_TokenFile file,char printout,parse_DictionaryClosure dc,char returnLastvalue,char stopIfBreakReturnContinue,char returnIfError)
	{ return M2CPP_Interperter::gsp()->glp()->readeval3(file,printout,dc,returnLastvalue,stopIfBreakReturnContinue,returnIfError); }
	parse_Symbol binding_insert(parse_Symbol entry,parse_SymbolHashTable table)
	{ return M2CPP_Interperter::gsp()->glp()->binding_insert(entry,table); }
	parse_Symbol binding_insert_1(parse_SymbolHashTable table,parse_Word newname,parse_Symbol entry)
	{ return M2CPP_Interperter::gsp()->glp()->binding_insert_1(table,newname,entry); }
	parse_Symbol binding_makeEntry(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary,char thread,char locallyCreated)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeEntry(word,position,dictionary,thread,locallyCreated); }
	parse_Symbol binding_makeEntry_1(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeEntry_1(word,position,dictionary); }
	parse_Symbol binding_makeSymbol(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary,char thread,char locallyCreated)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeSymbol(word,position,dictionary,thread,locallyCreated); }
	parse_Symbol binding_makeSymbol_1(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary,char thread)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeSymbol_1(word,position,dictionary,thread); }
	parse_Symbol binding_makeSymbol_2(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeSymbol_2(word,position,dictionary); }
	parse_SymbolClosure binding_makeProtectedSymbolClosure(parse_Word w)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeProtectedSymbolClosure(w); }
	parse_SymbolClosure binding_makeKeyword(parse_Word w)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeKeyword(w); }
	parse_SymbolClosure binding_makeProtectedSymbolClosure_1(M2_string s)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeProtectedSymbolClosure_1(s); }
	parse_SymbolClosure binding_makeKeyword_1(M2_string s)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeKeyword_1(s); }
	parse_Symbol binding_makeSymbol_3(parse_Token t)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeSymbol_3(t); }
	void binding_makeErrorTree(parse_ParseTree e,M2_string message)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeErrorTree(e,message); }
	void binding_makeErrorTree_1(parse_Token e,M2_string message)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeErrorTree_1(e,message); }
	void binding_makeSymbol_4(parse_ParseTree e,parse_Dictionary dictionary)
	{ return M2CPP_Interperter::gsp()->glp()->binding_makeSymbol(e,dictionary); }
	parse_Symbol binding_lookup(parse_Word word,parse_SymbolHashTable table)
	{ return M2CPP_Interperter::gsp()->glp()->binding_lookup(word,table); }
	parse_Symbol binding_globalLookup(parse_Word w)
	{ return M2CPP_Interperter::gsp()->glp()->binding_globalLookup(w); }
	parse_Symbol binding_lookup_1(parse_Word w,parse_Dictionary d)
	{ return M2CPP_Interperter::gsp()->glp()->binding_lookup_1(w,d); }
	void binding_bind(parse_ParseTree e,parse_Dictionary dictionary)
	{ return M2CPP_Interperter::gsp()->glp()->binding_bind(e,dictionary); }
	char binding_localBind(parse_ParseTree e,parse_Dictionary dictionary)
	{ return M2CPP_Interperter::gsp()->glp()->binding_localBind(e,dictionary); }
	parse_Symbol common_setupvar(M2_string name,parse_Expr value,char thread)
	{
		M2CPP_Interperter::gsp()->initializeLocalState();
		return M2CPP_Interperter::gsp()->glp()->setupVariable(name,value,thread!=0); 
	}
}

parse_Frame* M2CPP_InterperterLocal::localFrame() { return reinterpret_cast<parse_Frame*>(TS_Get_Local(expr_localFrame_id)); }
bool& M2CPP_InterperterLocal::stopIfError() { return *reinterpret_cast<bool*>(TS_Get_Local(tokens_stopIfError_id)); }
int& M2CPP_InterperterLocal::debugLevel() { return *reinterpret_cast<int*>(TS_Get_Local(expr_debugLevel_id)); }
errio_BasicFile* M2CPP_InterperterLocal::M2_stderr() { return reinterpret_cast<errio_BasicFile*>(TS_Get_Local(errio_stderr_id)); }
struct atomic_field* M2CPP_InterperterLocal::interruptedFlag() { return reinterpret_cast<struct atomic_field*>(TS_Get_Local(interrupts_interruptedFlag_id)); }
bool& M2CPP_InterperterLocal::interruptPending() { return *reinterpret_cast<bool*>(TS_Get_Local(interrupts_interruptPending_id)); }

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

M2CPP_InterperterLocal::M2CPP_InterperterLocal()
{
	m_BindingLookupCountIncrement = 1;
	m_BindingHadError = false;
}

parse_Expr M2CPP_InterperterLocal::readeval4(parse_TokenFile file,bool printout,parse_Dictionary dictionary,bool returnLastvalue,bool stopIfBreakReturnContinue,bool returnIfError){
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
		interruptPending() = false;
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
				if (stopIfError() || returnIfError)
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
								if (stopIfError() || returnIfError)
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
								if (stopIfError() || returnIfError)
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
											if (stopIfError() || returnIfError)
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
												if (stopIfError() || returnIfError)
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
													if (stopIfError() || returnIfError)
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
													if (stopIfError() || returnIfError)
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
													if (stopIfError() || returnIfError)
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
													if (stopIfError() || returnIfError)
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


parse_Expr M2CPP_InterperterLocal::readeval3(parse_TokenFile file,char printout,parse_DictionaryClosure dc,char returnLastvalue,char stopIfBreakReturnContinue,char returnIfError)
{
	parse_Frame saveLocalFrame = *reinterpret_cast<parse_Frame*>(TS_Get_Local(expr_localFrame_id));
	(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = dc->frame;
	parse_Expr savecf = expr_getGlobalVariable(interp_currentFileName);
	parse_Expr savecd = expr_getGlobalVariable(interp_currentFileDirectory);
	parse_PosFile savepf = interp_currentPosFile;
	expr_setGlobalVariable(interp_currentFileName, util_toExpr_4(file->posFile->file->filename));
	expr_setGlobalVariable(interp_currentFileDirectory, util_toExpr_4(interp_dirname(file->posFile->file->filename)));
	interp_currentPosFile = file->posFile;
	parse_Expr ret = readeval4(file, printout, dc->dictionary, returnLastvalue, stopIfBreakReturnContinue, returnIfError);
	expr_setGlobalVariable(interp_currentFileDirectory, savecd);
	expr_setGlobalVariable(interp_currentFileName, savecf);
	interp_currentPosFile = savepf;
	(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame;
	return ret;
}

parse_Expr M2CPP_InterperterLocal::readeval(parse_TokenFile file,char returnLastvalue,char returnIfError)
{
	parse_Expr savefe = expr_getGlobalVariable(interp_fileExitHooks);
	expr_setGlobalVariable(interp_fileExitHooks, basic_emptyList);
	bool printout = 0;
	parse_DictionaryClosure dict = actors5_newStaticLocalDictionaryClosure(file->posFile->file->filename);
	parse_Expr ret;
	ret = readeval3(file, printout, dict, returnLastvalue, 0, returnIfError);
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
							tokens_printErrorMessage(lastexiterror);
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


parse_TokenFile M2CPP_InterperterLocal::stringTokenFile(M2_string name,M2_string contents)
{
	parse_TokenFile ret = M2CPP_NewObject<parse_TokenFile,struct parse_TokenFile_struct>(TokenFile_typecode);
	ret->posFile = stdiop_makePosFile(stdio_newFile(name, 0, 0, M2CPP_NewConstString(""), 0, (- 1), (- 1), 0, 1, (- 1), 0, contents, 0, contents->len, 1, 0, stdio_noprompt, stdio_noprompt, 0, 1, 0, 0, 0, (- 1), 0, M2CPP_NewConstString(""), 0, 0, 0, nets_dummyNetList, 0, (- 1), 0, 0));
	ret->nexttoken = NULL;
	return ret;
}

parse_Expr M2CPP_InterperterLocal::value(parse_Expr e)
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
				if (i < 0 || i >= vals->len) fatalarrayindex(i,vals->len,__FILE__,__LINE__,40);
				return vals->array[i];
			}
			else
				return parse_nullE;
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
							return parse_nullE;
						else
							return err->value;
					}
					else
						return r;
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


void M2CPP_InterperterLocal::exit(parse_Error err)
{
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
extern "C" {
extern const char* startupString;
}
void M2CPP_InterperterLocal::interp_process()
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
	parse_Expr ret;
	ret = interp_readeval(stringTokenFile(actors5_startupFile, actors5_startupString), 0, 0);
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
			if (stopIfError())
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


void M2CPP_InterperterLocal::binding_append(struct SCC_M2_0_int_len_parse_SymbolList_array1 * buckets,parse_Word word,parse_Symbol entry)
{
	int h = (word->hash & (buckets->len - 1));
	parse_SymbolList tmp__1 = buckets->array[h];
	if (NULL == tmp__1)
	{
		parse_SymbolListCell tmp__3 = M2CPP_NewObject<parse_SymbolListCell,struct parse_SymbolListCell_struct>();
		tmp__3->word = word;
		tmp__3->entry = entry;
		tmp__3->next = NULL;
		buckets->array[h] = reinterpret_cast<parse_SymbolList>(tmp__3);
	}
	else
	{
		parse_SymbolListCell e = reinterpret_cast<parse_SymbolListCell>(tmp__1);
		while(1)
		{
			if (NULL == e->next)
			{
				parse_SymbolListCell tmp__5 = M2CPP_NewObject<parse_SymbolListCell,struct parse_SymbolListCell_struct>();
				tmp__5->word = word;
				tmp__5->entry = entry;
				tmp__5->next = NULL;
				e->next = reinterpret_cast<parse_SymbolList>(tmp__5);
				return;
			}
			else
				e = e->next;
		}
	}
}

void M2CPP_InterperterLocal::binding_enlarge(parse_SymbolHashTable table){
	int newlen = (2 * table->buckets->len);
	if (0 > newlen) fatalarraylen(newlen,__FILE__,__LINE__,46);
	struct SCC_M2_0_int_len_parse_SymbolList_array1 * newbuckets = M2CPP_NewArray<struct SCC_M2_0_int_len_parse_SymbolList_array1 *,struct SCC_M2_0_int_len_parse_SymbolList_array1,parse_SymbolList>(newlen);
	for(size_t i = 0; i < newlen; ++i)
	{
		newbuckets->array[i]=NULL;
	}
	for(size_t i = 0; i < table->buckets->len; ++i)
	{
		parse_SymbolList entryList = table->buckets->array[i];
		while(1)
		{
			if (NULL == entryList)
				break;
			parse_SymbolListCell entryListCell = reinterpret_cast<parse_SymbolListCell>(entryList);
			binding_append(newbuckets, entryListCell->word, entryListCell->entry);
			entryList = entryListCell->next;
		}
	}
	table->buckets = newbuckets;
}

parse_Symbol M2CPP_InterperterLocal::binding_insert(parse_Symbol entry,parse_SymbolHashTable table)
{
	acquireSpinLock(&(table->mutex));
	table->numEntries = (table->numEntries + 1);
	if ((3 * table->numEntries) > ((2 * table->buckets->len) + 1))
	{
		binding_enlarge(table);
	}
	int h_1 = entry->word->hash & (table->buckets->len - 1); //hash code
	parse_SymbolListCell tmp__13 = M2CPP_NewObject<parse_SymbolListCell,struct parse_SymbolListCell_struct>();
	tmp__13->word = entry->word;
	tmp__13->entry = entry;
	tmp__13->next = table->buckets->array[h_1];
	table->buckets->array[h_1] = reinterpret_cast<parse_SymbolList>(tmp__13);
	AO_compiler_barrier();
	releaseSpinLock(&(table->mutex));
	return entry;
}

parse_Symbol M2CPP_InterperterLocal::binding_insert_1(parse_SymbolHashTable table,parse_Word newname,parse_Symbol entry)
{
	acquireSpinLock(&(table->mutex));
	table->numEntries = (table->numEntries + 1);
	if ((3 * table->numEntries) > ((2 * table->buckets->len) + 1))
	{
		binding_enlarge(table);
	}
	int h_2 = (newname->hash & (table->buckets->len - 1)); //hash code
	parse_SymbolListCell tmp__16 = M2CPP_NewObject<parse_SymbolListCell,struct parse_SymbolListCell_struct>();
	tmp__16->word = newname;
	tmp__16->entry = entry;
	tmp__16->next = table->buckets->array[h_2];
	table->buckets->array[h_2] = reinterpret_cast<parse_SymbolList>(tmp__16);
	AO_compiler_barrier();
	releaseSpinLock(&(table->mutex));
	return entry;
}

parse_Symbol M2CPP_InterperterLocal::binding_makeEntry(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary,char thread,char locallyCreated)
{
	while(dictionary->Protected)
	{
		if (dictionary == dictionary->outerDictionary)
		{
			//This shouldn't occur from M2 since dictionaries in "actors5" and "project" in actors2 enforce this.
			//However, with a C API this could happen if we are less then careful with error checking.
			err_error(M2CPP_NewConstString("internal error: global dictionaries all protected"));
			assert(0);
			return expr_dummySymbol;
		}
		dictionary = dictionary->outerDictionary;
	}
	int frameindex = 0;
	if (dictionary->frameID == 0)
	{
		if (thread) 
		{
			//threadFrame grows whenever an assignment occurs, if needed, so we don't enlarge it now.
			frameindex = expr_threadFramesize;
			expr_threadFramesize = (expr_threadFramesize + 1);
		}
		else
		{
			//this allows the global frame to grow.
			frameindex = expr_enlarge(expr_globalFrame);
		}
	}
	else
	{
		if (dictionary->frameID == (*localFrame())->frameID)
		{
			//This takes care of scopes that span a file or the dictionary for a break loop,
			//with a single frame which ought to be allowed to grow.
			frameindex = expr_enlarge(*localFrame());
		}
		else
		{
			//this is a dynamic frame, not allocated yet.
			frameindex = dictionary->framesize;
			dictionary->framesize = (dictionary->framesize + 1);
		}
	}
	parse_Symbol tmp__21 = M2CPP_NewObject<parse_Symbol,struct parse_Symbol_struct>();
	tmp__21->word = word;
	tmp__21->hash = expr_nextHash();
	tmp__21->position = position;
	tmp__21->unary = expr_dummyUnaryFun;
	tmp__21->postfix = expr_dummyPostfixFun;
	tmp__21->binary = expr_dummyBinaryFun;
	tmp__21->frameID = dictionary->frameID;
	tmp__21->frameindex = frameindex;
	tmp__21->lookupCount = 1;
	tmp__21->Protected = 0;
	tmp__21->flagLookup = 0;
	tmp__21->thread = thread;
	return binding_insert(tmp__21, dictionary->symboltable);
}

parse_Symbol M2CPP_InterperterLocal::binding_makeEntry_1(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary)
{
	return binding_makeEntry(word, position, dictionary, 0, 0);
}

parse_Symbol M2CPP_InterperterLocal::binding_makeSymbol(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary,char thread,char locallyCreated)
{
	parse_Symbol entry_1 = binding_makeEntry(word, position, dictionary, thread, locallyCreated);
	if(dictionary->frameID == 0 && ctype_isalnum_2(word->name) && !thread)
	{
		if (entry_1->frameindex < 0 || entry_1->frameindex >= expr_globalFrame->values->len) fatalarrayindex(entry_1->frameindex,expr_globalFrame->values->len,__FILE__,__LINE__,35);
		parse_SymbolClosure tmp__25 = M2CPP_NewObject<parse_SymbolClosure,struct parse_SymbolClosure_struct>(SymbolClosure_typecode);
		tmp__25->frame = expr_globalFrame;
		tmp__25->symbol = entry_1;
		expr_globalFrame->values->array[entry_1->frameindex] = reinterpret_cast<parse_Expr>(tmp__25);
	}
	return entry_1;
}

parse_Symbol M2CPP_InterperterLocal::binding_makeSymbol_1(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary,char thread)
{
	return binding_makeSymbol(word, position, dictionary, thread, 0);
}

parse_Symbol M2CPP_InterperterLocal::binding_makeSymbol_2(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary)
{
	return binding_makeSymbol_1(word, position, dictionary, 0);
}

parse_SymbolClosure M2CPP_InterperterLocal::binding_makeProtectedSymbolClosure(parse_Word w)
{
	parse_Symbol entry_2 = binding_makeSymbol_2(w, stdiop0_dummyPosition, expr_globalDictionary);
	entry_2->Protected = 1;
	if (entry_2->frameindex < 0 || entry_2->frameindex >= expr_globalFrame->values->len) fatalarrayindex(entry_2->frameindex,expr_globalFrame->values->len,__FILE__,__LINE__,35);
	parse_Expr tmp__27 = expr_globalFrame->values->array[entry_2->frameindex];
	if (tmp__27 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (tmp__27->type_) {
	case SymbolClosure_typecode:
		{
			parse_SymbolClosure s = reinterpret_cast<parse_SymbolClosure>(tmp__27);
			return s;
		}
	default:
		{
			checkTypeValidity(tmp__27->type_,__FILE__,__LINE__);
			parse_SymbolClosure tmp__29 = M2CPP_NewObject<parse_SymbolClosure,struct parse_SymbolClosure_struct>(SymbolClosure_typecode);
			tmp__29->frame = expr_globalFrame;
			tmp__29->symbol = entry_2;
			return tmp__29;
		}
	};
}

parse_SymbolClosure M2CPP_InterperterLocal::binding_makeKeyword(parse_Word w)
{
	//Keywords differ from symbols in that their initial value isnull.
	parse_Symbol entry_3 = binding_makeEntry_1(w, stdiop0_dummyPosition, expr_globalDictionary);
	entry_3->Protected = 1;
	parse_SymbolClosure tmp__30 = M2CPP_NewObject<parse_SymbolClosure,parse_SymbolClosure_struct>(SymbolClosure_typecode);
	tmp__30->frame = expr_globalFrame;
	tmp__30->symbol = entry_3;
	if (entry_3->frameindex < 0 || entry_3->frameindex >= expr_globalFrame->values->len) fatalarrayindex(entry_3->frameindex,expr_globalFrame->values->len,__FILE__,__LINE__,30);
	expr_globalFrame->values->array[entry_3->frameindex] = reinterpret_cast<parse_Expr>(tmp__30);
	return tmp__30;
}

parse_SymbolClosure M2CPP_InterperterLocal::binding_makeProtectedSymbolClosure_1(M2_string s)
{
	return binding_makeProtectedSymbolClosure(lex_makeUniqueWord(s, expr_parseWORD));
}

parse_SymbolClosure M2CPP_InterperterLocal::binding_makeKeyword_1(M2_string s)
{
	return binding_makeKeyword(lex_makeUniqueWord(s, expr_parseWORD));
}

parse_Symbol M2CPP_InterperterLocal::binding_makeSymbol_3(parse_Token t)
{
	parse_Symbol e_2 = binding_makeSymbol_2(t->word, tokens_position(t), t->dictionary);
	t->entry = e_2;
	return e_2;
}

void M2CPP_InterperterLocal::binding_makeErrorTree(parse_ParseTree e,M2_string message)
{
	assert(0);
	i_binding_HadError() = 1;
	stdiop_printErrorMessage(parser_treePosition(e), message);
}

void M2CPP_InterperterLocal::binding_makeErrorTree_1(parse_Token e,M2_string message)
{
	assert(0);
	i_binding_HadError() = 1;
	tokens_printErrorMessage_1(e, message);
}

void M2CPP_InterperterLocal::binding_makeSymbol(parse_ParseTree e,parse_Dictionary dictionary)
{
	if (e == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (e->type_) {
	case Token_typecode:
		{
			parse_Token token_1 = reinterpret_cast<parse_Token>(e);
			token_1->dictionary = dictionary;
			binding_makeSymbol_3(token_1);
			break;
		}
	default:
		{
			checkTypeValidity(e->type_,__FILE__,__LINE__);
			binding_makeErrorTree(e, M2CPP_NewConstString("expected single identifier"));
			break;
		}
	};
}

parse_Symbol M2CPP_InterperterLocal::binding_lookup(parse_Word word,parse_SymbolHashTable table)
{
	if (table == expr_dummySymbolHashTable)
	{
		assert(0);
		err_error(M2CPP_NewConstString("dummy symbol table used"));
	}
	int h = (word->hash & (table->buckets->len - 1));
	if (h < 0 || h >= table->buckets->len) fatalarrayindex(h,table->buckets->len,__FILE__,__LINE__,20);
	parse_SymbolList entryList_1 = table->buckets->array[h];
	while(1)
	{
		if (NULL == entryList_1)
		{
			return NULL;
		}
		parse_SymbolListCell entryListCell_1 = reinterpret_cast<parse_SymbolListCell>(entryList_1);
		if (entryListCell_1->word == word)
		{
			parse_Symbol e_3 = entryListCell_1->entry;
			e_3->lookupCount = (e_3->lookupCount + binding_lookupCountIncrement());
			return e_3;
		}
		entryList_1 = entryListCell_1->next;
	}
	return NULL;
}

parse_Symbol M2CPP_InterperterLocal::binding_globalLookup(parse_Word w)
{
	parse_Dictionary d = expr_globalDictionary;
	do 
	{
		parse_Symbol tmp__73 = binding_lookup(w, d->symboltable);
		if (NULL != tmp__73)
			return tmp__73;
		d = d->outerDictionary;
	} while(d != d->outerDictionary);
	return NULL;
}

parse_Symbol M2CPP_InterperterLocal::binding_lookup_1(parse_Word w,parse_Dictionary d)
{
	do
	{
		parse_Symbol tmp__73 = binding_lookup(w, d->symboltable);
		if (NULL != tmp__73)
			return tmp__73;
		d = d->outerDictionary;
	} while(d != d->outerDictionary);
	return binding_globalLookup(w);
}

void M2CPP_InterperterLocal::binding_lookup(parse_Token t,char forcedef,char thread)
{
	int n = t->word->name->len;
	if(( n>=1 && ctype_isdigit_1(t->word->name->array[0]) ) || ( n>=2 && t->word->name->array[0] == '.' && ctype_isdigit_1(t->word->name->array[1])))
	{
	}
	else
	{
		parse_Symbol tmp__87 = binding_lookup_1(t->word, t->dictionary);
		if (NULL == tmp__87)
		{
			if (forcedef) 
			{
				//undefined variables are defined as global
				bool locallyCreated_1 = (t->dictionary->frameID != 0) && (expr_dictionaryDepth(t->dictionary) > 0);
				t->dictionary = expr_globalDictionary;
				t->entry = binding_makeSymbol(t->word, tokens_position(t), expr_globalDictionary, thread, locallyCreated_1);
			}
			else
			{
				tokens_printErrorMessage_1(t, strings_plus_(M2CPP_NewConstString("undefined symbol "), t->word->name));
				i_binding_HadError() = 1;
			}
		}
		else
		{	
			parse_Symbol entry_4 = ((parse_Symbol)tmp__87);
			t->entry = entry_4;
			if (entry_4->flagLookup) 
			{
				tokens_printErrorMessage_1(t, M2CPP_NewConstString("flagged symbol encountered"));
				i_binding_HadError() = 1;
			}
			if (thread && !entry_4->thread) 
			{
				tokens_printErrorMessage_1(t, M2CPP_NewConstString("symbol already present, but not thread local"));
				i_binding_HadError() = 1;
			}
		}
	}
}

void M2CPP_InterperterLocal::binding_lookup_1(parse_Token t)
{
	binding_lookup(t, 1, 0);
}

void M2CPP_InterperterLocal::binding_lookuponly(parse_Token t)
{
	binding_lookup(t, 0, 0);
}

void M2CPP_InterperterLocal::binding_bind(parse_Token t,parse_Dictionary dictionary)
{
	t->dictionary = dictionary;
	binding_lookup_1(t);
}

void M2CPP_InterperterLocal::binding_bindThread(parse_Token t,parse_Dictionary dictionary)
{
	t->dictionary = dictionary;
	binding_lookup(t, 1, 1);
}

void M2CPP_InterperterLocal::binding_bindop(parse_Token t,parse_Dictionary dictionary)
{
	t->dictionary = dictionary;
	binding_lookuponly(t);
}

void M2CPP_InterperterLocal::binding_bindFormalParm(parse_ParseTree e,parse_Dictionary dictionary,parse_functionDescription desc)
{
	if (e == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (e->type_) {
	case Token_typecode:
		{
			parse_Token t_1 = reinterpret_cast<parse_Token>(e);
			if (t_1->word->typecode == parse_TCid)
				binding_makeSymbol(e, dictionary);
			else
				binding_makeErrorTree_1(t_1, M2CPP_NewConstString("expected symbol"));
			desc->numparms = (desc->numparms + 1);
			break;
		}
	default:
			checkTypeValidity(e->type_,__FILE__,__LINE__);
			binding_makeErrorTree(e, M2CPP_NewConstString("syntax error: expected function parameter"));
			break;
	};
}

void M2CPP_InterperterLocal::binding_bindFormalParmList(parse_ParseTree e,parse_Dictionary dictionary,parse_functionDescription desc)
{
	if (e == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (e->type_) {
	case Binary_typecode:
		{
			parse_Binary binary_1 = reinterpret_cast<parse_Binary>(e);
			if (binary_1->Operator->word == binding_CommaW)
			{
				binding_bindFormalParmList(binary_1->lhs, dictionary, desc);
				binding_bindop(binary_1->Operator, dictionary);
				binding_bindFormalParm(binary_1->rhs, dictionary, desc);
			}
			else
				binding_makeErrorTree(e, M2CPP_NewConstString("syntax error: expected function parameter list"));
			break;
		}
	default:
		checkTypeValidity(e->type_,__FILE__,__LINE__);
		binding_bindFormalParm(e, dictionary, desc);
		break;
	};
}

void M2CPP_InterperterLocal::binding_bindSingleParm(parse_ParseTree e,parse_Dictionary dictionary)
{
	if (e == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (e->type_) {
	case Token_typecode:
		{
			parse_Token t_2 = reinterpret_cast<parse_Token>(e);
			if (t_2->word->typecode == parse_TCid) 
				binding_makeSymbol(e, dictionary);
			else
				binding_makeErrorTree_1(t_2, M2CPP_NewConstString("expected symbol"));
			break;
		}
	default:
		checkTypeValidity(e->type_,__FILE__,__LINE__);
		binding_makeErrorTree(e, M2CPP_NewConstString("expected symbol"));
		break;
	};
}

void M2CPP_InterperterLocal::binding_bindParenParmList(parse_ParseTree e,parse_Dictionary dictionary,parse_functionDescription desc)
{
	if (e == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (e->type_) {
	case Token_typecode:
		{
			parse_Token t_3 = reinterpret_cast<parse_Token>(e);
			binding_bindFormalParm(e, dictionary, desc);
			desc->restargs = 1;
			break;
		}
	case Parentheses_typecode:
		{
			parse_Parentheses p = reinterpret_cast<parse_Parentheses>(e);
			binding_bindFormalParmList(p->contents, dictionary, desc);
			break;
		}
	case EmptyParentheses_typecode:;
		{
			parse_EmptyParentheses p_1 = reinterpret_cast<parse_EmptyParentheses>(e);
			break;
		}
	default:
		checkTypeValidity(e->type_,__FILE__,__LINE__);
		binding_makeErrorTree(e, M2CPP_NewConstString("expected parenthesized argument list or symbol"));
		break;
	};
}

char M2CPP_InterperterLocal::binding_opHasBinaryMethod(parse_Symbol o)
{
	for(size_t i = 0; i < binding_opsWithBinaryMethod->len; ++i)
	{
		if(binding_opsWithBinaryMethod->array[i]->symbol==o)
			return true;
	}
	return false;
}

char M2CPP_InterperterLocal::binding_opHasUnaryMethod(parse_Symbol o)
{
	for(size_t i = 0; i < binding_opsWithBinaryMethod->len; ++i)
	{
		if(binding_opsWithUnaryMethod->array[i]->symbol==o)
			return true;
	}
	return false;
}

char M2CPP_InterperterLocal::binding_opHasPostfixMethod(parse_Symbol o)
{
	for(size_t i = 0; i < binding_opsWithBinaryMethod->len; ++i)
	{
		if(binding_opsWithPostfixMethod->array[i]->symbol==o)
			return true;
	}
	return false;
}

void M2CPP_InterperterLocal::binding_bindTokenLocally(parse_Token t,parse_Dictionary dictionary)
{
	binding_lookupCountIncrement() = 0;
	parse_Symbol r_1 = binding_lookup_1(t->word, dictionary);
	binding_lookupCountIncrement() = 1;
	if (NULL != r_1) 
	{
		parse_Symbol entry_5 = r_1;
		if (dictionary->frameID == entry_5->frameID)
		{
			tokens_printWarningMessage(t, strings_plus_(strings_plus_(M2CPP_NewConstString("local declaration of "), t->word->name), M2CPP_NewConstString(" shields variable with same name")));
		}
	}
	t->dictionary = dictionary;
	binding_makeSymbol_3(t);
}

void M2CPP_InterperterLocal::binding_bindToken(parse_Token t,parse_Dictionary dictionary,char colon)
{
	if (colon) 
		binding_bindTokenLocally(t, dictionary);
	else
		binding_bind(t, dictionary);
}

void M2CPP_InterperterLocal::binding_bindParallelAssignmentItem(parse_ParseTree e,parse_Dictionary dictionary,char colon)
{
	if (e == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (e->type_) {
	case Token_typecode:
		{
			parse_Token token_2 = reinterpret_cast<parse_Token>(e);
			if (token_2->word->typecode != parse_TCid)
				binding_makeErrorTree_1(token_2, M2CPP_NewConstString("syntax error: parallel assignment expected symbol"));
			else
				binding_bindToken(token_2, dictionary, colon);
			break;
		}
	default:
		checkTypeValidity(e->type_,__FILE__,__LINE__);
		binding_makeErrorTree(e, M2CPP_NewConstString("syntax error: parallel assignment expected symbol"));
		break;
	};
}

void M2CPP_InterperterLocal::binding_bindParallelAssignmentList(parse_ParseTree e,parse_Dictionary dictionary,char colon)
{
	if (e == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (e->type_) {
	case Binary_typecode:
		{
			parse_Binary binary_2 = reinterpret_cast<parse_Binary>(e);
			if (binary_2->Operator->word == binding_CommaW)
			{
				binding_bindParallelAssignmentList(binary_2->lhs, dictionary, colon);
				binding_bindop(binary_2->Operator, dictionary);
				binding_bindParallelAssignmentItem(binary_2->rhs, dictionary, colon);
			}
			else
				binding_makeErrorTree(e, M2CPP_NewConstString("syntax error: parallel assignment expected symbol list"));
			break;
		}
	default:
		checkTypeValidity(e->type_,__FILE__,__LINE__);
		binding_bindParallelAssignmentItem(e, dictionary, colon);
		break;
	};
}

void M2CPP_InterperterLocal::binding_bindassignment(parse_Binary assn,parse_Dictionary dictionary,char colon)
{
	binding_bindop(assn->Operator, dictionary);
	parse_ParseTree body = assn->rhs;
	parse_ParseTree tmp__111 = assn->lhs;
	if (tmp__111 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (tmp__111->type_) {
	case Parentheses_typecode:
		{
			parse_Parentheses p_2 = reinterpret_cast<parse_Parentheses>(tmp__111);
			binding_bindParallelAssignmentList(p_2->contents, dictionary, colon);
			binding_bind(body, dictionary);
			break;
		}
	case Token_typecode:
		{
			parse_Token token_3 = reinterpret_cast<parse_Token>(tmp__111);
			if (token_3->word->typecode != parse_TCid)
			{
				binding_makeErrorTree_1(assn->Operator, strings_plus_(strings_plus_(M2CPP_NewConstString("expected a symbol to left of '"), assn->Operator->entry->word->name), M2CPP_NewConstString("'")));
				return;
			}
			binding_bindToken(token_3, dictionary, colon);
			binding_bind(body, dictionary);
			break;
		}
	case Adjacent_typecode:
		{
			parse_Adjacent a = reinterpret_cast<parse_Adjacent>(tmp__111);
			binding_bind(a->lhs, dictionary);
			binding_bind(a->rhs, dictionary);
			binding_bind(body, dictionary);
			break;
		}
	case Unary_typecode:
		{
			parse_Unary unary_1 = reinterpret_cast<parse_Unary>(tmp__111);
			binding_bindop(unary_1->Operator, dictionary);
			binding_bind(unary_1->rhs, dictionary);
			binding_bind(body, dictionary);
			if (colon) 
			{
				if (!(binding_opHasUnaryMethod(unary_1->Operator->entry)))
				{
					binding_makeErrorTree_1(assn->Operator, M2CPP_NewConstString("can't assign a method for this unary operator"));
				}
			}
			else
			{
				if (!(binding_opHasUnaryMethod(unary_1->Operator->entry)))
				{
					binding_makeErrorTree_1(assn->Operator, M2CPP_NewConstString("can't assign a value for this unary operator"));
				}
			}
			break;
		}
	case Postfix_typecode:
		{
			parse_Postfix unary_2 = reinterpret_cast<parse_Postfix>(tmp__111);
			binding_bind(unary_2->lhs, dictionary);
			binding_bindop(unary_2->Operator, dictionary);
			binding_bind(body, dictionary);
			if (colon) 
			{
				if (!(binding_opHasPostfixMethod(unary_2->Operator->entry)))
				{
					binding_makeErrorTree_1(assn->Operator, M2CPP_NewConstString("can't assign a method for this postfix operator"));
				}
			}
			else
			{
				if (!(binding_opHasPostfixMethod(unary_2->Operator->entry)))
				{
					binding_makeErrorTree_1(assn->Operator, M2CPP_NewConstString("can't assign a value for this postfix operator"));
				}
			}
			break;
		}
	case Binary_typecode:
		{
			parse_Binary binary_3 = reinterpret_cast<parse_Binary>(tmp__111);
			binding_bind(binary_3->lhs, dictionary);
			binding_bindop(binary_3->Operator, dictionary);
			if (binary_3->Operator->word == binding_DotS->symbol->word)
			{
				binding_bind(binary_3->rhs, expr_globalDictionary);
			}
			else
			{
				binding_bind(binary_3->rhs, dictionary);
			}
			binding_bind(body, dictionary);
			if (colon) 
			{
				if (!(binding_opHasBinaryMethod(binary_3->Operator->entry)))
				{
					binding_makeErrorTree_1(assn->Operator, M2CPP_NewConstString("can't assign a method for this binary operator"));
				}
			}
			else
			{
				if (!(binary_3->Operator->word == binding_DotS->symbol->word || 
					  binary_3->Operator->word == binding_SharpS->symbol->word || 
					  binding_opHasBinaryMethod(binary_3->Operator->entry)))
				{
					binding_makeErrorTree_1(assn->Operator, M2CPP_NewConstString("can't assign a value for this binary operator"));
				}
				if (binary_3->Operator->word == binding_DotS->symbol->word)
				{
					parse_ParseTree tmp__115 = binary_3->rhs;
					if (tmp__115 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
					switch (tmp__115->type_) {
					case Token_typecode:
						{
							parse_Token t_4 = reinterpret_cast<parse_Token>(tmp__115);
							if (t_4->word->typecode != parse_TCid)
							{
								binding_makeErrorTree_1(assn->Operator, M2CPP_NewConstString("expected a symbol to right of '.'"));
							}
							break;
						}
					default:
						checkTypeValidity(tmp__115->type_,__FILE__,__LINE__);
						binding_makeErrorTree_1(assn->Operator, M2CPP_NewConstString("expected a symbol to right of '.'"));
						break;
					};
				}
			}
			break;
		}
	case New_typecode:
		{
			parse_New n_1 = reinterpret_cast<parse_New>(tmp__111);
			if (colon) 
			{
				binding_bind(n_1->newclass, dictionary);
				binding_bind(n_1->newparent, dictionary);
				binding_bind(n_1->newinitializer, dictionary);
				binding_bind(body, dictionary);
			}
			else
			{
				binding_makeErrorTree_1(assn->Operator, M2CPP_NewConstString("left hand side of assignment inappropriate"));
			}
			break;
		}
	default:
		checkTypeValidity(tmp__111->type_,__FILE__,__LINE__);
		binding_makeErrorTree_1(assn->Operator, M2CPP_NewConstString("left hand side of assignment inappropriate"));
		break;
	};
}

parse_ParseTree M2CPP_InterperterLocal::binding_bindnewdictionary(parse_ParseTree e,parse_Dictionary dictionary)
{
	parse_Dictionary n_2 = expr_newLocalDictionary(dictionary);
	binding_bind(e, n_2);
	parse_StartDictionary tmp__116 = M2CPP_NewObject<parse_StartDictionary,struct parse_StartDictionary_struct>(StartDictionary_typecode);
	tmp__116->dictionary = n_2;
	tmp__116->body = e;
	return reinterpret_cast<parse_ParseTree>(tmp__116);
}

void M2CPP_InterperterLocal::binding_bind(parse_ParseTree e,parse_Dictionary dictionary)
{
	if (e == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (e->type_) {;
	case StartDictionary_typecode:
		{
			parse_StartDictionary s_4 = reinterpret_cast<parse_StartDictionary>(e);
			binding_bind(s_4->body, dictionary);
			break;
		}
	case IfThen_typecode:
		{
			parse_IfThen i = reinterpret_cast<parse_IfThen>(e);
			binding_bind(i->predicate, dictionary);
			binding_bind(i->thenclause, dictionary);
			break;
		}
	case IfThenElse_typecode:
		{
			parse_IfThenElse i_1 = reinterpret_cast<parse_IfThenElse>(e);
			binding_bind(i_1->predicate, dictionary);
			binding_bind(i_1->thenclause, dictionary);
			binding_bind(i_1->elseClause, dictionary);
			break;
		}
	case Token_typecode:
		{
			parse_Token token_4 = reinterpret_cast<parse_Token>(e);
			if (token_4->word->typecode == parse_TCid)
			{
				binding_bind(token_4, dictionary);
			}
			break;
		}
	case Adjacent_typecode:
		{
			parse_Adjacent adjacent = reinterpret_cast<parse_Adjacent>(e);
			binding_bind(adjacent->lhs, dictionary);
			binding_bind(adjacent->rhs, dictionary);
			break;
		}
	case Binary_typecode:
		{
			parse_Binary binary_4 = reinterpret_cast<parse_Binary>(e);
			if (binary_4->Operator->word == binding_EqualW)
			{
				binding_bindassignment(binary_4, dictionary, 0);
			}
			else
			{
				if (binary_4->Operator->word == binding_ColonEqualW)
					binding_bindassignment(binary_4, dictionary, 1);
				else
				{
					if (binary_4->Operator->word == binding_DotS->symbol->word)
					{
						binding_bind(binary_4->lhs, dictionary);
						binding_bindop(binary_4->Operator, dictionary);
						binding_bind(binary_4->rhs, expr_globalDictionary);
						parse_ParseTree tmp__117 = binary_4->rhs;
						if (tmp__117 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
						switch (tmp__117->type_) {
						case Token_typecode:
							{
								parse_Token token_5 = reinterpret_cast<parse_Token>(tmp__117);
								if (token_5->word->typecode != parse_TCid)
									binding_makeErrorTree_1(binary_4->Operator, M2CPP_NewConstString("expected a symbol to right of '.'"));
								break;
							}
						default:
							checkTypeValidity(tmp__117->type_,__FILE__,__LINE__);
							binding_makeErrorTree_1(binary_4->Operator, M2CPP_NewConstString("expected a symbol to right of '.'"));
							break;
						};
					}
					else
					{
						if (binary_4->Operator->word == binding_DotQuestionS->symbol->word)
						{
							binding_bind(binary_4->lhs, dictionary);
							binding_bindop(binary_4->Operator, dictionary);
							binding_bind(binary_4->rhs, expr_globalDictionary);
							parse_ParseTree tmp__118 = binary_4->rhs;
							if (tmp__118 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
							switch (tmp__118->type_) {
							case Token_typecode:
								{
									parse_Token token_6 = reinterpret_cast<parse_Token>(tmp__118);
									if (token_6->word->typecode != parse_TCid)
									{
										binding_makeErrorTree_1(binary_4->Operator, M2CPP_NewConstString("expected a symbol to right of '.?'"));
									}
									break;
								}
							default:
								checkTypeValidity(tmp__118->type_,__FILE__,__LINE__);
								binding_makeErrorTree_1(binary_4->Operator, M2CPP_NewConstString("expected a symbol to right of '.?'"));
								break;
							};
						}
						else
						{
							binding_bind(binary_4->lhs, dictionary);
							binding_bindop(binary_4->Operator, dictionary);
							binding_bind(binary_4->rhs, dictionary);
						}
					}
				}
			}
			break;
		}
	case LocalQuote_typecode:
		{
			parse_LocalQuote q = reinterpret_cast<parse_LocalQuote>(e);
			binding_bind(q->Operator, dictionary);
			parse_Token tok = q->rhs;
			tok->dictionary = dictionary;
			parse_Symbol r_2 = binding_lookup(tok->word, dictionary->symboltable);
			if (NULL == r_2) goto L111_;
			goto L112_;
		L112_:;
			tok->entry = r_2;
			goto L113_;
		L111_:;
			binding_makeSymbol_3(tok);
			goto L113_;
		L113_:;
			break;
		}
	case GlobalQuote_typecode:
		{
			parse_GlobalQuote q_1 = reinterpret_cast<parse_GlobalQuote>(e);
			binding_bind(q_1->Operator, dictionary);
			binding_bind(q_1->rhs, expr_globalDictionary);
			break;
		}
	case ThreadQuote_typecode:
		{
			parse_ThreadQuote q_2 = reinterpret_cast<parse_ThreadQuote>(e);
			binding_bind(q_2->Operator, dictionary);
			binding_bindThread(q_2->rhs, expr_globalDictionary);
			break;
		}
	case Quote_typecode:
		{
			parse_Quote q_3 = reinterpret_cast<parse_Quote>(e);
			binding_bind(q_3->Operator, dictionary);
			binding_bind(q_3->rhs, dictionary);
			break;
		}
	case Arrow_typecode:
		{
			parse_Arrow a_1 = reinterpret_cast<parse_Arrow>(e);
			parse_Dictionary newdict = expr_newLocalDictionary(dictionary);
			parse_functionDescription tmp__119 = (parse_functionDescription) GC_MALLOC_ATOMIC(sizeof(struct parse_functionDescription_struct));
			tmp__119->frameID = newdict->frameID;
			tmp__119->framesize = 0;
			tmp__119->numparms = 0;
			tmp__119->restargs = 0;
			a_1->desc = tmp__119;
			binding_bindParenParmList(a_1->lhs, newdict, a_1->desc);
			binding_bind(a_1->rhs, newdict);
			a_1->desc->framesize = newdict->framesize;
			break;
		}
	case Unary_typecode:
		{
			parse_Unary unary_3 = reinterpret_cast<parse_Unary>(e);
			binding_bindop(unary_3->Operator, dictionary);
			binding_bind(unary_3->rhs, dictionary);
			break;
		}
	case Postfix_typecode:
		{
			parse_Postfix postfix_1 = reinterpret_cast<parse_Postfix>(e);
			binding_bind(postfix_1->lhs, dictionary);
			binding_bindop(postfix_1->Operator, dictionary);
			break;
		}
	case Parentheses_typecode:
		{
			parse_Parentheses ee = reinterpret_cast<parse_Parentheses>(e);
			binding_bind(ee->contents, dictionary);
			break;
		}
	case EmptyParentheses_typecode:
		break;
	case dummy_typecode:
		break;
	case WhileDo_typecode:
		{
			parse_WhileDo w_1 = reinterpret_cast<parse_WhileDo>(e);
			binding_bind(w_1->predicate, dictionary);
			binding_bind(w_1->doClause, dictionary);
			break;
		}
	case For_typecode:
		{
			parse_For w_2 = reinterpret_cast<parse_For>(e);
			binding_bind(w_2->inClause, dictionary);
			binding_bind(w_2->fromClause, dictionary);
			binding_bind(w_2->toClause, dictionary);
			parse_Dictionary newdict_1 = expr_newLocalDictionary(dictionary);
			binding_bindSingleParm(w_2->variable, newdict_1);
			binding_bind(w_2->whenClause, newdict_1);
			binding_bind(w_2->listClause, newdict_1);
			binding_bind(w_2->doClause, newdict_1);
			w_2->dictionary = newdict_1;
			break;
		}
	case WhileList_typecode:
		{
			parse_WhileList w_3 = reinterpret_cast<parse_WhileList>(e);
			binding_bind(w_3->predicate, dictionary);
			binding_bind(w_3->listClause, dictionary);
			break;
		}
	case WhileListDo_typecode:
		{
			parse_WhileListDo w_4 = reinterpret_cast<parse_WhileListDo>(e);
			binding_bind(w_4->predicate, dictionary);
			binding_bind(w_4->listClause, dictionary);
			binding_bind(w_4->doClause, dictionary);
			break;
		}
	case New_typecode:
		{
			parse_New n_3 = reinterpret_cast<parse_New>(e);
			binding_bind(n_3->newclass, dictionary);
			binding_bind(n_3->newparent, dictionary);
			binding_bind(n_3->newinitializer, dictionary);
			break;
		}
	case TryElse_typecode:
		{
			parse_TryElse i_2 = reinterpret_cast<parse_TryElse>(e);
			binding_bind(i_2->primary, dictionary);
			binding_bind(i_2->alternate, dictionary);
			break;
		}
	case TryThenElse_typecode:;
		{
			parse_TryThenElse i_3 = reinterpret_cast<parse_TryThenElse>(e);
			binding_bind(i_3->primary, dictionary);
			binding_bind(i_3->sequel, dictionary);
			binding_bind(i_3->alternate, dictionary);
			break;
		}
	case Try_typecode:
		{
			parse_Try i_4 = reinterpret_cast<parse_Try>(e);
			binding_bind(i_4->primary, dictionary);
			break;
		}
	case Catch_typecode:
		{
			parse_Catch i_5 = reinterpret_cast<parse_Catch>(e);
			binding_bind(i_5->primary, dictionary);
			break;
		}
	};
}

bool M2CPP_InterperterLocal::binding_localBind(parse_ParseTree e,parse_Dictionary dictionary)
{
	i_binding_HadError() = 0;
	binding_bind(e, dictionary);
	return !i_binding_HadError();
}

parse_Symbol M2CPP_InterperterLocal::setupVariable(M2_string name, parse_Expr value, bool thread)
{
	parse_Word word_3 = lex_makeUniqueWord(name, expr_parseWORD);
	parse_Symbol tmp__93 = binding_lookup_1(word_3, expr_globalDictionary);
	if(NULL == tmp__93)
	{
		parse_Symbol entry_2 = binding_makeSymbol_1(word_3, stdiop0_dummyPosition, expr_globalDictionary, thread);
		parse_Frame tmp__94;
		if (thread) 
			tmp__94 = expr_enlargeThreadFrame();
		else
			tmp__94 = expr_globalFrame;
		if (entry_2->frameindex < 0 || entry_2->frameindex >= tmp__94->values->len)
		{
			assert(0);
			fatalarrayindex(entry_2->frameindex,tmp__94->values->len,__FILE__,__LINE__,78);
		}
		tmp__94->values->array[entry_2->frameindex] = value;
		return entry_2;
	}
	else
	{
		parse_Symbol entry_3 = ((parse_Symbol)tmp__93);
		parse_Frame tmp__97;
		if (thread) 
		{
			assert(entry_3->thread);
			tmp__97 = expr_enlargeThreadFrame();
		}
		else
			tmp__97 = expr_globalFrame;
		if (entry_3->frameindex < 0 || entry_3->frameindex >= tmp__97->values->len)
		{
			assert(0);
			fatalarrayindex(entry_3->frameindex,tmp__97->values->len,__FILE__,__LINE__,78);
		}
		tmp__97->values->array[entry_3->frameindex] = value;
		return entry_3;
	}
}
parse_Symbol M2CPP_InterperterLocal::setupVariable(parse_Expr value, const VariableOptions& params)
{
}
parse_Symbol M2CPP_InterperterLocal::setupVariable(int value, const VariableOptions& params)
{
	return setupVariable(util_toExpr(value),params);
}
parse_Symbol M2CPP_InterperterLocal::setupVariable(long value, const VariableOptions& params)
{
 	return setupVariable(util_toExpr_1(value),params);
}
parse_Symbol M2CPP_InterperterLocal::setupVariable(unsigned long value, const VariableOptions& params)
{
	return setupVariable(util_toExpr_2(value),params);
}
parse_Symbol M2CPP_InterperterLocal::setupVariable(float value, const VariableOptions& params)
{
	return setupVariable(util_toExpr_9(value),params);
}
parse_Symbol M2CPP_InterperterLocal::setupVariable(double value, const VariableOptions& params)
{
	return setupVariable(util_toExpr_9(value),params);
}
parse_Symbol M2CPP_InterperterLocal::setupVariable(const std::string& value, const VariableOptions& params)
{
	return setupVariable(value.c_str(),params);
}
parse_Symbol M2CPP_InterperterLocal::setupVariable(const char* value, const VariableOptions& params)
{
	return setupVariable(params.name(),util_toExpr_4(M2CPP_NewString(value)),params.isThread());
}
