#pragma once
#include <fstream>
#include "typedefs.hpp"
#include "../e/newdelete.hpp"
#include "variableoptions.hpp"

//exports to d.
extern "C" {
	extern void interp_process();
	extern parse_Expr interp_value(parse_Expr e);
	extern parse_Expr interp_readeval(parse_TokenFile file,char returnLastvalue,char returnIfError);
	extern parse_Expr interp_readeval3(parse_TokenFile file,char printout,parse_DictionaryClosure dc,char returnLastvalue,char stopIfBreakReturnContinue,char returnIfError);
	extern parse_Symbol binding_insert(parse_Symbol entry,parse_SymbolHashTable table);
	extern parse_Symbol binding_insert_1(parse_SymbolHashTable table,parse_Word newname,parse_Symbol entry);
	extern parse_Symbol binding_makeEntry(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary,char thread,char locallyCreated);
	extern parse_Symbol binding_makeEntry_1(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary);
	extern parse_Symbol binding_makeSymbol(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary,char thread,char locallyCreated);
	extern parse_Symbol binding_makeSymbol_1(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary,char thread);
	extern parse_Symbol binding_makeSymbol_2(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary);
	extern parse_SymbolClosure binding_makeProtectedSymbolClosure(parse_Word w);
	extern parse_SymbolClosure binding_makeKeyword(parse_Word w);
	extern parse_SymbolClosure binding_makeProtectedSymbolClosure_1(M2_string s);
	extern parse_SymbolClosure binding_makeKeyword_1(M2_string s);
	extern parse_Symbol binding_makeSymbol_3(parse_Token t);
	extern void binding_makeErrorTree(parse_ParseTree e,M2_string message);
	extern void binding_makeErrorTree_1(parse_Token e,M2_string message);
	extern void binding_makeSymbol_4(parse_ParseTree e,parse_Dictionary dictionary);
	extern parse_Symbol binding_lookup(parse_Word word,parse_SymbolHashTable table);
	extern parse_Symbol binding_globalLookup(parse_Word w);
	extern parse_Symbol binding_lookup_1(parse_Word w,parse_Dictionary d);
	extern void binding_bind(parse_ParseTree e,parse_Dictionary dictionary);
	extern char binding_localBind(parse_ParseTree e,parse_Dictionary dictionary);
	extern parse_Symbol common_setupvar(M2_string name,parse_Expr value,char thread);
}

/***
	The idea here is that this class is designed to hold thread/instance specific information for a given global interperter state.
	Recall that we may have multiple threads running on one global dictionary.  Hence we need a common place to store local state.
***/
class M2CPP_InterperterLocal : public our_new_delete
{
public:
	/***
		Default constructor for initializing local variables.
	***/
	M2CPP_InterperterLocal();
	//These are the class equivalents for the thread local variables.
	/***
		Local frame for evaluation.
	***/
	parse_Frame*& localFrame();
	bool& stopIfError();
	/***
		Debug level for the current local interperter.
	***/
	int& debugLevel();
	/***
		StdErr file for the local interperter.
	***/
	errio_BasicFile* M2_stderr();
	/***
		Boolean interrupted flag for the local interperter.
	***/
	struct atomic_field* interruptedFlag();
	/***
		Set the interrupted flag.
		@param value True for interrupted, false to clear.
	***/
	void setInterruptedFlag(bool value);
	/***
		Get the interrupted flag.
		@return True for interrupted, false otherwise.
	***/
	bool getInterruptedFlag();
	/***
		True if interrupt pending, false otherwise.
	***/
	bool& interruptPending();
	/***
		True if errors should be surpressed, false otherwise.
	***/
	bool shouldSuppressErrors() { return m_SuppressErrors; }
	/***
		Some sort of state variable for binding.
	 ***/
	int& binding_lookupCountIncrement() { return m_BindingLookupCountIncrement; }
	/***
		True if binding had an error, false otherwise.
		This should not be accessed outside of binding functions.
	***/
	bool& i_binding_HadError() { return m_BindingHadError; }
	/***
		This is the entry point for an interperter.
	***/
	void interp_process();

	/***
		Attempt to execute the given expression.  
		@param e SymbolClosure, CodeClosure,stringCell, not null.
		@return Expr, possibly an error.
	***/
	parse_Expr value(parse_Expr e);
	/***
		???
		@param returnLastvalue ???
		@param returnIfError ???
		@param file File to read & evaluate.
		@return Expr, possibly an error.
	***/
	parse_Expr readeval(parse_TokenFile file,char returnLastvalue,char returnIfError);
	/***
		Evaluate the given file with the given local directory closure
		@param file File to evaluate.
		@param printout ???
		@param dc Dictionary Closure to consider as top level local dictionary.
		@param returnLastValue ???
		@param stopIfBreakReturnContinue ???
		@param returnIfError ???
		@return Expr, possibly an error.
	***/
	parse_Expr readeval3(parse_TokenFile file,char printout,parse_DictionaryClosure dc,char returnLastvalue,char stopIfBreakReturnContinue,char returnIfError);

	/***
		Setup the variable in global namespace.
		@param name The name of the variable.
		@param value The value of the variable.
		@param thread True if thread local variable, false otherwise.
	 ***/
	parse_Symbol setupVariable(M2_string name, parse_Expr value, bool thread);

	/***
		Append the entry to the table.
		This assumes there is room in the table to append.
		If there is not, undefined results may occur. 
		Thread safe because always called from enlarge and thus never has readers while appending.
		Note this currently assumes x86 memory ordering.
		@param buckets Hash table buckets to insert into.
		@param word Name of symbol.
		@param entry Symbol to append.
	***/
	void binding_append(struct SCC_M2_0_int_len_parse_SymbolList_array1 * buckets,parse_Word word,parse_Symbol entry);
	/***
		Enlarge the table.
		This does copy and replace on the table.
		Thread safe because always called from inside lock
		Note this currently assumes x86 memory ordering.
		@param table Table to enlarge.
	***/
	void binding_enlarge(parse_SymbolHashTable table);
	/***
		Insert the entry into the table.
		Thread safe because it replaces bucket with new bucket so no double counting
		Note this currently assumes x86 memory ordering.
		@param entry Entry to be inserted.  Dictionary of entry must be this table.
		@param table Table to insert into.
		@return entry
	***/
	parse_Symbol binding_insert(parse_Symbol entry,parse_SymbolHashTable table);
	/***
		Insert the entry into the table.
		Thread safe because it replaces bucket with new bucket so no double counting
		Note this currently assumes x86 memory ordering.
		@param table Table to insert into.
		@param newname Name of entry.  
		@param entry Entry to be inserted.  Dictionary of entry must be this table.
		@return entry
	***/
	parse_Symbol binding_insert_1(parse_SymbolHashTable table,parse_Word newname,parse_Symbol entry);
	/***
		Make a entry in the dictionary and allocate space in appropriate frame for entry.
		@param word Name of entry.
		@param position Position of entry in file.
		@param dictionary Dictionary to create entry in.
		@param thread True if thread local, false otherwise.
		@param locallyCreated Unused.
		@return New symbol, not null.
	***/
	parse_Symbol binding_makeEntry(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary,char thread,char locallyCreated);
	/***
		Make a non-thread local not-locally-created entry.
		@param word Name of entry.
		@param position Position of entry in file.
		@param dictionary Dictionary to create entry in.
		@return New symbol, not null.
	***/
	parse_Symbol binding_makeEntry_1(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary);
	/***
		???
		@param word Name of entry.
		@param position Position of entry in file.
		@param dictionary Dictionary to create entry in.
		@param thread True if thread local, false otherwise.
		@param locallyCreated Unused.
		@return New symbol, not null.
	***/
	parse_Symbol binding_makeSymbol(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary,char thread,char locallyCreated);
	/***
		???
		@param word Name of entry.
		@param position Position of entry in file.
		@param dictionary Dictionary to create entry in.
		@param thread True if thread local, false otherwise.
		@return New symbol, not null.
	***/
	parse_Symbol binding_makeSymbol_1(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary,char thread);
	/***
		???
		@param word Name of entry.
		@param position Position of entry in file.
		@param dictionary Dictionary to create entry in.
		@return New symbol, not null.
	***/
	parse_Symbol binding_makeSymbol_2(parse_Word word,stdiop0_Position position,parse_Dictionary dictionary);
	/***
		Create a protected symbol closure at the global scope.
		@param w Name of symbol closure.
		@return New symbol closure, not null.
	***/
	parse_SymbolClosure binding_makeProtectedSymbolClosure(parse_Word w);
	/***
		Create a keyword at the global scope.
		@param w Name of keyword.
		@return New symbol closure, not null.
	***/
	parse_SymbolClosure binding_makeKeyword(parse_Word w);
	/***
		Create a protected symbol closure at the global scope.
		@param s Name of symbol closure.
		@return New symbol closure, not null.
	***/
	parse_SymbolClosure binding_makeProtectedSymbolClosure_1(M2_string s);
	/***
		Create a keyword at the global scope.
		@param s Name of keyword.
		@return New symbol, not null.
	***/
	parse_SymbolClosure binding_makeKeyword_1(M2_string s);
	/***
	???
	@param t ???
	***/
	parse_Symbol binding_makeSymbol_3(parse_Token t);
	/***
		Raise an error.  Does not immediately terminate execution.
		@param e Parse tree that raised error.
		@param message Error message.
	***/
	void binding_makeErrorTree(parse_ParseTree e,M2_string message);
	/***
		Raise an error.  Does not immediately terminate execution.
		@param e Parse tree that raised error.
		@param message Error message.
	***/
	void binding_makeErrorTree_1(parse_Token e,M2_string message);
	/***
		???
		@param e ???
		@param dictionary ???
	***/
	void binding_makeSymbol(parse_ParseTree e,parse_Dictionary dictionary);
	/***
		???
		Note: This function has weird side effects via thread local lookupCountIncrement.
		@param word ???
		@param table ???
	***/
	parse_Symbol binding_lookup(parse_Word word,parse_SymbolHashTable table);
	/***
		???
		@param word ???
		@return Symbol or NULL
	***/
	parse_Symbol binding_globalLookup(parse_Word w);
	/***
		This is the same as a global lookup except that this lets you specify a dictionary to start looking in before performing global lookup.
		@param w ???
		@param d ???
		@return Symbol or NULL
	***/
	parse_Symbol binding_lookup_1(parse_Word w,parse_Dictionary d);
	/***
		@param t ???
		@param forcedef Should this force definition of the symbol at global scope if the definition does not exist.
		@param thread ???
		@return Symbol or NULL
	***/
	void binding_lookup(parse_Token t,char forcedef,char thread);
	/***
		???
		@param t ???
	***/
	void binding_lookup_1(parse_Token t);
	/***
		???
		@param t ???
	***/
	void binding_lookuponly(parse_Token t);
	/***
		???
		@param t ???
		@param dictionary ???
	***/
	void binding_bind(parse_Token t,parse_Dictionary dictionary);
	/***
		???
		@param t ???
		@param dictionary ???
	***/
	void binding_bindThread(parse_Token t,parse_Dictionary dictionary);
	/***
		???
		@param t ???
		@param dictionary ???
	***/
	void binding_bindop(parse_Token t,parse_Dictionary dictionary);
	/***
		???
		@param t ???
		@param dictionary ???
		@param desc ???
	***/
	void binding_bindFormalParm(parse_ParseTree e,parse_Dictionary dictionary,parse_functionDescription desc);
	/***
		???
		@param e ???
		@param dictionary ???
		@param desc ???
	***/
	void binding_bindFormalParmList(parse_ParseTree e,parse_Dictionary dictionary,parse_functionDescription desc);
	/***
		???
		@param e ???
		@param dictionary ???
	***/
	void binding_bindSingleParm(parse_ParseTree e,parse_Dictionary dictionary);
	/***
		???
		@param e ???
		@param dictionary ???
		@param desc ???
	***/
	void binding_bindParenParmList(parse_ParseTree e,parse_Dictionary dictionary,parse_functionDescription desc);
	/***
		???
		@param o ???
	***/
	char binding_opHasBinaryMethod(parse_Symbol o);
	/***
		???
		@param o
	***/
	char binding_opHasUnaryMethod(parse_Symbol o);
	/***
		???
		@param o
	***/
	char binding_opHasPostfixMethod(parse_Symbol o);
	/***
		???
		@param t ???
		@param dictionary ???
	***/
	void binding_bindTokenLocally(parse_Token t,parse_Dictionary dictionary);
	/***
		???
		@param t ???
		@param dictionary ???
		@param colon ???
	***/
	void binding_bindToken(parse_Token t,parse_Dictionary dictionary,char colon);
	/***
		???
		@param e ???
		@param dictionary ???
		@param colon ???
	***/
	void binding_bindParallelAssignmentItem(parse_ParseTree e,parse_Dictionary dictionary,char colon);
	/***
		???
		@param e ???
		@param dictionary ???
		@param colon ???
	***/
	void binding_bindParallelAssignmentList(parse_ParseTree e,parse_Dictionary dictionary,char colon);
	/***
		Handle binding an assignment operator.
		@param assn ???
		@param dictionary The "local" dictionary for this bind operation.
		@param colon ???
	***/
	void binding_bindassignment(parse_Binary assn,parse_Dictionary dictionary,char colon);
	/***
		???
		@param e
		@param dictionary ???
	***/
	parse_ParseTree binding_bindnewdictionary(parse_ParseTree e,parse_Dictionary dictionary);
	/***
		Execute the bind on the parse tree.
		This is called recursively.
		@param e The parse tree to bind.
		@param dictionary The "local" dictionary for this bind operation.
	***/
	void binding_bind(parse_ParseTree e,parse_Dictionary dictionary);
	/***
		This is the binding function to call to attempt to bind.
		It resets data specific to binding in this local context.
		In particular it resets the binding error flag.
		@param e The parse tree to bind.
		@param dictionary The "local" dictionary for this bind operation.
		@return True on success, false on failure.
	***/
	bool binding_localBind(parse_ParseTree e,parse_Dictionary dictionary);
	/***
		Setup the variable according to the variable options.
		@param value The value to be assigned to the variable.
		@param params Parameters of the variable to setup.
	***/
	parse_Symbol setupVariable(int value, const VariableOptions& params);	
	/***
		Setup the variable according to the variable options.
		@param value The value to be assigned to the variable.
		@param params Parameters of the variable to setup.
	***/
	parse_Symbol setupVariable(long value, const VariableOptions& params);	
	/***
		Setup the variable according to the variable options.
		@param value The value to be assigned to the variable.
		@param params Parameters of the variable to setup.
	***/
	parse_Symbol setupVariable(unsigned int value, const VariableOptions& params);	
	/***
		Setup the variable according to the variable options.
		@param value The value to be assigned to the variable.
		@param params Parameters of the variable to setup.
	***/
	parse_Symbol setupVariable(unsigned long value, const VariableOptions& params);	
	/***
		Setup the variable according to the variable options.
		@param value The value to be assigned to the variable.
		@param params Parameters of the variable to setup.
	***/
	parse_Symbol setupVariable(float value, const VariableOptions& params);
	/***
		Setup the variable according to the variable options.
		@param value The value to be assigned to the variable.
		@param params Parameters of the variable to setup.
	***/
	parse_Symbol setupVariable(double value, const VariableOptions& params);
	/***
		Setup the variable according to the variable options.
		@param value The value to be assigned to the variable.
		@param params Parameters of the variable to setup.
	***/
	parse_Symbol setupVariable(const std::string& value, const VariableOptions& params);
	/***
		Setup the variable according to the variable options.
		@param value The value to be assigned to the variable.
		@param params Parameters of the variable to setup.
	***/
	parse_Symbol setupVariable(const char* value, const VariableOptions& params);
	/***
		Setup the variable according to the variable options.
		@param value The value to be assigned to the variable.
		@param params Parameters of the variable to setup.
	***/
	parse_Symbol setupVariable(parse_Expr value, const VariableOptions& params);		

	/***
		Enter new frame
		@param frameId The id of the frame.
		@param framesize The length of the storage area for frame variables.
		@param notRecycleable Set to true to disallow frame recycling.
		@return New frame.
	***/
	parse_Frame enterNewFrame(int frameId, int framesize, bool notRecycleable);
	/***
		Enter given frame.
		@param frame Frame to enter, not null.
	***/
	void enterFrame(parse_Frame frame);
	/***
		Exit current frame.
		@return Previous frame, not null.
	***/
	parse_Frame exitFrame();
	/***
		Exit current frame to given frame.
		@param frame Frame to exit to.
	***/
	void exitFrame(parse_Frame frame);
	/***
		Return a frame from the recycle bin if possible, else return a new frame.
		@return Frame, not null.
	***/
	parse_Frame recycledFrame(parse_Frame outerFrame, int frameId, int frameSize);
	/***
		Recycle the given frame.
	***/
	void recycleFrame(parse_Frame frame);
	/***
		Return the current recursion limit.
	***/
	int getRecursionLimit() { return m_RecursionLimit; }
	/***
		Set the current recursion limit.
	***/
	void setRecursionLimit(int recursionLimit) { assert(recursionLimit); m_RecursionLimit = recursionLimit; }
	/***
		Get current recursion depth
	***/
	int getRecursionDepth() { return m_RecursionDepth; }
	/***
		Set current recursion depth
	***/
	void setRecursionDepth(int recursionDepth) { m_RecursionDepth = recursionDepthp; }
	/***
		Increment recursion depth
	***/
	void incrementRecursionDepth() { m_RecursionDepth++; }
	/***
		Decrement recursion depth
	***/
	void decrementRecursionDepth() { m_RecursionDepth--; }
	/***
		Create new error.
		@param position position in code, not null.  Use stdiop0_dummyPosition for unused.
		@param message Message string, not null.
		@param value expr associated with the message, not null.  Use parse_nullE for unused.
		@param frame Frame that triggered the error, not null.  Use expr_dummyFrame for unused.
		@return new parse error, not null.
	***/
	parse_Error createNewError(stdiop0_Position position, M2_string message, parse_Expr value, parse_Frame frame);
protected:
	/***
		From an error, deduce the correct exit code and attempt to exit.
		@param err An error, not null.  
	***/
	void exit(parse_Error err);
	/***
		Create a new file with line numbers for tokenizing the given string.
		@param name Name of the file.
		@param contents Contents of the file as a string.
		@return Not null.
	***/
	parse_TokenFile stringTokenFile(M2_string name,M2_string contents);
	/***
		Evaluate the given file with the given local directory closure
		@param file File to evaluate.
		@param printout ???
		@param dc Dictionary Closure to consider as top level local dictionary.
		@param returnLastValue ???
		@param stopIfBreakReturnContinue ???
		@param returnIfError ???
		@return Expr, possibly an error.
	***/
	parse_Expr readeval4(parse_TokenFile file,bool printout,parse_Dictionary dictionary,bool returnLastvalue,bool stopIfBreakReturnContinue,bool returnIfError);
	/***
		True if the binding process had an error, false otherwise.
	***/
	bool m_BindingHadError;
	/***
		Lookup count increment for binding process.
	***/
	int m_BindingLookupCountIncrement;
	/***
		Current recursion depth
	***/
	int m_RecursionDepth;
	/***
		Current recursion limit.
	***/
	int m_RecursionLimit;
	/***
		Length of list for each size in the recycle bin.
	***/
	static const int c_RecycleBinListLength = 25;
	/***
		Length of recycle bin.
	***/
	static const int c_RecycleBinLength = 25;
	/***
		Recycle bin for frames;
	***/
	parse_Frame m_RecycleBin[c_RecycleBinLength];
	/***
		Length of each list for the recycle bin.
	***/
	size_t m_RecycleBinLength[c_RecycleBinLength];
	/***
		File stream for trace mode.
	 ***/
	std::fstream m_TraceFile;
};

