/***
	This can return expr_dummyCode on impossible code type.
	@param c Code, not null.
	@return Tail code or error.
 ***/
parse_Code evaluate_evalAllButTail(parse_Code c){
	assert(c!=NULL);
	while (true)
	{
		switch (c->type_) {
		case ifCode_typecode:
			{
				parse_ifCode i = reinterpret_cast<parse_ifCode>(c);
				parse_Expr p = evaluate_eval(i->predicate);
				if(M2CPP_IsError(p))
				{
					c = reinterpret_cast<parse_Code>(p);
					continue;
				}
				if (p == parse_True) 
					c = i->thenClause;
				else if(p == parse_False)
					c = i->elseClause;
				else
				{
					parse_Error tmp__12  M2CPP_Interperter::glp()->createNewEerror(common_codePosition(i->predicate),
																				   M2CPP_NewConstString("expected true or false"),
																				   parse_nullE,
																				   expr_dummyFrame);
					return reinterpret_cast<parse_Code>(tmp__12);
				}
				break;
			}
		case semiCode_typecode:
			{
				parse_semiCode v = reinterpret_cast<parse_semiCode>(c);
				parse_CodeSequence w = v->w;
				assert(w->len-1>0);
				parse_Code rc;
				for(size_t i = 0; i < w->len-1; ++i)
				{
					rc = w->array[i+1];
					parse_Expr r = evaluate_eval(w->array[i]);
					if(M2CPP_IsError(r))
						return reinterpret_cast<parse_Code>(r);
				}
				return rc;
			}
		default:
			return c;
		}
}
/***
	This builds error for hitting the recursion limit
	@return Error packet, not null.
***/
parse_Expr evaluate_RecursionLimit()
{
	std::streamstream ss << "recursion limit of " << getRecursionLimit() << " exceeded";
	return buildErrorPacket(ss.str());
}
/***
	This builds an error for hitting the internal recursion limit
	@return Error packet, not null.
***/
parse_Expr evaluate_InternalRecursionLimit()
{
	std::stringstream ss << "internal recursion limit of " << getRecursionLimit() << " exceeded";
	return buildErrorPacket(ss.str());
	return expr_buildErrorPacket(strings_plus_(strings_plus_(str__5, strings1_tostring_2(getRecursionLimit(), str__6));
}
/***
	Store eval(rhs) in x[eval(i)]
	@param x Hash table to store in, not null.
	@param i Code must evaluate to string key for hash table, not null.
	@param rhs Code to evaluate & store in hash table, not null.
	@return eval(rhs) or error.
 ***/
parse_Expr evaluate_storeInHashTable(parse_HashTable x,parse_Code i,parse_Code rhs)
{
	assert(x && i && rhs);
	parse_Expr ival = evaluate_eval(i);
	if(M2CPP_IsError(ival))
		return ival;
	parse_Expr val = evaluate_eval(rhs);
	if(M2CPP_IsError(val))
		return val;
	return hashtables_storeInHashTable_1(x, ival, val);
}
/***
	Store eval(rhs) in the dictionary closure with name eval(i).
	This assumes that the name does not already exist in the dictionary closure.
	@param dc Dictionary closure to store in.
	@param i Code must evaluate to string name of symbol.
	@param rhs Coder to evaluate & store in the dictionary closure.
	@return eval(rhs) or error.
***/
parse_Expr evaluate_storeInDictionary(parse_DictionaryClosure dc,parse_Code i,parse_Code rhs){
	assert(dc && i && rhs);
	ival = evaluate_eval(i);
	if(!M2CPP_IsTypeExact(ival,stringCell_typecode))
		return M2CPP_ErrorOrErrorMesage(ival, "expected a string");
	M2_stringCell newname = reinterpret_cast<M2_stringCell>(ival);
	parse_Expr rhsval = evaluate_eval(rhs);
	if(!M2CPP_IsTypeExact(rhsval,SymbolClosure_typecode))
		return M2CPP_ErrorOrErrorMesage(rhsval,"expected a symbol");
	parse_SymbolClosure sc = reinterpret_cast<parse_SymbolClosure>(rhsval);
	parse_Symbol sym = sc->symbol;
	//we don't need to check this for threads (why?)
	if(!sym->thread && sc->frame != dc->frame)
	{
		assert(0); //this really is an exceptional situation!
		return expr_buildErrorPacket(M2CPP_NewConstString("internal error: expected a symbol with the same dictionary frame"));
	}
	parse_Word newword = lex_makeUniqueWord(newname->v, sym->word->parse);
	parse_Symbol tmp__41 = binding_lookup(newword, dc->dictionary->symboltable);
	if (tmp__41!=NULL)
		return expr_buildErrorPacket(strings_plus_("symbol already exists: ", newname->v));
	binding_insert_1(dc->dictionary->symboltable, newword, sym);
	return rhsval;
}
/***
	Assign m[i]=rhs.
	This will enlarge the vector if needed.
	This depends on X86 Memory ordering to enforce consistency.
	@param m Vector, not null.
	@param i Code that evaluates to small ZZcell, not null.
	@param rhs Code that evaluates to right hand side.
 ***/
static parse_Expr assignvector(parse_List m,parse_Code i,parse_Code rhs){
	assert(m && i && rhs);
	parse_Sequence x = m->v;
	parse_Expr ival = evaluate_eval(i);
	assert(ival!=NULL);
	if(!M2CPP_IsTypeExact(ival,ZZcell_typecode))
		return M2CPP_ErrorOrErrorMesage(ival,"index not an integer");
	gmp_ZZcell j = reinterpret_cast<gmp_ZZcell>(ival_2);
	if (!gmp_isInt_1(j))
		return M2CPP_PrintErrorMessageE(i,"expected small integer");
	int k = gmp_toInt_1(j);
	//do bounds checking.
	if (k < 0)
	{
		k = k + x->len;
		if (k < 0)
		{
			M2_string str__11 = M2CPP_NewConstString("negative subscript out of bounds 0 .. ");
			return common_printErrorMessageE(i, strings_plus_(str__11, strings1_tostring_2((x->len - 1))));
		}
	}
	parse_Expr val = evaluate_eval(rhs);
	if(M2CPP_IsError(val->type_))
		return val;
	if (k >= x->len)
	{
		int newlen = k + 1;
		parse_Sequence tmp__49 = M2CPP_ResizeSequence(x,newlen);
		x_1->array[k] = val;
		//strict ordering here required.
		AO_compiler_barrier();
		m->v = x = tmp__49;
	}
	else
		x_1->array[k] = val;
	return val;
}



/***
	f[eval(KEY)]=eval(CONTENT)
	@param f Database to store in
	@param KEY Code that must evaluate to stringCell that is the key for the database.
	@param CONTENT Code that evaluates to the data being stored.
	@return eval(CONTENT) or error.
 ***/
static parse_Expr dbmstore(parse_Database f,parse_Code KEY,parse_Code CONTENT)
{
	assert(f && KEY && CONTENT);
	parse_Expr Key = evaluate_eval(KEY);
	if(M2CPP_IsError(Key))
		return Key;
	else if(M2CPP_Type(e)!=stringCell_typecode)
		return common_printErrorMessageE(KEY, M2CPP_NewConstString("expected a string"));
	M2_stringCell key = reinterpret_cast<M2_stringCell>(Key);
	parse_Expr Content = evaluate_eval(CONTENT);
	if(M2CPP_IsError(Content))
		return Content;
	else if(stringCell_typecode!=M2CPP_Type(Content))
	{
		if(M2CPP_Type(Content)==Nothing_typecode)
			return expr_buildErrorPacket(M2CPP_NewConstString("storing null database record, use 'remove' to remove records"));
		else
			return common_printErrorMessageE(CONTENT, M2CPP_NewConstString("expected a string"));
	}
	parse_Expr content = reinterpret_cast<M2_stringCell>(Content);
	return common_dbmstore(f, key->v, content->v);
}
/***
	????
	@param lhsarray ???
	@param lhsindex ???
	@param rhs ???
	@return ????
 ***/
static parse_Expr assignelemfun(parse_Code lhsarray,parse_Code lhsindex,parse_Code rhs)
{
	assert(lhsarray && lhsindex && rhs);
	parse_Expr x = evaluate_eval(lhsarray);
	if(M2CPP_IsError(x))
		return x;
	switch (x->type_) {
	case List_typecode:
		{
			parse_List x_3 = reinterpret_cast<parse_List>(x);
			if (x->Mutable) 
				return assignvector(x_3, lhsindex, rhs);
			else
				return expr_buildErrorPacket(M2CPP_NewConstString("assignment attempted to element of immutable list"));
		}
	case Sequence_typecode:
		return expr_buildErrorPacket("assignment attempted to element of sequence");
	case HashTable_typecode:
		{
			parse_HashTable x_5 = reinterpret_cast<parse_HashTable>(x);
			return evaluate_storeInHashTable(x_5, lhsindex, rhs);
		}
	case Database_typecode:
		{
			parse_Database x_6 = reinterpret_cast<parse_Database>(x);
			return dbmstore(x_6, lhsindex, rhs);
		}
	case DictionaryClosure_typecode:
		{
			parse_DictionaryClosure dc_1 = reinterpret_cast<parse_DictionaryClosure>(x_2);
			if (dc_1->dictionary->Protected)
				return common_printErrorMessageE(lhsarray, "attempted to create symbol in protected dictionary");
			else
				return evaluate_storeInDictionary(dc_1, lhsindex, rhs);
		}
	default:
		return M2CPP_PrintErrorMessageE(lhsarray,"exepected a list, hash table, database, or dictionary");
	};
}
/***
	@param x ???
	@param i ???
	@param rhs ???
	@return ????
 ***/
static parse_Expr assignquotedobject(parse_HashTable x,parse_Code i,parse_Code rhs)
{
	assert(x && i && rhs);
	if(!M2CPP_IsTypeExact(i,globalSymbolClosureCode_typecode))
		return M2CPP_PrintErrorMessageE(i,"'.' expected right hand argument to be a symbol");
	parse_globalSymbolClosureCode c_1 = reinterpret_cast<parse_globalSymbolClosureCode>(i);
	parse_SymbolClosure ival = reinterpret_cast<parse_SymbolClosure>(GC_MALLOC(sizeof(struct parse_SymbolClosure_struct)));
	ival->type_ = SymbolClosure_typecode;
	ival->frame = expr_globalFrame;
	ival->symbol = c_1->symbol;
	parse_Expr val_2 = evaluate_eval(rhs);
	if(M2CPP_IsError(val_2))
		return val_2;
	return hashtables_storeInHashTable_1(x, ival, val_2);
}
/***
	????
	@param lhsarray ???
	@param lhsindex ???
	@param rhs ???
***/
static parse_Expr assignquotedelemfun(parse_Code lhsarray,parse_Code lhsindex,parse_Code rhs)
{
	assert(lhsarray && lhsindex && rhs);
	parse_Expr x = evaluate_eval(lhsarray);
	if(!M2CPP_IsTypeExact(x,HashTable_typecode))
		return M2CPP_PrintErrorMessageE(lhsarray,"'.' expected left hand side to be a hash table");
	parse_HashTable x_8 = reinterpret_cast<parse_HashTable>(x);
	return assignquotedobject(x_8, lhsindex, rhs);
}
/***
	Evaluate while do code
	@param c While do code, not null.
	@return Result or error.
***/
static parse_Expr evalWhileDoCode(parse_whileDoCode c)
{
	assert(c!=NULL);
	while(true)
	{
		parse_Expr p_1 = evaluate_eval(c->predicate);
		if(M2CPP_IsError(p_1))
		{
			parse_Error err_1 = reinterpret_cast<parse_Error>(p_1);
			//When breaking out of a loop we pass the result in a parse_Error
			if (err_1->message == tokens_breakMessage)
				if (err_1->value == parse_dummyExpr) 
					return parse_nullE;
				else
					return err_1->value;
			else
				return p_1;
		}
		if (p_1 == parse_True)
		{
			parse_Expr b = evaluate_eval(c->doClause);
			if(M2CPP_IsError(b))
			{
				parse_Error err_2 = reinterpret_cast<parse_Error>(b);
				//we pass continue as an error
				if (err_2->message == tokens_continueMessage) { }
				else
					//we pass break as an error.
					if (err_2->message == tokens_breakMessage)
						if (err_2->value == parse_dummyExpr)
							return parse_nullE;
						else
							return err_2->value;
					else
						//actual error
						return b;
			}
		}
		else
		{
			if (p_1 == parse_False)
				break;
			else
				return M2CPP_PrintErrorMessageE(c.predicate,"expected true or false");
		}
	}
	return parse_nullE;
}
/***
	Evaluate a while list code block that has an optional do block.
	@param co Code of type whileListCode or whileListDoCode, not null.
	@return list or error, not null.
***/
static parse_Expr evalWhileListOptDo(parse_Code co)
{
	assert(co!=NULL);
	assert(co->type_ == whileListDoCode_typecode || co->type_ == whileListCode_typecode);
	//note this could really be a whileDoListCode so don't use position pointer.
	parse_whileListCode c = reinterpret_cast<parse_whileListCode>(co);
	int n = 1;
	parse_Sequence r = M2CPP_NewSequence(n,parse_nullE);
	int i = 0;
	while(true)
	{
		parse_Expr p = evaluate_eval(c->predicate);
		if(M2CPP_IsError(p))
		{
			parse_Error err_3 = reinterpret_cast<parse_Error>(p);
			//break message is an error
			if (err_3->message == tokens_breakMessage)
			{
				if (err_3->value == parse_dummyExpr)
					return parse_nullE;
				else
					return err_3->value;
			}
			else
				//actual error
				return p_2;
		}
		if (p_2 == parse_True)
		{
			bool useb = true;
			parse_Expr b_1 = evaluate_eval(c->listClause);
			if(M2CPP_IsError(b_1))
			{
				parse_Error err_4 = reinterpret_cast<parse_Error>(b_1);
				if (err_4->message == tokens_continueMessage) 
					useb = false;
				else if (err_4->message == tokens_continueMessageWithArg)
					b_1 = err_4->value;
				else if (err_4->message == tokens_breakMessage)
				{
					if (err_4->value == parse_dummyExpr)
						return M2CPP_NewList(r,i);
					else
						return err_4->value;
				}
				else
					//actual error
					return b_1;
			}
			if (useb) 
			{
				if (i == n) 
				{
					M2CPP_EnlargeSequence(r);
					n = r->len;
				}
				r->array[i] = b_1;
				++i;
			}
			//if this is a do block, do the block.
			if(co->_type==whileListDoCode_typecode)
			{
				parse_Expr d = evaluate_eval(reinterpret_cast<parse_whileListDoCode>(co));
				if(M2CPP_IsError(d))
				{
					parse_Error err_4 = reinterpret_cast<parse_Error>(d);
					if (err_4->message == tokens_breakMessage)
					{
						if (err_4->value == parse_dummyExpr)
							return M2CPP_NewList(r,i);
						else
							return err_4->value;
					}
					else
						//actual error
						return d;
				}
			}
		}
		else if (p_2 == parse_False)
			break;
		else
			return M2CPP_PrintErrorMessageE(c->predicate,"expected true or false");
	}
	return M2CPP_NewList(r,i);
}
/***
	Evaluate a while list code block.
	@param c Code, not null.
	@return list or error, not null.
***/
static parse_Expr evalWhileListCode(parse_whileListCode c)
{
	return evalWhileListOptDo(reinterpret_cast<parse_Code>(c));
}
/***
	Evaluate a while list code block.
	@param c Code, not null.
	@return list or error, not null.
***/
static parse_Expr evalWhileListDoCode(parse_whileListDoCode c)
{
	return evalWhileListOptDo(reinterpret_cast<parse_Code>(c));
}
static parse_Expr evalForCode(parse_forCode c)
{	
	parse_Expr inValue, fromValue, toValue;
	parse_Sequence r;
	if (c->listClause == expr_dummyCode)
		r = expr_emptySequence;
	else
		r = M2CPP_NewSequence(1,parse_nullE);
	int i = 0; //index in r
	int j = 0; //value of loop variable if its an integer loop else index in list.
	parse_Sequence w = expr_emptySequence; // the list x when its for i in w....
	int n = 0;//upper bound on j if there is a to clause.
	bool listLoop = false;
	bool toLimit = false;
	if (c->inClause != expr_dummyCode)
	{
		listLoop = true;
	    invalue = evaluate_eval(c->inClause);
		M2CPP_PerformAssertions(invalue);
		switch (invalue->type_) {
		case Error_typecode:
			return invalue;
		case Sequence_typecode:
			w = reinterpret_cast<parse_Sequence>(invalue);
			break;
		case List_typecode:
			w = reinterpret_cast<parse_List>(invalue)->v;
			break;
		default:
			return M2CPP_PrintErrorMessageE(c->inClause, "expected a list or sequence");
		};
	}
	else
	{
		if (c->fromClause != expr_dummyCode)
		{
			fromvalue = evaluate_eval(c->fromClause);
			if(!M2CPP_IsTypeExact(fromvalue,ZZcell_typecode))
				return M2CPP_ErrorOrErrorMesage(c->fromClause,"expected an integer");
			gmp_ZZcell f_1 = reinterpret_cast<gmp_ZZcell>(fromvalue);
			if (gmp_isInt_1(f_1)) 
				j = gmp_toInt_1(f_1);
			else
				return M2CPP_PrintErrorMessageE(c->fromClause,"expected a small integer");
		}
		if (c->toClause != expr_dummyCode)
		{
			toLimit = true;
			tovalue = evaluate_eval(c->toClause);
			if(!M2CPP_IsTypeExact(tovalue,ZZcell_typecode))
				return M2CPP_ErrorOrErrorMesage(c->toClause,"expected an integer");
			gmp_ZZcell f_2 = reinterpret_cast<gmp_ZZcell>(tovalue);
			if (gmp_isInt_1(f_2)) 
				n = gmp_toInt_1(f_2);
			else
				return M2CPP_PrintErrorMessageE(c->toClause,"expected a small integer");
		}
	}
	//set local frame
	M2CPP_InterperterLocal* m2il = M2CPP_Interperter::gsp()->glp();
	parse_frame nlf = m2il->enterNewFrame(c->frameId,c->framesize,false);
	while(true)
	{
		if(toLimit && j>n)
			break;
		if(listLoop && j >= w->len)
			break;
		if(listLoop)
			nlf->array[0] = w->array[j];
		else
			nlf->array[0] = M2CPP_ZZCell(j);
	}
	++j;
	if (c->whenClause != expr_dummyCode)
	{
		parse_Expr p_4 = evaluate_eval(c->whenClause);
		if(M2CPP_IsError(p_4))
		{
			parse_Error err_8 = reinterpret_cast<parse_Error>(p_4);
			m2il->exitFrame();
			if (err_8->message == tokens_breakMessage)
			{
				if (err_8->value == parse_dummyExpr)
				{
					if (c->listClause == expr_dummyCode)
						return parse_nullE;
					else
						return M2CPP_NewList(r,i);
				}
				else
					return err_8->value;
			}
			else
				return p_4;
		}
		if (p_4 == parse_False)
			break;
		else if(p_4!=Parse_True)
		{
			m2il->exitFrame();
			return M2CPP_PrintErrorMessageE(c->whenClause,"expected true or false");
		}
	}
	if (c->listClause != expr_dummyCode)
	{
		parse_Expr b_3 = evaluate_eval(c->listClause);
		bool useb_2 = true;
		if(M2CPP_IsError(b_3))
		{
			parse_Error err_9 = reinterpret_cast<parse_Error>(b_3);
			if (err_9->message == tokens_continueMessage)
				useb_2 = false;
			else if(err_9->message == tokens_continueMessageWithArg)
				b_3 = err_9->value;
			else
			{
				if (err_9->message == tokens_breakMessage)
				{
					if (err_9->value == parse_dummyExpr)
					{
						b_3 = M2CPP_NewList(r,i);
					}
					else
					{
						tmp__204 = err_9->value;
					}
					b_3 = tmp__204;
				}
				m2il->exitFrame();
				return b_3;
			}
		}
		if (useb_2) 
		{
			if (i == r->len)
				r = M2CPP_EnlargeSequence(r);
			r_3->array[i] = b_3;
			++i;
		}
	}
	if (c->doClause != expr_dummyCode)
	{
		parse_Expr b_4 = evaluate_eval(c->doClause);
		if(M2CPP_IsError(b_4))
		{
			parse_Error err_10 = reinterpret_cast<parse_Error>(b_4);
			if (err_10->message != tokens_continueMessage)
			{
				m2il->exitFrame();
				if (err_10->message == tokens_breakMessage)
				{
					if (err_10->value == parse_dummyExpr)
					{
						if (c->listClause == expr_dummyCode)
							return parse_nullE;
						else
							return M2CPP_NewList(r,i);
					}
					else
						return err_10->value;
				}
				else
					return b_4;
			}
		}
	}
	m2il->exitFrame();
	if(c->listClause == expr_dummyCode)
		return parse_nullE;
	else
		return M2CPP_NewList(r,i);
}
parse_Sequence evaluate_evalSequence(parse_CodeSequence v)
{
	M2CPP_InterperterLocal* m2il = M2CPP_Interperter::gsp()->glp();
	m2il->evalSequenceHadError() = false;
	if(v->len == 0)
		return expr_emptySequence;
	parse_Sequence seq = M2CPP_NewSequence(v->len);
	for(size_t i = 0; i < v->len; ++i)
	{
		parse_Expr e = evaluate_eval(v->array[i]);
		if(M2CPP_IsError(e))
		{
			m2il->evalSequenceHadError() = true;
			m2il->evalSequenceErrorMessage() = e;
			return expr_emptySequence;
		}
		seq->array[i] = e;
	}
	return seq;
}
/***
	Apply function closure to sequence.
	@param c Function closure, not null./
	@param v Sequence, not null.
	@return Expr or error, not null.
***/
parse_Expr evaluate_applyFCS(parse_FunctionClosure c,parse_Sequence v)
{
	M2CPP_InterperterLocal* m2il = M2CPP_Interperter::gsp()->glp();
	if(m2il->recursionDepth() > m2il->recursionLimit())
		return evaluate_RecursionLimit();
	
	parse_FunctionCode model = c->model;
	parse_FunctionDescription desc = model->desc;
	parse_Frame previousFrame = c->frame;
	int framesize = desc->framesize;
	if (desc->restargs) 
	{
		parse_Frame f = m2il->recycledFrame(previousFrame,desc->frameID,framesize);
		f->values->array[0]=v;
		parse_Frame saveLocalFrame = m2il->localFrame();
		m2il->enterFrame(f);
		m2il->incrementRecursionDepth();
		parse_Expr ret = evaluate_eval(model->body);
		m2il->decrementRecursionDepth();
		m2il->exitFrame(saveLocalFrame);
		m2il->recycleFrame(f);
		if(M2CPP_IsError(ret))
			return common_returnFromFunction(ret);
		return ret;
	}
	else if(desc->numparms != v->len)
		return common_WrongNumArgs_1(model->arrow, desc->numparms, v->len);
	else if (framesize == 0)
		{
			parse_Frame saveLocalFrame = localFrame();
			m2il->enterFrame(previousFrame);
			m2il->incrementRecursionDepth();
			parse_Expr ret = eval(model->body);
			m2il->decrementRecursionDepth();
			m2il->exitFrame(saveLocalFrame);
			if(M2CPP_IsError(ret))
				return common_returnFromFunction(ret);
			return ret;
		}
	else
	{
		parse_Frame f = m2il->recycledFrame(previousFrame,desc->frameID,framesize);
		for(size_t i = 0; i < v->len; ++i)
			f->values->array[i]=v->array[i];
		parse_Frame saveLocalFrame = m2il->localFrame();
		m2il->enterFrame(f);
		m2il->incrementRecursionDepth();
		parse_Expr ret = evaluate_eval(model->body);
		m2il->decrementRecursionDepth();
		m2il->exitFrame(saveLocalFrame);
		m2il->recycleFrame(f);
		if(M2CPP_IsError(ret))
			return common_returnFromFunction(ret);
		return ret;
	}
}
/***
	???
	@param model ???
	@return Error message, not null.
****/
static parse_Expr wrongModel1(parse_functionCode model)
{
	M2_string str__32 = M2CPP_NewConstString("expected ");
	M2_string str__33 = M2CPP_NewConstString(" argument");
	M2_string str__36 = M2CPP_NewConstString(" but got 1");
	M2_string tmp__305;
	if (model->desc->numparms == 1)
		tmp__305 = M2CPP_NewConstString("");
	else
		tmp__305 = M2CPP_NewConstString("s");
	return common_printErrorMessageE_2(model->arrow, strings_plus_(strings_plus_(strings_plus_(strings_plus_(str__32, strings1_tostring_2(model->desc->numparms)), str__33), tmp__305), str__36));
}
/***
	Apply function closure to code.
	@param fc Function closure, not null.
	@param ec Code, not null.
***/
parse_Expr evaluate_applyFCC(parse_FunctionClosure fc,parse_Code ec)
{
	assert(fc && ec);
	M2CPP_InterperterLocal* m2il = M2CPP_Interperter::gsp()->glp();
	if(m2il->recursionDepth() > m2il->recursionLimit())
		return evaluate_RecursionLimit();

	parse_Frame previousFrame = fc->frame;
	parse_FunctionCode model = c->model;
	parse_FunctionDescription desc = model->desc;
	int framesize = desc->framesize;
	if(M2CPP_Type(ec) == sequenceCode_typecode)
		return evaluate_applyFCCS(fc, reinterpret_cast<parse_sequenceCode>(ec)->x);
	m2il->incrementRecursionDepth();
	parse_Expr e = evaluate_eval(ec);
	m2il->decrementRecursionDepth();
	if(M2CPP_IsError(e))
		return e;
	if(M2CPP_Type(e) == Sequence_typecode)
		return evaluate_applyFCS(fc, v);
	if(desc->numparms!=1)
		return wrongModel1(model);
	parse_Frame saveLocalFrame = m2il->localFrame();
	parse_Frame f = m2il->recycledFrame(previousFrame,desc->frameId,framesize);
	f->values->array[0] = e;
	parse_Expr ret = parse_nullE;
	while(true)
	{
		m2il->enterFrame(f);
		m2il->incrementRecursionDepth();
		parse_Code tailCode = evalAllButTail(model->body);
		m2il->decrementRecursionDepth();
		assert(tailcode);
		switch(M2CPP_Type(tailcode))
		{
		case Error_typecode:
			ret = reinterpret_cast<parse_Expr>(tailCode);
			break;
		case adjacentCode_typecode:
			{
				parse_AdjacentCode b = reinterpret_cast<parse_AdjacentCode>(tailCode);
				parse_Expr left = evaluate_eval(b_5->lhs);
				switch (M2CPP_Type(left)) {
				case FunctionClosure_typecode:
					{
						parse_FunctionClosure c2 = reinterpret_cast<parse_FunctionClosure>(left);
						parse_Code rhs = b->rhs;
						assert(rhs);
						if(M2CPP_IsTypeExact(rhs,sequenceCode_typecode))
						{
							parse_sequenceCode cs = reinterpret_cast<parse_sequenceCode>(rhs);
							m2il->incrementRecursionDepth();
							ret = evaluate_applyFCCS(c2, cs->x);
							m2il->decrementRecursionDepth();
						}
						else
						{
							m2il->incrementRecursionDepth();
							parse_Expr e = evaluate_eval(rhs);
							m2il->decrementRecursionDepth();
							assert(e);
							switch (M2CPP_Type(e)) {
							case Error_typecode:
								ret = e;
								break;
							case Sequence_typecode:
								{
									parse_Expr v = reinterpret_cast<parse_Sequence>(e);
									m2il->incrementRecursionDepth();
									ret = evaluate_applyFCS(c2, v);
									m2il->decrementRecursionDepth();
									break;
								}
							default:
								{
									parse_Frame previousFrame = c2->frame;
									parse_FunctionCode model = c2->model;
									parse_FunctionDescription desc = model->desc;
									if (desc->numparms != 1) 
										return wrongModel1(model);
								    if (desc->framesize > framesize || f->notrecyclable);
									{
										framesize = desc->framesize;
										f = m2il->recycledFrame(previousFrame,desc->frameID,framesize);
										f->values->array[0] = e;
									}
									else
									{
										for(size_t i = 0; i < f->values->len; ++i)
											f->values->array[i] = parse_nullE;
										f->outerFrame = previousFrame;
										f->frameID = desc->frameId;
										f->values->array[0] = e;
									}
								}
							}
						}
					}
				case CompiledFunction_typecode:
					{
						parse_CompiledFunction ff = reinterpret_cast<parse_CompiledFunction>(left);
						m2il->incrementRecursionDepth();
						parse_Expr z = evaluate_eval(b->rhs);
						m2il->decrementRecursionDepth();
						if(M2CPP_IsError(z))
						{
							ret = common_returnFromFunction(z);
							break;//not a real break.
						}
						incrementRecursionDepth();
						ret = ff->fn(z);
						decrementRecursionDepth();
						break;
					}
					case CompiledFunctionClosure_typecode:;
						ff_1 = ((parse_CompiledFunctionClosure)left);
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
						z_1 = evaluate_eval(b_5->rhs);
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
						if (z_1 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
						switch (z_1->type_) {;
						case Error_typecode:;
							ret_5 = z_1;
							goto L310_;
							break;
						default:
							checkTypeValidity(z_1->type_,__FILE__,__LINE__);
							(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
							ret_5 = ff_1->fn(z_1, ff_1->env);
							(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
							goto L310_;
							break;
						};
						break;
					case SpecialExpr_typecode:;
						s = ((parse_SpecialExpr)left);
						z_2 = evaluate_eval(b_5->rhs);
						if (z_2 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
						switch (z_2->type_) {;
						case Error_typecode:;
							ret_5 = z_2;
							goto L310_;
							break;
						default:
							checkTypeValidity(z_2->type_,__FILE__,__LINE__);
							(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
							ret_5 = evaluate_applyEE(s->e, z_2);
							(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
							goto L310_;
							break;
						};
						break;
					case Error_typecode:;
						ret_5 = left;
						goto L310_;
						break;
					default:
						checkTypeValidity(left->type_,__FILE__,__LINE__);
						ret_5 = evaluate_binarymethod_1(left, b_5->rhs, binding_AdjacentS);
						goto L310_;
						break;
					};
					break;
				default:
					checkTypeValidity(tailCode->type_,__FILE__,__LINE__);
					(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
					ret_5 = evaluate_eval(tailCode);
					(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
					goto L310_;
					break;
				};
				goto L308_;
			L310_:;
				(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame_5;
				tmp__334 = (!(f_7->notrecyclable));
				if (tmp__334) {
					tmp__334 = (framesize_1 < (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len);
				}
				if (tmp__334) 
				{
					tmp__335 = 1;
					tmp__336 = f_7->values;
					if ((tmp__336->len == 0)) goto L329_;
					if ((tmp__335 > 0)) tmp__337 = 0; else tmp__337 = (tmp__336->len - 1);
				L330_:;
					tmp__336->array[tmp__337] = parse_nullE;
					tmp__337 = (tmp__337 + tmp__335);
					if ((((tmp__335 > 0) && (tmp__337 < tmp__336->len)) || ((tmp__335 < 0) && (tmp__337 >= 0)))) goto L330_;
				L329_:;
					tmp__338 = framesize_1;
					if (tmp__338 < 0 || tmp__338 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__338,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",698,51);
					f_7->outerFrame = (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__338];
					f_7->frameID = (- 2);
					tmp__339 = framesize_1;
					if (tmp__339 < 0 || tmp__339 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__339,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",700,36);
					(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__339] = f_7;
				}
				if (ret_5 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
				switch (ret_5->type_) {;
				case Error_typecode:;
					err_16 = ((parse_Error)ret_5);
					tmp__340 = common_returnFromFunction(ret_5);
					break;
				default:
					checkTypeValidity(ret_5->type_,__FILE__,__LINE__);
					tmp__340 = ret_5;
					break;
				};
				tmp__341 = tmp__340;
			}
			else
			{
				tmp__343 = 0;
				tmp__345 = framesize_1;
				if (0 > tmp__345) fatalarraylen(tmp__345,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",706,42);
				tmp__344 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (tmp__345 - 1)*sizeof(parse_Expr));
				tmp__344->type_ = 6;
				tmp__344->len = tmp__345;
				if ((tmp__345 == 0)) goto L332_;
			L331_:;
				tmp__344->array[tmp__343] = e_7;
				if (((++ tmp__343) < tmp__345)) goto L333_;
				goto L332_;
			L333_:;
			L334_:;
				tmp__344->array[tmp__343] = parse_nullE;
				if (((++ tmp__343) < tmp__345)) goto L337_;
				goto L332_;
			L337_:;
				goto L334_;
				goto L331_;
			L332_:;
				tmp__342 = (parse_Frame) GC_MALLOC(sizeof(struct parse_Frame_struct));
				tmp__342->outerFrame = previousFrame_1;
				tmp__342->frameID = desc_1->frameID;
				tmp__342->valuesUsed = framesize_1;
				tmp__342->notrecyclable = 0;
				tmp__342->values = tmp__344;
				f_8 = tmp__342;
				(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = f_8;
				(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
				ret_6 = evaluate_eval(model_1->body);
				(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
				(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame_5;
				if (ret_6 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
				switch (ret_6->type_) {;
				case Error_typecode:;
					err_17 = ((parse_Error)ret_6);
					tmp__346 = common_returnFromFunction(ret_6);
					break;
				default:
					checkTypeValidity(ret_6->type_,__FILE__,__LINE__);
					tmp__346 = ret_6;
					break;
				};
				tmp__341 = tmp__346;
			}
			tmp__309 = tmp__341;
			break;
		};
		tmp__307 = tmp__309;
		break;
	};
	return tmp__307;
}
parse_Expr evaluate_applyFCE(parse_FunctionClosure fc,parse_Expr e){
	parse_Frame previousFrame_2;
	parse_functionCode model_2;
	parse_functionDescription desc_2;
	int framesize_2;
	parse_Frame saveLocalFrame_6;
	parse_Frame f_9;
	parse_Expr ret_7;
	parse_Error err_18;
	parse_Frame f_10;
	parse_Expr ret_8;
	parse_Error err_19;
	parse_Expr tmp__347;
	parse_Expr tmp__348;
	int tmp__349;
	parse_Frame tmp__350;
	int tmp__351;
	parse_Sequence tmp__352;
	int tmp__353;
	int tmp__354;
	int tmp__355;
	int tmp__356;
	parse_Sequence tmp__357;
	int tmp__358;
	int tmp__359;
	int tmp__360;
	parse_Expr tmp__361;
	parse_Expr tmp__362;
	parse_Frame tmp__363;
	int tmp__364;
	parse_Sequence tmp__365;
	int tmp__366;
	parse_Expr tmp__367;
	if (((*((int*)TS_Get_Local(expr_recursionDepth_id))) > (*((int*)TS_Get_Local(expr_recursionLimit_id))))) 
	{
		tmp__347 = evaluate_RecursionLimit();
		return tmp__347;
	}
	previousFrame_2 = fc->frame;
	model_2 = fc->model;
	desc_2 = model_2->desc;
	framesize_2 = desc_2->framesize;
	if ((desc_2->numparms != 1)) 
	{
		tmp__348 = wrongModel1(model_2);
		return tmp__348;
	}
	saveLocalFrame_6 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
	if ((framesize_2 < (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len)) 
	{
		tmp__349 = framesize_2;
		if (tmp__349 < 0 || tmp__349 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__349,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",726,26);
		f_9 = (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__349];
		if ((f_9 == f_9->outerFrame)) 
		{
			tmp__351 = 0;
			tmp__353 = framesize_2;
			if (0 > tmp__353) fatalarraylen(tmp__353,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",728,86);
			tmp__352 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (tmp__353 - 1)*sizeof(parse_Expr));
			tmp__352->type_ = 6;
			tmp__352->len = tmp__353;
			if ((tmp__353 == 0)) goto L343_;
		L342_:;
			tmp__352->array[tmp__351] = e;
			if (((++ tmp__351) < tmp__353)) goto L344_;
			goto L343_;
		L344_:;
		L345_:;
			tmp__352->array[tmp__351] = parse_nullE;
			if (((++ tmp__351) < tmp__353)) goto L348_;
			goto L343_;
		L348_:;
			goto L345_;
			goto L342_;
		L343_:;
			tmp__350 = (parse_Frame) GC_MALLOC(sizeof(struct parse_Frame_struct));
			tmp__350->outerFrame = previousFrame_2;
			tmp__350->frameID = desc_2->frameID;
			tmp__350->valuesUsed = framesize_2;
			tmp__350->notrecyclable = 0;
			tmp__350->values = tmp__352;
			f_9 = tmp__350;
		}
		else
		{
			tmp__354 = framesize_2;
			if (tmp__354 < 0 || tmp__354 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__354,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",731,26);
			(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__354] = f_9->outerFrame;
			f_9->outerFrame = previousFrame_2;
			f_9->frameID = desc_2->frameID;
			tmp__355 = 0;
			if (tmp__355 < 0 || tmp__355 >= f_9->values->len) fatalarrayindex(tmp__355,f_9->values->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",734,24);
			f_9->values->array[tmp__355] = e;
		}
		(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = f_9;
		(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
		ret_7 = evaluate_eval(model_2->body);
		(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
		(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame_6;
		if ((!(f_9->notrecyclable))) 
		{
			tmp__356 = 1;
			tmp__357 = f_9->values;
			if ((tmp__357->len == 0)) goto L351_;
			if ((tmp__356 > 0)) tmp__358 = 0; else tmp__358 = (tmp__357->len - 1);
		L352_:;
			tmp__357->array[tmp__358] = parse_nullE;
			tmp__358 = (tmp__358 + tmp__356);
			if ((((tmp__356 > 0) && (tmp__358 < tmp__357->len)) || ((tmp__356 < 0) && (tmp__358 >= 0)))) goto L352_;
		L351_:;
			tmp__359 = framesize_2;
			if (tmp__359 < 0 || tmp__359 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__359,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",744,41);
			f_9->outerFrame = (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__359];
			f_9->frameID = (- 2);
			tmp__360 = framesize_2;
			if (tmp__360 < 0 || tmp__360 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__360,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",746,26);
			(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__360] = f_9;
		}
		if (ret_7 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
		switch (ret_7->type_) {;
		case Error_typecode:;
			err_18 = ((parse_Error)ret_7);
			tmp__361 = common_returnFromFunction(ret_7);
			break;
		default:
			checkTypeValidity(ret_7->type_,__FILE__,__LINE__);
			tmp__361 = ret_7;
			break;
		};
		tmp__362 = tmp__361;
	}
	else
	{
		tmp__364 = 0;
		tmp__366 = framesize_2;
		if (0 > tmp__366) fatalarraylen(tmp__366,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",752,32);
		tmp__365 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (tmp__366 - 1)*sizeof(parse_Expr));
		tmp__365->type_ = 6;
		tmp__365->len = tmp__366;
		if ((tmp__366 == 0)) goto L354_;
	L353_:;
		tmp__365->array[tmp__364] = e;
		if (((++ tmp__364) < tmp__366)) goto L355_;
		goto L354_;
	L355_:;
	L356_:;
		tmp__365->array[tmp__364] = parse_nullE;
		if (((++ tmp__364) < tmp__366)) goto L359_;
		goto L354_;
	L359_:;
		goto L356_;
		goto L353_;
	L354_:;
		tmp__363 = (parse_Frame) GC_MALLOC(sizeof(struct parse_Frame_struct));
		tmp__363->outerFrame = previousFrame_2;
		tmp__363->frameID = desc_2->frameID;
		tmp__363->valuesUsed = framesize_2;
		tmp__363->notrecyclable = 0;
		tmp__363->values = tmp__365;
		f_10 = tmp__363;
		(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = f_10;
		(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
		ret_8 = evaluate_eval(model_2->body);
		(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
		(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame_6;
		if (ret_8 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
		switch (ret_8->type_) {;
		case Error_typecode:;
			err_19 = ((parse_Error)ret_8);
			tmp__367 = common_returnFromFunction(ret_8);
			break;
		default:
			checkTypeValidity(ret_8->type_,__FILE__,__LINE__);
			tmp__367 = ret_8;
			break;
		};
		tmp__362 = tmp__367;
	}
	return tmp__362;
}
parse_Expr evaluate_applyFCCS(parse_FunctionClosure c,parse_CodeSequence cs){
	parse_functionCode model_3;
	parse_functionDescription desc_3;
	parse_Sequence v_3;
	parse_Frame previousFrame_3;
	int framesize_3;
	parse_Frame saveLocalFrame_7;
	parse_Expr ret_9;
	parse_Error err_20;
	parse_Frame f_11;
	parse_Frame previousStashedFrame_2;
	char haderror;
	parse_Expr codevalue;
	parse_Expr codevalue_1;
	parse_Frame saveLocalFrame_8;
	parse_Expr ret_10;
	parse_Error err_21;
	parse_Frame saveLocalFrame_9;
	char haderror_1;
	parse_Expr codevalue_2;
	parse_Frame f_12;
	parse_Expr ret_11;
	parse_Error err_22;
	parse_Expr tmp__368;
	parse_Expr tmp__369;
	parse_Expr tmp__370;
	parse_Expr tmp__371;
	parse_Expr tmp__372;
	parse_Expr tmp__373;
	int tmp__374;
	parse_Frame tmp__375;
	int tmp__376;
	parse_Sequence tmp__377;
	int tmp__378;
	int tmp__379;
	parse_CodeSequence tmp__380;
	int tmp__381;
	parse_Expr tmp__382;
	int tmp__383;
	int tmp__384;
	parse_CodeSequence tmp__385;
	int i_6;
	parse_Expr tmp__386;
	int tmp__387;
	parse_Expr tmp__388;
	int tmp__389;
	parse_Sequence tmp__390;
	int tmp__391;
	int tmp__392;
	int tmp__393;
	parse_Expr tmp__394;
	parse_Frame tmp__395;
	int tmp__396;
	parse_Sequence tmp__397;
	int tmp__398;
	int tmp__399;
	parse_CodeSequence tmp__400;
	int tmp__401;
	parse_Expr tmp__402;
	parse_Expr tmp__403;
	model_3 = c->model;
	desc_3 = model_3->desc;
	if (((*((int*)TS_Get_Local(expr_recursionDepth_id))) > (*((int*)TS_Get_Local(expr_recursionLimit_id))))) 
	{
		tmp__368 = evaluate_RecursionLimit();
	}
	else
	{
		if (desc_3->restargs) 
		{
			(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
			v_3 = evaluate_evalSequence(cs);
			(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
			if (evaluate_evalSequenceHadError) 
			{
				tmp__369 = evaluate_evalSequenceErrorMessage;
			}
			else
			{
				tmp__369 = evaluate_applyFCS(c, v_3);
			}
			tmp__370 = tmp__369;
		}
		else
		{
			if ((desc_3->numparms != cs->len)) 
			{
				tmp__371 = common_WrongNumArgs_1(model_3->arrow, desc_3->numparms, cs->len);
			}
			else
			{
				previousFrame_3 = c->frame;
				framesize_3 = desc_3->framesize;
				if ((framesize_3 == 0)) 
				{
					saveLocalFrame_7 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
					(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = previousFrame_3;
					(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
					ret_9 = evaluate_eval(model_3->body);
					(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
					if (ret_9 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
					switch (ret_9->type_) {;
					case Error_typecode:;
						err_20 = ((parse_Error)ret_9);
						tmp__372 = common_returnFromFunction(ret_9);
						ret_9 = tmp__372;
						break;
					default:
						checkTypeValidity(ret_9->type_,__FILE__,__LINE__);
						break;
					};
					(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame_7;
					tmp__373 = ret_9;
				}
				else
				{
					if ((framesize_3 < (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len)) 
					{
						tmp__374 = framesize_3;
						if (tmp__374 < 0 || tmp__374 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__374,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",795,36);
						f_11 = (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__374];
						previousStashedFrame_2 = f_11->outerFrame;
						if ((f_11 == previousStashedFrame_2)) 
						{
							haderror = 0;
							tmp__376 = 0;
							tmp__378 = framesize_3;
							if (0 > tmp__378) fatalarraylen(tmp__378,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",800,47);
							tmp__377 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (tmp__378 - 1)*sizeof(parse_Expr));
							tmp__377->type_ = 6;
							tmp__377->len = tmp__378;
							if ((tmp__378 == 0)) goto L368_;
						L367_:;
							tmp__379 = 1;
							tmp__380 = cs;
							if ((tmp__380->len == 0)) goto L370_;
							if ((tmp__379 > 0)) tmp__381 = 0; else tmp__381 = (tmp__380->len - 1);
						L371_:;
							codevalue = evaluate_eval(tmp__380->array[tmp__381]);
							if (codevalue == 0) invalidNullPointer(__FILE__,__LINE__,-1);
							switch (codevalue->type_) {;
							case Error_typecode:;
								haderror = 1;
								(*((parse_Expr*)TS_Get_Local(errorreturn_id))) = codevalue;
							L372_:;
								tmp__377->array[tmp__376] = parse_nullE;
								if (((++ tmp__376) < tmp__378)) goto L375_;
								goto L368_;
							L375_:;
								goto L372_;
								break;
							default:
								checkTypeValidity(codevalue->type_,__FILE__,__LINE__);
								tmp__377->array[tmp__376] = codevalue;
								if (((++ tmp__376) < tmp__378)) goto L376_;
								goto L368_;
							L376_:;
								break;
							};
							tmp__381 = (tmp__381 + tmp__379);
							if ((((tmp__379 > 0) && (tmp__381 < tmp__380->len)) || ((tmp__379 < 0) && (tmp__381 >= 0)))) goto L371_;
						L370_:;
						L377_:;
							tmp__377->array[tmp__376] = parse_nullE;
							if (((++ tmp__376) < tmp__378)) goto L380_;
							goto L368_;
						L380_:;
							goto L377_;
							goto L367_;
						L368_:;
							tmp__375 = (parse_Frame) GC_MALLOC(sizeof(struct parse_Frame_struct));
							tmp__375->outerFrame = previousFrame_3;
							tmp__375->frameID = desc_3->frameID;
							tmp__375->valuesUsed = framesize_3;
							tmp__375->notrecyclable = 0;
							tmp__375->values = tmp__377;
							f_11 = tmp__375;
							if (haderror) 
							{
								tmp__382 = (*((parse_Expr*)TS_Get_Local(errorreturn_id)));
								return tmp__382;
							}
						}
						else
						{
							tmp__383 = framesize_3;
							if (tmp__383 < 0 || tmp__383 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__383,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",815,36);
							(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__383] = previousStashedFrame_2;
							f_11->outerFrame = previousFrame_3;
							f_11->frameID = desc_3->frameID;
							tmp__384 = 1;
							tmp__385 = cs;
							if ((tmp__385->len == 0)) goto L383_;
							if ((tmp__384 > 0)) i_6 = 0; else i_6 = (tmp__385->len - 1);
						L384_:;
							codevalue_1 = evaluate_eval(tmp__385->array[i_6]);
							if (codevalue_1 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
							switch (codevalue_1->type_) {;
							case Error_typecode:;
								tmp__386 = codevalue_1;
								return tmp__386;
								break;
							default:
								checkTypeValidity(codevalue_1->type_,__FILE__,__LINE__);
								tmp__387 = i_6;
								if (tmp__387 < 0 || tmp__387 >= f_11->values->len) fatalarrayindex(tmp__387,f_11->values->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",822,44);
								f_11->values->array[tmp__387] = codevalue_1;
								break;
							};
							i_6 = (i_6 + tmp__384);
							if ((((tmp__384 > 0) && (i_6 < tmp__385->len)) || ((tmp__384 < 0) && (i_6 >= 0)))) goto L384_;
						L383_:;
						}
						saveLocalFrame_8 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
						(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = f_11;
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
						ret_10 = evaluate_eval(model_3->body);
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
						if (ret_10 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
						switch (ret_10->type_) {;
						case Error_typecode:;
							err_21 = ((parse_Error)ret_10);
							tmp__388 = common_returnFromFunction(ret_10);
							ret_10 = tmp__388;
							break;
						default:
							checkTypeValidity(ret_10->type_,__FILE__,__LINE__);
							break;
						};
						(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame_8;
						if ((!(f_11->notrecyclable))) 
						{
							tmp__389 = 1;
							tmp__390 = f_11->values;
							if ((tmp__390->len == 0)) goto L387_;
							if ((tmp__389 > 0)) tmp__391 = 0; else tmp__391 = (tmp__390->len - 1);
						L388_:;
							tmp__390->array[tmp__391] = parse_nullE;
							tmp__391 = (tmp__391 + tmp__389);
							if ((((tmp__389 > 0) && (tmp__391 < tmp__390->len)) || ((tmp__389 < 0) && (tmp__391 >= 0)))) goto L388_;
						L387_:;
							tmp__392 = framesize_3;
							if (tmp__392 < 0 || tmp__392 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__392,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",835,51);
							f_11->outerFrame = (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__392];
							f_11->frameID = (- 2);
							tmp__393 = framesize_3;
							if (tmp__393 < 0 || tmp__393 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__393,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",837,36);
							(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__393] = f_11;
						}
						tmp__394 = ret_10;
					}
					else
					{
						saveLocalFrame_9 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
						haderror_1 = 0;
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
						tmp__396 = 0;
						tmp__398 = framesize_3;
						if (0 > tmp__398) fatalarraylen(tmp__398,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",846,42);
						tmp__397 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (tmp__398 - 1)*sizeof(parse_Expr));
						tmp__397->type_ = 6;
						tmp__397->len = tmp__398;
						if ((tmp__398 == 0)) goto L390_;
					L389_:;
						tmp__399 = 1;
						tmp__400 = cs;
						if ((tmp__400->len == 0)) goto L392_;
						if ((tmp__399 > 0)) tmp__401 = 0; else tmp__401 = (tmp__400->len - 1);
					L393_:;
						codevalue_2 = evaluate_eval(tmp__400->array[tmp__401]);
						if (codevalue_2 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
						switch (codevalue_2->type_) {;
						case Error_typecode:;
							haderror_1 = 1;
							(*((parse_Expr*)TS_Get_Local(errorreturn_id))) = codevalue_2;
						L394_:;
							tmp__397->array[tmp__396] = parse_nullE;
							if (((++ tmp__396) < tmp__398)) goto L397_;
							goto L390_;
						L397_:;
							goto L394_;
							break;
						default:
							checkTypeValidity(codevalue_2->type_,__FILE__,__LINE__);
							tmp__397->array[tmp__396] = codevalue_2;
							if (((++ tmp__396) < tmp__398)) goto L398_;
							goto L390_;
						L398_:;
							break;
						};
						tmp__401 = (tmp__401 + tmp__399);
						if ((((tmp__399 > 0) && (tmp__401 < tmp__400->len)) || ((tmp__399 < 0) && (tmp__401 >= 0)))) goto L393_;
					L392_:;
					L399_:;
						tmp__397->array[tmp__396] = parse_nullE;
						if (((++ tmp__396) < tmp__398)) goto L402_;
						goto L390_;
					L402_:;
						goto L399_;
						goto L389_;
					L390_:;
						tmp__395 = (parse_Frame) GC_MALLOC(sizeof(struct parse_Frame_struct));
						tmp__395->outerFrame = previousFrame_3;
						tmp__395->frameID = desc_3->frameID;
						tmp__395->valuesUsed = framesize_3;
						tmp__395->notrecyclable = 0;
						tmp__395->values = tmp__397;
						f_12 = tmp__395;
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
						if (haderror_1) 
						{
							tmp__402 = (*((parse_Expr*)TS_Get_Local(errorreturn_id)));
							return tmp__402;
						}
						(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = f_12;
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
						ret_11 = evaluate_eval(model_3->body);
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
						if (ret_11 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
						switch (ret_11->type_) {;
						case Error_typecode:;
							err_22 = ((parse_Error)ret_11);
							tmp__403 = common_returnFromFunction(ret_11);
							ret_11 = tmp__403;
							break;
						default:
							checkTypeValidity(ret_11->type_,__FILE__,__LINE__);
							break;
						};
						(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame_9;
						tmp__394 = ret_11;
					}
					tmp__373 = tmp__394;
				}
				tmp__371 = tmp__373;
			}
			tmp__370 = tmp__371;
		}
		tmp__368 = tmp__370;
	}
	return tmp__368;
}
static M2_string str__37;
parse_Expr evaluate_applyES(parse_Expr f,parse_Sequence v){
	parse_CompiledFunction ff_2;
	parse_CompiledFunctionClosure ff_3;
	parse_FunctionClosure c_3;
	parse_SpecialExpr s_1;
	parse_Expr tmp__404;
	if (f == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (f->type_) {;
	case CompiledFunction_typecode:;
		ff_2 = ((parse_CompiledFunction)f);
		tmp__404 = ff_2->fn(((parse_Expr)v));
		break;
	case CompiledFunctionClosure_typecode:;
		ff_3 = ((parse_CompiledFunctionClosure)f);
		tmp__404 = ff_3->fn(((parse_Expr)v), ff_3->env);
		break;
	case FunctionClosure_typecode:;
		c_3 = ((parse_FunctionClosure)f);
		tmp__404 = evaluate_applyFCS(c_3, v);
		break;
	case SpecialExpr_typecode:;
		s_1 = ((parse_SpecialExpr)f);
		tmp__404 = evaluate_applyES(s_1->e, v);
		break;
	default:
		checkTypeValidity(f->type_,__FILE__,__LINE__);
		tmp__404 = expr_buildErrorPacket(str__37);
		break;
	};
	return tmp__404;
}
static M2_string str__38;
parse_Expr evaluate_applyEE(parse_Expr f,parse_Expr e){
	parse_CompiledFunction ff_4;
	parse_CompiledFunctionClosure ff_5;
	parse_FunctionClosure c_4;
	parse_Sequence v_4;
	parse_SpecialExpr s_2;
	parse_Expr tmp__405;
	parse_Expr tmp__406;
	if (f == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (f->type_) {;
	case CompiledFunction_typecode:;
		ff_4 = ((parse_CompiledFunction)f);
		tmp__405 = ff_4->fn(e);
		break;
	case CompiledFunctionClosure_typecode:;
		ff_5 = ((parse_CompiledFunctionClosure)f);
		tmp__405 = ff_5->fn(e, ff_5->env);
		break;
	case FunctionClosure_typecode:;
		c_4 = ((parse_FunctionClosure)f);
		if (e == 0) invalidNullPointer(__FILE__,__LINE__,-1);
		switch (e->type_) {;
		case Sequence_typecode:;
			v_4 = ((parse_Sequence)e);
			tmp__406 = evaluate_applyFCS(c_4, v_4);
			break;
		default:
			checkTypeValidity(e->type_,__FILE__,__LINE__);
			tmp__406 = evaluate_applyFCE(c_4, e);
			break;
		};
		tmp__405 = tmp__406;
		break;
	case SpecialExpr_typecode:;
		s_2 = ((parse_SpecialExpr)f);
		tmp__405 = evaluate_applyEE(s_2->e, e);
		break;
	default:
		checkTypeValidity(f->type_,__FILE__,__LINE__);
		tmp__405 = expr_buildErrorPacket(str__38);
		break;
	};
	return tmp__405;
}
static M2_string str__39;
parse_Expr evaluate_applyEEE(parse_Expr g,parse_Expr e0,parse_Expr e1){
	parse_CompiledFunction ff_6;
	parse_CompiledFunctionClosure ff_7;
	parse_SpecialExpr s_3;
	parse_FunctionClosure c_5;
	parse_functionCode model_4;
	parse_functionDescription desc_4;
	parse_Frame previousFrame_4;
	int framesize_4;
	parse_Frame f_13;
	parse_Frame previousStashedFrame_3;
	parse_Frame saveLocalFrame_10;
	parse_Expr ret_12;
	parse_Error err_23;
	parse_Frame saveLocalFrame_11;
	parse_Frame f_14;
	parse_Expr ret_13;
	parse_Error err_24;
	parse_Sequence tmp__407;
	parse_Expr tmp__408;
	parse_Sequence tmp__409;
	parse_Sequence tmp__410;
	parse_Expr tmp__411;
	parse_Expr tmp__412;
	parse_Expr tmp__413;
	int tmp__414;
	parse_Frame tmp__415;
	int tmp__416;
	parse_Sequence tmp__417;
	int tmp__418;
	int tmp__419;
	int tmp__420;
	int tmp__421;
	parse_Expr tmp__422;
	int tmp__423;
	parse_Sequence tmp__424;
	int tmp__425;
	int tmp__426;
	int tmp__427;
	parse_Expr tmp__428;
	parse_Frame tmp__429;
	int tmp__430;
	parse_Sequence tmp__431;
	int tmp__432;
	parse_Expr tmp__433;
	if (g == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (g->type_) {;
	case CompiledFunction_typecode:;
		ff_6 = ((parse_CompiledFunction)g);
		tmp__407 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (2 - 1)*sizeof(parse_Expr));
		tmp__407->type_ = 6;
		tmp__407->len = 2;
		tmp__407->array[0] = e0;
		tmp__407->array[1] = e1;
		tmp__408 = ff_6->fn(((parse_Expr)tmp__407));
		break;
	case CompiledFunctionClosure_typecode:;
		ff_7 = ((parse_CompiledFunctionClosure)g);
		tmp__409 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (2 - 1)*sizeof(parse_Expr));
		tmp__409->type_ = 6;
		tmp__409->len = 2;
		tmp__409->array[0] = e0;
		tmp__409->array[1] = e1;
		tmp__408 = ff_7->fn(((parse_Expr)tmp__409), ff_7->env);
		break;
	case SpecialExpr_typecode:;
		s_3 = ((parse_SpecialExpr)g);
		tmp__408 = evaluate_applyEEE(s_3->e, e0, e1);
		break;
	case FunctionClosure_typecode:;
		c_5 = ((parse_FunctionClosure)g);
		model_4 = c_5->model;
		desc_4 = model_4->desc;
		if (desc_4->restargs) 
		{
			tmp__410 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (2 - 1)*sizeof(parse_Expr));
			tmp__410->type_ = 6;
			tmp__410->len = 2;
			tmp__410->array[0] = e0;
			tmp__410->array[1] = e1;
			tmp__411 = evaluate_applyFCS(c_5, tmp__410);
		}
		else
		{
			if ((desc_4->numparms != 2)) 
			{
				tmp__412 = common_WrongNumArgs_1(model_4->arrow, desc_4->numparms, 2);
			}
			else
			{
				if (((*((int*)TS_Get_Local(expr_recursionDepth_id))) > (*((int*)TS_Get_Local(expr_recursionLimit_id))))) 
				{
					tmp__413 = evaluate_RecursionLimit();
				}
				else
				{
					previousFrame_4 = c_5->frame;
					framesize_4 = desc_4->framesize;
					if ((framesize_4 < (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len)) 
					{
						tmp__414 = framesize_4;
						if (tmp__414 < 0 || tmp__414 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__414,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",915,36);
						f_13 = (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__414];
						previousStashedFrame_3 = f_13->outerFrame;
						if ((f_13 == previousStashedFrame_3)) 
						{
							tmp__416 = 0;
							tmp__418 = framesize_4;
							if (0 > tmp__418) fatalarraylen(tmp__418,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",919,47);
							tmp__417 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (tmp__418 - 1)*sizeof(parse_Expr));
							tmp__417->type_ = 6;
							tmp__417->len = tmp__418;
							if ((tmp__418 == 0)) goto L410_;
						L409_:;
							tmp__417->array[tmp__416] = e0;
							if (((++ tmp__416) < tmp__418)) goto L411_;
							goto L410_;
						L411_:;
							tmp__417->array[tmp__416] = e1;
							if (((++ tmp__416) < tmp__418)) goto L412_;
							goto L410_;
						L412_:;
						L413_:;
							tmp__417->array[tmp__416] = parse_nullE;
							if (((++ tmp__416) < tmp__418)) goto L416_;
							goto L410_;
						L416_:;
							goto L413_;
							goto L409_;
						L410_:;
							tmp__415 = (parse_Frame) GC_MALLOC(sizeof(struct parse_Frame_struct));
							tmp__415->outerFrame = previousFrame_4;
							tmp__415->frameID = desc_4->frameID;
							tmp__415->valuesUsed = framesize_4;
							tmp__415->notrecyclable = 0;
							tmp__415->values = tmp__417;
							f_13 = tmp__415;
						}
						else
						{
							tmp__419 = framesize_4;
							if (tmp__419 < 0 || tmp__419 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__419,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",927,36);
							(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__419] = previousStashedFrame_3;
							f_13->outerFrame = previousFrame_4;
							f_13->frameID = desc_4->frameID;
							tmp__420 = 0;
							if (tmp__420 < 0 || tmp__420 >= f_13->values->len) fatalarrayindex(tmp__420,f_13->values->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",930,34);
							f_13->values->array[tmp__420] = e0;
							tmp__421 = 1;
							if (tmp__421 < 0 || tmp__421 >= f_13->values->len) fatalarrayindex(tmp__421,f_13->values->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",931,34);
							f_13->values->array[tmp__421] = e1;
						}
						saveLocalFrame_10 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
						(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = f_13;
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
						ret_12 = evaluate_eval(model_4->body);
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
						if (ret_12 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
						switch (ret_12->type_) {;
						case Error_typecode:;
							err_23 = ((parse_Error)ret_12);
							tmp__422 = common_returnFromFunction(ret_12);
							ret_12 = tmp__422;
							break;
						default:
							checkTypeValidity(ret_12->type_,__FILE__,__LINE__);
							break;
						};
						(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame_10;
						if ((!(f_13->notrecyclable))) 
						{
							tmp__423 = 1;
							tmp__424 = f_13->values;
							if ((tmp__424->len == 0)) goto L419_;
							if ((tmp__423 > 0)) tmp__425 = 0; else tmp__425 = (tmp__424->len - 1);
						L420_:;
							tmp__424->array[tmp__425] = parse_nullE;
							tmp__425 = (tmp__425 + tmp__423);
							if ((((tmp__423 > 0) && (tmp__425 < tmp__424->len)) || ((tmp__423 < 0) && (tmp__425 >= 0)))) goto L420_;
						L419_:;
							tmp__426 = framesize_4;
							if (tmp__426 < 0 || tmp__426 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__426,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",942,51);
							f_13->outerFrame = (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__426];
							f_13->frameID = (- 2);
							tmp__427 = framesize_4;
							if (tmp__427 < 0 || tmp__427 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__427,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",944,36);
							(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__427] = f_13;
						}
						tmp__428 = ret_12;
					}
					else
					{
						saveLocalFrame_11 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
						tmp__430 = 0;
						tmp__432 = framesize_4;
						if (0 > tmp__432) fatalarraylen(tmp__432,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",951,42);
						tmp__431 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (tmp__432 - 1)*sizeof(parse_Expr));
						tmp__431->type_ = 6;
						tmp__431->len = tmp__432;
						if ((tmp__432 == 0)) goto L422_;
					L421_:;
						tmp__431->array[tmp__430] = e0;
						if (((++ tmp__430) < tmp__432)) goto L423_;
						goto L422_;
					L423_:;
						tmp__431->array[tmp__430] = e1;
						if (((++ tmp__430) < tmp__432)) goto L424_;
						goto L422_;
					L424_:;
					L425_:;
						tmp__431->array[tmp__430] = parse_nullE;
						if (((++ tmp__430) < tmp__432)) goto L428_;
						goto L422_;
					L428_:;
						goto L425_;
						goto L421_;
					L422_:;
						tmp__429 = (parse_Frame) GC_MALLOC(sizeof(struct parse_Frame_struct));
						tmp__429->outerFrame = previousFrame_4;
						tmp__429->frameID = desc_4->frameID;
						tmp__429->valuesUsed = framesize_4;
						tmp__429->notrecyclable = 0;
						tmp__429->values = tmp__431;
						f_14 = tmp__429;
						(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = f_14;
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
						ret_13 = evaluate_eval(model_4->body);
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
						if (ret_13 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
						switch (ret_13->type_) {;
						case Error_typecode:;
							err_24 = ((parse_Error)ret_13);
							tmp__433 = common_returnFromFunction(ret_13);
							ret_13 = tmp__433;
							break;
						default:
							checkTypeValidity(ret_13->type_,__FILE__,__LINE__);
							break;
						};
						(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame_11;
						tmp__428 = ret_13;
					}
					tmp__413 = tmp__428;
				}
				tmp__412 = tmp__413;
			}
			tmp__411 = tmp__412;
		}
		tmp__408 = tmp__411;
		break;
	default:
		checkTypeValidity(g->type_,__FILE__,__LINE__);
		tmp__408 = expr_buildErrorPacket(str__39);
		break;
	};
	return tmp__408;
}
parse_Expr evaluate_applyEEEE(parse_Expr g,parse_Expr e0,parse_Expr e1,parse_Expr e2)
{
	parse_Frame f_15;
	parse_Frame previousStashedFrame_4;
	parse_Frame saveLocalFrame_12;
	parse_Expr ret_14;
	parse_Error err_25;
	parse_Frame saveLocalFrame_13;
	parse_Frame f_16;
	parse_Expr ret_15;
	parse_Error err_26;
	parse_Sequence tmp__434;
	parse_Expr tmp__435;
	parse_Sequence tmp__436;
	parse_Expr tmp__439;
	parse_Expr tmp__440;
	int tmp__441;
	parse_Frame tmp__442;
	int tmp__443;
	parse_Sequence tmp__444;
	int tmp__445;
	int tmp__446;
	int tmp__447;
	int tmp__448;
	int tmp__449;
	parse_Expr tmp__450;
	int tmp__451;
	parse_Sequence tmp__452;
	int tmp__453;
	int tmp__454;
	int tmp__455;
	parse_Expr tmp__456;
	parse_Frame tmp__457;
	int tmp__458;
	parse_Sequence tmp__459;
	int tmp__460;
	parse_Expr tmp__461;
	M2CPP_PerformAssertions(g);
	switch (g->type_) {
	case CompiledFunction_typecode:
		{
			parse_CompiledFunction ff = reinterpret_cast<parse_CompiledFunction>(g);
			parse_Sequence tmp = M2CPP_NewSequence(3);
			tmp->array[0] = e0;
			tmp->array[1] = e1;
			tmp->array[2] = e2;
			return ff_8->fn(reinterpret_cast<parse_Expr>(tmp));
		}
	case CompiledFunctionClosure_typecode:
		{
			parse_CompiledFunctionClosure ff = reinterpret_cast<parse_CompiledFunctionClosure>(g);
			parse_Sequence tmp = M2CPP_NewSequence(3);
			tmp->array[0] = e0;
			tmp->array[1] = e1;
			tmp->array[2] = e2;
			return ff_9->fn(reinterpret_cast<parse_Expr>(tmp), ff->env);
		}
	case SpecialExpr_typecode:
		{
			parse_SpecialExpr s_4 = reinterpret_cast<parse_SpecialExpr>(g);
			return evaluate_applyEEEE(s_4->e, e0, e1, e2);
		}
	case FunctionClosure_typecode:
		{
			//this should be factored out into its own function!
			parse_FunctionClosure c = reinterpret_cast<parse_FunctionClosure>(g);
			parse_functionCode model = c->model;
			parse_functionDescription desc = model->desc;
			if (desc->restargs) 
			{
				parse_Sequence tmp__437 = M2CPP_NewSequence(3);
				tmp__437->array[0] = e0;
				tmp__437->array[1] = e1;
				tmp__437->array[2] = e2;
				return evaluate_applyFCS(c_6, tmp__437);
			}
			if (desc_5->numparms != 3)
				return common_WrongNumArgs_1(model_5->arrow, desc_5->numparms, 3);
			if (il->getRecursionDepth() > il->getRecursionLimit())
				return evaluate_RecursionLimit();
			parse_Frame previousFrame = c->frame;
			int framesize = desc->framesize;
			//next we need to enter a new frame...
			f = 
			if (framesize_5 < (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len)) 
					{
						tmp__441 = framesize_5;
						if (tmp__441 < 0 || tmp__441 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__441,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",984,36);
						f_15 = (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__441];
						previousStashedFrame_4 = f_15->outerFrame;
						if ((f_15 == previousStashedFrame_4)) 
						{
							tmp__443 = 0;
							tmp__445 = framesize_5;
							if (0 > tmp__445) fatalarraylen(tmp__445,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",988,47);
							tmp__444 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (tmp__445 - 1)*sizeof(parse_Expr));
							tmp__444->type_ = 6;
							tmp__444->len = tmp__445;
							if ((tmp__445 == 0)) goto L435_;
						L434_:;
							tmp__444->array[tmp__443] = e0;
							if (((++ tmp__443) < tmp__445)) goto L436_;
							goto L435_;
						L436_:;
							tmp__444->array[tmp__443] = e1;
							if (((++ tmp__443) < tmp__445)) goto L437_;
							goto L435_;
						L437_:;
							tmp__444->array[tmp__443] = e2;
							if (((++ tmp__443) < tmp__445)) goto L438_;
							goto L435_;
						L438_:;
						L439_:;
							tmp__444->array[tmp__443] = parse_nullE;
							if (((++ tmp__443) < tmp__445)) goto L442_;
							goto L435_;
						L442_:;
							goto L439_;
							goto L434_;
						L435_:;
							tmp__442 = (parse_Frame) GC_MALLOC(sizeof(struct parse_Frame_struct));
							tmp__442->outerFrame = previousFrame_5;
							tmp__442->frameID = desc_5->frameID;
							tmp__442->valuesUsed = framesize_5;
							tmp__442->notrecyclable = 0;
							tmp__442->values = tmp__444;
							f_15 = tmp__442;
						}
						else
						{
							tmp__446 = framesize_5;
							if (tmp__446 < 0 || tmp__446 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__446,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",997,36);
							(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__446] = previousStashedFrame_4;
							f_15->outerFrame = previousFrame_5;
							f_15->frameID = desc_5->frameID;
							tmp__447 = 0;
							if (tmp__447 < 0 || tmp__447 >= f_15->values->len) fatalarrayindex(tmp__447,f_15->values->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1000,34);
							f_15->values->array[tmp__447] = e0;
							tmp__448 = 1;
							if (tmp__448 < 0 || tmp__448 >= f_15->values->len) fatalarrayindex(tmp__448,f_15->values->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1001,34);
							f_15->values->array[tmp__448] = e1;
							tmp__449 = 2;
							if (tmp__449 < 0 || tmp__449 >= f_15->values->len) fatalarrayindex(tmp__449,f_15->values->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1002,34);
							f_15->values->array[tmp__449] = e2;
						}
						saveLocalFrame_12 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
						(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = f_15;
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
						ret_14 = evaluate_eval(model_5->body);
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
						if (ret_14 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
						switch (ret_14->type_) {;
						case Error_typecode:;
							err_25 = ((parse_Error)ret_14);
							tmp__450 = common_returnFromFunction(ret_14);
							ret_14 = tmp__450;
							break;
						default:
							checkTypeValidity(ret_14->type_,__FILE__,__LINE__);
							break;
						};
						(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame_12;
						if ((!(f_15->notrecyclable))) 
						{
							tmp__451 = 1;
							tmp__452 = f_15->values;
							if ((tmp__452->len == 0)) goto L445_;
							if ((tmp__451 > 0)) tmp__453 = 0; else tmp__453 = (tmp__452->len - 1);
						L446_:;
							tmp__452->array[tmp__453] = parse_nullE;
							tmp__453 = (tmp__453 + tmp__451);
							if ((((tmp__451 > 0) && (tmp__453 < tmp__452->len)) || ((tmp__451 < 0) && (tmp__453 >= 0)))) goto L446_;
						L445_:;
							tmp__454 = framesize_5;
							if (tmp__454 < 0 || tmp__454 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__454,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1013,51);
							f_15->outerFrame = (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__454];
							f_15->frameID = (- 2);
							tmp__455 = framesize_5;
							if (tmp__455 < 0 || tmp__455 >= (*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len) fatalarrayindex(tmp__455,(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1015,36);
							(*((struct SCC_M2_0_int_len_parse_Frame_array1 **)TS_Get_Local(recycleBin_id)))->array[tmp__455] = f_15;
						}
						tmp__456 = ret_14;
					}
					else
					{
						saveLocalFrame_13 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
						tmp__458 = 0;
						tmp__460 = framesize_5;
						if (0 > tmp__460) fatalarraylen(tmp__460,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1021,42);
						tmp__459 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (tmp__460 - 1)*sizeof(parse_Expr));
						tmp__459->type_ = 6;
						tmp__459->len = tmp__460;
						if ((tmp__460 == 0)) goto L448_;
					L447_:;
						tmp__459->array[tmp__458] = e0;
						if (((++ tmp__458) < tmp__460)) goto L449_;
						goto L448_;
					L449_:;
						tmp__459->array[tmp__458] = e1;
						if (((++ tmp__458) < tmp__460)) goto L450_;
						goto L448_;
					L450_:;
						tmp__459->array[tmp__458] = e2;
						if (((++ tmp__458) < tmp__460)) goto L451_;
						goto L448_;
					L451_:;
					L452_:;
						tmp__459->array[tmp__458] = parse_nullE;
						if (((++ tmp__458) < tmp__460)) goto L455_;
						goto L448_;
					L455_:;
						goto L452_;
						goto L447_;
					L448_:;
						tmp__457 = (parse_Frame) GC_MALLOC(sizeof(struct parse_Frame_struct));
						tmp__457->outerFrame = previousFrame_5;
						tmp__457->frameID = desc_5->frameID;
						tmp__457->valuesUsed = framesize_5;
						tmp__457->notrecyclable = 0;
						tmp__457->values = tmp__459;
						f_16 = tmp__457;
						(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = f_16;
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) + 1);
						ret_15 = evaluate_eval(model_5->body);
						(*((int*)TS_Get_Local(expr_recursionDepth_id))) = ((*((int*)TS_Get_Local(expr_recursionDepth_id))) - 1);
						if (ret_15 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
						switch (ret_15->type_) {;
						case Error_typecode:;
							err_26 = ((parse_Error)ret_15);
							tmp__461 = common_returnFromFunction(ret_15);
							ret_15 = tmp__461;
							break;
						default:
							checkTypeValidity(ret_15->type_,__FILE__,__LINE__);
							break;
						};
						(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame_13;
						tmp__456 = ret_15;
					}
					tmp__440 = tmp__456;
				}
				tmp__439 = tmp__440;
			}
			tmp__438 = tmp__439;
		}
		tmp__435 = tmp__438;
		break;
	default:
		checkTypeValidity(g->type_,__FILE__,__LINE__);
		static M2_string str__40;
		tmp__435 = expr_buildErrorPacket(M2CPP_NewConstString("expected a function"));
		break;
	};
	return tmp__435;
}
parse_Expr evaluate_unarymethod(parse_Code rhs,parse_SymbolClosure methodkey)
{
	parse_Expr right = evaluate_eval(rhs);
	if(M2CPP_IsError(right))
		return right;
	parse_Expr method = hashtables_lookup(classes_Class(right), reinterpret_cast<parse_Expr>(methodkey), methodkey->symbol->hash);
	if (method == parse_nullE)
		return expr_MissingMethod_1(methodkey);
	else
		return evaluate_applyEE(method, right);
}
parse_Expr evaluate_binarymethod(parse_Code lhs,parse_Code rhs,parse_SymbolClosure methodkey)
{
	parse_Expr left = evaluate_eval(lhs);
	if(M2CPP_IsError(left))
		return left;
	parse_Expr right = evaluate_eval(rhs);
	if(M2CPP_IsError(right))
		return right;
	parse_Expr method = hashtables_lookupBinaryMethod(classes_Class(left), classes_Class(right), reinterpret_cast<parse_Expr>(methodkey), methodkey->symbol->hash);
	if (method == parse_nullE)
		return expr_MissingMethodPair_2(methodkey, left, right);
	else
		return evaluate_applyEEE(method, left, right);
}
parse_Expr evaluate_binarymethod_1(parse_Expr left,parse_Code rhs,parse_SymbolClosure methodkey)
{
	parse_Expr right = evaluate_eval(rhs);
	if(M2CPP_IsError(right))
		return right;
	parse_Expr method = hashtables_lookupBinaryMethod(classes_Class(left), classes_Class(right_2), reinterpret_cast<parse_Expr>(methodkey), methodkey->symbol->hash);
	if (method == parse_nullE)
	{
		if (methodkey == binding_AdjacentS)
		{
			M2CPP_PerformAssertions(left);
			if(M2CPP_IsTypeExact(left,SymbolClosure_typecode))
			{
				parse_SymbolClosure f = reinterpret_cast<parse_SymbolClosure>(left);
				M2_string str__41 = M2CPP_NewConstString("symbol '");
				M2_string str__42 = M2CPP_NewConstString("' has not been defined as a function");
				return expr_buildErrorPacket(strings_plus_(strings_plus_(str__41, f->symbol->word->name), str__42));
			}
			else
				return expr_MissingMethodPair_2(methodkey, left, right);
		}
		else
			return expr_MissingMethodPair_2(methodkey, left, right);
	}
	else
		return evaluate_applyEEE(method, left, right);
}
static M2_string str__43;
static M2_string str__44;
static parse_Expr globalAssignmentHook(parse_Symbol t,parse_Expr oldvalue,parse_Expr newvalue){
	parse_Expr method_3;
	parse_Expr symbody;
	parse_Expr sym_1;
	parse_Expr g_1;
	parse_List s_5;
	parse_Expr r_5;
	parse_CompiledFunction f_19;
	parse_CompiledFunctionClosure f_20;
	parse_FunctionClosure f_21;
	parse_SpecialExpr f_22;
	parse_Expr y_1;
	parse_Expr y_2;
	parse_Expr y_3;
	parse_SymbolBody tmp__471;
	parse_SymbolClosure tmp__472;
	parse_Frame tmp__473;
	int tmp__474;
	parse_Sequence tmp__475;
	int tmp__476;
	parse_Expr tmp__477;
	parse_Expr tmp__478;
	parse_Expr tmp__479;
	parse_Expr tmp__480;
	parse_Expr tmp__481;
	method_3 = hashtables_lookup_1(classes_Class(oldvalue), binding_GlobalReleaseE);
	tmp__471 = (parse_SymbolBody) GC_MALLOC(sizeof(struct parse_SymbolBody_struct));
	tmp__471->type_ = 99;
	tmp__471->symbol = t;
	symbody = ((parse_Expr)tmp__471);
	if (t->thread) 
	{
		tmp__473 = expr_enlargeThreadFrame();
	}
	else
	{
		tmp__473 = expr_globalFrame;
	}
	tmp__472 = (parse_SymbolClosure) GC_MALLOC(sizeof(struct parse_SymbolClosure_struct));
	tmp__472->type_ = 9;
	tmp__472->frame = tmp__473;
	tmp__472->symbol = t;
	sym_1 = ((parse_Expr)tmp__472);
	g_1 = hashtables_lookup1_1(evaluate_globalAssignmentHooks, symbody);
	if ((g_1 != parse_notfoundE)) 
	{
		if (g_1 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
		switch (g_1->type_) {;
		case List_typecode:;
			s_5 = ((parse_List)g_1);
			tmp__474 = 1;
			tmp__475 = s_5->v;
			if ((tmp__475->len == 0)) goto L463_;
			if ((tmp__474 > 0)) tmp__476 = 0; else tmp__476 = (tmp__475->len - 1);
		L464_:;
			r_5 = evaluate_applyEEE(tmp__475->array[tmp__476], sym_1, newvalue);
			if (r_5 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
			switch (r_5->type_) {;
			case Error_typecode:;
				tmp__477 = r_5;
				return tmp__477;
				break;
			default:
				checkTypeValidity(r_5->type_,__FILE__,__LINE__);
				break;
			};
			tmp__476 = (tmp__476 + tmp__474);
			if ((((tmp__474 > 0) && (tmp__476 < tmp__475->len)) || ((tmp__474 < 0) && (tmp__476 >= 0)))) goto L464_;
		L463_:;
			tmp__478 = parse_nullE;
			break;
		case CompiledFunction_typecode:;
			f_19 = ((parse_CompiledFunction)g_1);
			tmp__478 = evaluate_applyEEE(g_1, sym_1, newvalue);
			break;
		case CompiledFunctionClosure_typecode:;
			f_20 = ((parse_CompiledFunctionClosure)g_1);
			tmp__478 = evaluate_applyEEE(g_1, sym_1, newvalue);
			break;
		case FunctionClosure_typecode:;
			f_21 = ((parse_FunctionClosure)g_1);
			tmp__478 = evaluate_applyEEE(g_1, sym_1, newvalue);
			break;
		case SpecialExpr_typecode:;
			f_22 = ((parse_SpecialExpr)g_1);
			tmp__478 = evaluate_applyEEE(f_22->e, sym_1, newvalue);
			break;
		default:
			checkTypeValidity(g_1->type_,__FILE__,__LINE__);
			tmp__478 = expr_buildErrorPacket(strings_plus_(strings_plus_(str__43, t->word->name), str__44));
			break;
		};
		y_1 = tmp__478;
		if (y_1 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
		switch (y_1->type_) {;
		case Error_typecode:;
			tmp__479 = y_1;
			return tmp__479;
			break;
		default:
			checkTypeValidity(y_1->type_,__FILE__,__LINE__);
			break;
		};
	}
	if ((method_3 != parse_nullE)) 
	{
		y_2 = evaluate_applyEEE(method_3, sym_1, oldvalue);
		if (y_2 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
		switch (y_2->type_) {;
		case Error_typecode:;
			tmp__480 = y_2;
			return tmp__480;
			break;
		default:
			checkTypeValidity(y_2->type_,__FILE__,__LINE__);
			break;
		};
	}
	method_3 = hashtables_lookup_1(classes_Class(newvalue), binding_GlobalAssignE);
	if ((method_3 != parse_nullE)) 
	{
		y_3 = evaluate_applyEEE(method_3, sym_1, newvalue);
		if (y_3 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
		switch (y_3->type_) {;
		case Error_typecode:;
			tmp__481 = y_3;
			return tmp__481;
			break;
		default:
			checkTypeValidity(y_3->type_,__FILE__,__LINE__);
			break;
		};
	}
	return parse_nullE;
}
static parse_Expr localAssignment(int nestingDepth,int frameindex,parse_Expr newvalue){
	parse_Frame f_23;
	parse_Frame tmp__482;
	parse_Frame tmp__483;
	parse_Frame tmp__484;
	parse_Frame tmp__485;
	int tmp__486;
	f_23 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
	if ((nestingDepth == 0)) 
	{
	}
	else
	{
		if ((nestingDepth == 1)) 
		{
			tmp__482 = f_23->outerFrame;
			f_23 = tmp__482;
		}
		else
		{
			if ((nestingDepth == 2)) 
			{
				tmp__483 = f_23->outerFrame->outerFrame;
				f_23 = tmp__483;
			}
			else
			{
				tmp__484 = f_23->outerFrame->outerFrame->outerFrame;
				f_23 = tmp__484;
				nestingDepth = (nestingDepth - 3);
			L470_:;
				if ((! (nestingDepth > 0))) goto L471_;
				nestingDepth = (nestingDepth - 1);
				tmp__485 = f_23->outerFrame;
				f_23 = tmp__485;
				goto L470_;
			L471_:;
			}
		}
	}
	tmp__486 = frameindex;
	if (tmp__486 < 0 || tmp__486 >= f_23->values->len) fatalarrayindex(tmp__486,f_23->values->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1114,14);
	f_23->values->array[tmp__486] = newvalue;
	return newvalue;
}
static M2_string str__45;
static M2_string str__46;
static M2_string str__47;
static M2_string str__48;
static parse_Expr globalAssignment(int frameindex,parse_Symbol t,parse_Expr newvalue){
	parse_Sequence vals;
	parse_Expr r_6;
	parse_Expr tmp__487;
	parse_Expr tmp__488;
	parse_Frame tmp__489;
	int tmp__490;
	parse_Expr tmp__491;
	int tmp__492;
	if (t->Protected) 
	{
		if ((t->position != stdiop0_dummyPosition)) 
		{
			tmp__487 = expr_buildErrorPacket(strings_plus_(strings_plus_(strings_plus_(str__45, t->word->name), str__46), stdiop_tostring(t->position)));
		}
		else
		{
			tmp__487 = expr_buildErrorPacket(strings_plus_(strings_plus_(str__47, t->word->name), str__48));
		}
		tmp__488 = tmp__487;
		return tmp__488;
	}
	if (t->thread) 
	{
		tmp__489 = expr_enlargeThreadFrame();
	}
	else
	{
		tmp__489 = expr_globalFrame;
	}
	vals = tmp__489->values;
	tmp__490 = frameindex;
	if (tmp__490 < 0 || tmp__490 >= vals->len) fatalarrayindex(tmp__490,vals->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1124,38);
	r_6 = globalAssignmentHook(t, vals->array[tmp__490], newvalue);
	if (r_6 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (r_6->type_) {;
	case Error_typecode:;
		tmp__491 = r_6;
		return tmp__491;
		break;
	default:
		checkTypeValidity(r_6->type_,__FILE__,__LINE__);
		break;
	};
	tmp__492 = frameindex;
	if (tmp__492 < 0 || tmp__492 >= vals->len) fatalarrayindex(tmp__492,vals->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1126,10);
	vals->array[tmp__492] = newvalue;
	return newvalue;
}
static parse_Expr assignment(int nestingDepth,int frameindex,parse_Symbol t,parse_Expr newvalue){
	parse_Expr tmp__493;
	if ((nestingDepth == (- 1))) 
	{
		tmp__493 = globalAssignment(frameindex, t, newvalue);
	}
	else
	{
		tmp__493 = localAssignment(nestingDepth, frameindex, newvalue);
	}
	return tmp__493;
}
static parse_Expr globalAssignmentFun(parse_globalAssignmentCode x){
	parse_Symbol t_1;
	parse_Expr newvalue_1;
	parse_Expr tmp__494;
	t_1 = x->lhs;
	newvalue_1 = evaluate_eval(x->rhs);
	if (newvalue_1 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (newvalue_1->type_) {;
	case Error_typecode:;
		tmp__494 = newvalue_1;
		return tmp__494;
		break;
	default:
		checkTypeValidity(newvalue_1->type_,__FILE__,__LINE__);
		break;
	};
	return globalAssignment(t_1->frameindex, t_1, newvalue_1);
}
static M2_string str__49;
static M2_string str__50;
static M2_string str__51;
static M2_string str__52;
static M2_string str__53;
static M2_string str__54;
static parse_Expr parallelAssignmentFun(parse_parallelAssignmentCode x){
	parse_SymbolSequence syms;
	M2_arrayint nestingDepth_1;
	M2_arrayint frameindex_1;
	int nlhs;
	parse_Expr value_1;
	parse_Sequence values;
	parse_Expr r_7;
	int tmp__495;
	parse_SymbolSequence tmp__496;
	int tmp__497;
	parse_Expr tmp__498;
	parse_Expr tmp__499;
	int i_7;
	int tmp__500;
	int tmp__501;
	int tmp__502;
	int tmp__503;
	int tmp__504;
	parse_Expr tmp__505;
	parse_Expr tmp__506;
	parse_Expr tmp__507;
	syms = x->lhs;
	nestingDepth_1 = x->nestingDepth;
	frameindex_1 = x->frameindex;
	nlhs = frameindex_1->len;
	tmp__495 = 1;
	tmp__496 = syms;
	if ((tmp__496->len == 0)) goto L478_;
	if ((tmp__495 > 0)) tmp__497 = 0; else tmp__497 = (tmp__496->len - 1);
 L479_:;
	if (tmp__496->array[tmp__497]->Protected) 
	{
		tmp__498 = expr_buildErrorPacket(strings_plus_(strings_plus_(str__49, tmp__496->array[tmp__497]->word->name), str__50));
		return tmp__498;
	}
	tmp__497 = (tmp__497 + tmp__495);
	if ((((tmp__495 > 0) && (tmp__497 < tmp__496->len)) || ((tmp__495 < 0) && (tmp__497 >= 0)))) goto L479_;
 L478_:;
	value_1 = evaluate_eval(x->rhs);
	if (value_1 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (value_1->type_) {;
	case Error_typecode:;
		tmp__499 = value_1;
		return tmp__499;
		break;
	case Sequence_typecode:;
		values = ((parse_Sequence)value_1);
		if ((nlhs == values->len)) 
		{
			tmp__500 = (nlhs - 1);
			i_7 = 0;
			goto L484_;
		L482_:;
			i_7 = (i_7 + 1);
		L484_:;
			if ((((i_7 > tmp__500) && (1 >= 0)) || ((i_7 < tmp__500) && (1 < 0)))) goto L483_;
			tmp__501 = i_7;
			if (tmp__501 < 0 || tmp__501 >= nestingDepth_1->len) fatalarrayindex(tmp__501,nestingDepth_1->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1152,44);
			tmp__502 = i_7;
			if (tmp__502 < 0 || tmp__502 >= frameindex_1->len) fatalarrayindex(tmp__502,frameindex_1->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1152,57);
			tmp__503 = i_7;
			if (tmp__503 < 0 || tmp__503 >= syms->len) fatalarrayindex(tmp__503,syms->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1152,64);
			tmp__504 = i_7;
			if (tmp__504 < 0 || tmp__504 >= values->len) fatalarrayindex(tmp__504,values->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1152,73);
			r_7 = assignment(nestingDepth_1->array[tmp__501], frameindex_1->array[tmp__502], syms->array[tmp__503], values->array[tmp__504]);
			if (r_7 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
			switch (r_7->type_) {;
			case Error_typecode:;
				tmp__505 = r_7;
				return tmp__505;
				break;
			default:
				checkTypeValidity(r_7->type_,__FILE__,__LINE__);
				break;
			};
			goto L482_;
		L483_:;
			tmp__506 = value_1;
		}
		else
		{
			tmp__506 = expr_buildErrorPacket(strings_plus_(strings_plus_(str__51, strings1_tostring_2(nlhs)), str__52));
		}
		tmp__507 = tmp__506;
		break;
	default:
		checkTypeValidity(value_1->type_,__FILE__,__LINE__);
		tmp__507 = expr_buildErrorPacket(strings_plus_(strings_plus_(str__53, strings1_tostring_2(nlhs)), str__54));
		break;
	};
	return tmp__507;
}
static M2_string str__55;
static M2_string str__56;
static char steppingFurther(parse_Code c){
	stdiop0_Position p_5;
	char tmp__508;
	char tmp__509;
	char tmp__510;
	char tmp__511;
	char tmp__512;
	char tmp__513;
	char tmp__514;
	tmp__508 = (*((char*)TS_Get_Local(interrupts_steppingFlag_id)));
	if (tmp__508) {
		p_5 = common_codePosition(c);
		tmp__509 = (p_5 == stdiop0_dummyPosition);
		if ((! tmp__509)) {
			tmp__509 = (p_5->loadDepth < (*((unsigned short*)TS_Get_Local(common_errorDepth_id))));
		}
		if (tmp__509) 
		{
			tmp__510 = 1;
			return tmp__510;
		}
		if ((interrupts_stepCount >= 0)) 
		{
			tmp__511 = ((*((stdiop0_Position*)TS_Get_Local(lastCodePosition_id)))->filename != p_5->filename);
			if ((! tmp__511)) {
				tmp__511 = ((*((stdiop0_Position*)TS_Get_Local(lastCodePosition_id)))->line != p_5->line);
			}
			if (tmp__511) 
			{
				interrupts_stepCount = (interrupts_stepCount - 1);
				(*((stdiop0_Position*)TS_Get_Local(lastCodePosition_id)))->filename = p_5->filename;
				(*((stdiop0_Position*)TS_Get_Local(lastCodePosition_id)))->line = p_5->line;
				tmp__512 = ((*((int*)TS_Get_Local(expr_debugLevel_id))) == 1001);
				if (tmp__512) {
					tmp__512 = (interrupts_stepCount >= 0);
				}
				if (tmp__512) 
				{
					stdiop_printErrorMessage(p_5, strings_plus_(str__55, stdio_present_1(common_tostring(c))));
				}
			}
			tmp__513 = (interrupts_stepCount >= 0);
		}
		else
		{
			if ((interrupts_microStepCount >= 0)) 
			{
				if (((*((parse_Code*)TS_Get_Local(lastCode_id))) != c)) 
				{
					interrupts_microStepCount = (interrupts_microStepCount - 1);
					(*((parse_Code*)TS_Get_Local(lastCode_id))) = c;
					if ((interrupts_microStepCount >= 0)) 
					{
						stdiop_printErrorMessage(p_5, strings_plus_(str__56, stdio_present_1(common_tostring(c))));
					}
				}
				tmp__514 = (interrupts_microStepCount >= 0);
			}
			else
			{
				tmp__514 = 0;
			}
			tmp__513 = tmp__514;
		}
		tmp__508 = tmp__513;
	}
	return tmp__508;
}
static parse_Expr handleError(parse_Code c,parse_Expr e)
{

	parse_Expr z_3;
	parse_Error z_4;
	gmp_ZZcell step;
	parse_Expr tmp__515;
	char tmp__516;
	char tmp__517;
	char tmp__518;
	char tmp__519;
	char tmp__520;
	char tmp__521;
	char tmp__522;
	parse_Expr tmp__523;
	char tmp__524;
	char tmp__525;
	char tmp__526;
	char tmp__527;
	char tmp__528;
	parse_Error tmp__529;
	parse_Expr tmp__530;
	parse_Expr tmp__531;
	char tmp__532;
	parse_Expr tmp__533;
	int tmp__534;
	int tmp__535;
	parse_Expr tmp__536;
	parse_Expr tmp__537;
	parse_Expr tmp__538;
	parse_Expr tmp__539;
	parse_Expr tmp__540;
	parse_Expr tmp__541;
	parse_Expr tmp__542;
	if(!M2CPP_IsError(e))
		return e;
	parse_Error err = reinterpret_cast<parse_Error>(e);
	M2CPP_InterperterLocal* il = M2CPP_Interperter::glp();
	if(il->shouldSuppressErrors())
		return e;
	if(err->message == tokens_returnMessage || err->message == tokens_continueMessage || err->message == tokens_continueMessageWithArg ||
	   err->message == tokens_stepMessage || err->message == tokens_stepMessageWithArg || err->message == tokens->breakMessage ||
	   err->message == tokens_unwindMessage || err->message == tokens_throwMessage)
	{
		//an error message that is being used to transfer control must be passed in the line.
		//the position is plugged in just in case its unhandled.
		if (err->position == stdiop0_dummyPosition)
			err_27->position = common_codePosition(c);
		return e;
	}
	stdiop0_Position p = common_codePosition(c);
	interrupts_clearAllFlags();
	interrupts_clearAlarm();
	if(p->loadDepth >= il->getErrorDepth() && !stdiop0_equal_equal_equal_(err->position, p))
	{
		parse_Frame oldReportFrame = err->frame;
		err->frame = parse_noRecycle(il->getLocalFrame());
		err->position = p;
		if(!err->printed || il->shouldBackTrace() && il->getLocalFrame()!=oldReportFrame)
		{
			if(il->inDebuggingMode() && !il->stopIfError() && !(strings_equal_equal_equal_(p_6->filename, str__57)))
			{
				if (!err->printed)
					tokens_printError(err);
				stdiop_printErrorMessage(err_27->position, str__58);
				parse_Expr z = tokens_debuggerFun(il->getLocalFrame(), c);
				if(M2CPP_IsError(z))
				{
					parse_Error z_4 = reinterpret_cast<parse_Error>(z);
					if (z_4->message == tokens_breakMessage) 
					{
						tmp__530 = expr_buildErrorPacket(tokens_unwindMessage);
					}
						else
						{
							if ((z_4->message == tokens_returnMessage)) 
							{
								interrupts_setSteppingFlag();
								(*((stdiop0_Position*)TS_Get_Local(lastCodePosition_id)))->filename = str__59;
								tmp__531 = z_4->value;
							}
							else
							{
								tmp__532 = (z_4->message == tokens_stepMessageWithArg);
								if ((! tmp__532)) {
									tmp__532 = (z_4->message == tokens_stepMessage);
								}
								if (tmp__532) 
								{
									interrupts_setSteppingFlag();
									(*((stdiop0_Position*)TS_Get_Local(lastCodePosition_id)))->filename = str__60;
									tmp__533 = z_4->value;
									if (tmp__533 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
									switch (tmp__533->type_) {;
									case ZZcell_typecode:;
										step = ((gmp_ZZcell)tmp__533);
										if (gmp_isInt_1(step)) 
										{
											tmp__534 = gmp_toInt_1(step);
										}
										else
										{
											tmp__534 = 1;
										}
										tmp__535 = tmp__534;
										break;
									default:
										checkTypeValidity(tmp__533->type_,__FILE__,__LINE__);
										tmp__535 = 1;
										break;
									};
									interrupts_stepCount = tmp__535;
									if ((interrupts_stepCount < 0)) 
									{
										interrupts_microStepCount = (- interrupts_stepCount);
										interrupts_stepCount = (- 1);
									}
									tmp__536 = evaluate_eval(c);
								}
								else
								{
									if ((z_4->message == tokens_continueMessage)) 
									{
										tmp__537 = evaluate_eval(c);
									}
									else
									{
										tmp__537 = e;
									}
									tmp__536 = tmp__537;
								}
								tmp__531 = tmp__536;
							}
							tmp__530 = tmp__531;
						}
						tmp__538 = tmp__530;
						break;
					default:
						checkTypeValidity(z_3->type_,__FILE__,__LINE__);
						tmp__538 = e;
						break;
					};
					tmp__539 = tmp__538;
				}
				else
				{
					tokens_printError(err_27);
					tmp__539 = e;
				}
				tmp__540 = tmp__539;
			}
			else
			{
				tmp__540 = e;
			}
			tmp__541 = tmp__540;
		}
		else
		{
			if ((p_6 != stdiop0_dummyPosition)) 
			{
				err_27->position = p_6;
			}
			tmp__541 = e;
		}
		tmp__542 = tmp__541;
		break;
	default:
		checkTypeValidity(e->type_,__FILE__,__LINE__);
		tmp__542 = e;
		break;
	};
	return tmp__542;
}
static M2_string str__61;
static M2_string str__62;
parse_Expr evaluate_eval(parse_Code c){
	parse_unaryCode u;
	parse_binaryCode b_6;
	parse_adjacentCode b_7;
	parse_Expr left_2;
	parse_FunctionClosure fc_1;
	parse_CompiledFunction ff_10;
	parse_Expr z_5;
	parse_CompiledFunctionClosure ff_11;
	parse_Expr z_6;
	parse_SpecialExpr s_6;
	parse_FunctionClosure fc_2;
	parse_CompiledFunction ff_12;
	parse_Expr z_7;
	parse_CompiledFunctionClosure ff_13;
	parse_Expr z_8;
	parse_functionCode m_1;
	parse_localMemoryReferenceCode r_8;
	parse_Frame f_24;
	int nd;
	parse_globalMemoryReferenceCode r_9;
	parse_threadMemoryReferenceCode r_10;
	int i_8;
	parse_Sequence v_5;
	parse_localAssignmentCode x_35;
	parse_Expr newvalue_2;
	parse_globalAssignmentCode a;
	parse_parallelAssignmentCode p_7;
	parse_globalSymbolClosureCode c_7;
	parse_threadSymbolClosureCode c_8;
	parse_tryCode c_9;
	char oldSuppressErrors;
	parse_Expr p_8;
	parse_Error err_28;
	parse_catchCode c_10;
	parse_Expr p_9;
	parse_Error err_29;
	parse_ifCode c_11;
	parse_Expr p_10;
	parse_localSymbolClosureCode r_11;
	parse_Frame f_25;
	int nd_1;
	parse_ternaryCode b_8;
	parse_multaryCode b_9;
	parse_newLocalFrameCode n_4;
	parse_Expr x_36;
	parse_forCode c_12;
	parse_whileListDoCode c_13;
	parse_whileDoCode c_14;
	parse_whileListCode c_15;
	parse_newCode c_16;
	parse_newOfCode c_17;
	parse_newFromCode c_18;
	parse_newOfFromCode c_19;
	parse_realCode v_6;
	parse_integerCode v_7;
	parse_stringCode v_8;
	parse_Error v_9;
	parse_semiCode v_10;
	parse_CodeSequence w_2;
	int n_5;
	parse_Expr r_12;
	int i_9;
	parse_sequenceCode v_11;
	parse_Sequence r_13;
	parse_listCode v_12;
	parse_Sequence r_14;
	parse_arrayCode v_13;
	parse_Sequence r_15;
	parse_Expr e_9;
	char tmp__543;
	parse_Expr tmp__544;
	parse_Expr tmp__545;
	parse_Expr tmp__546;
	parse_Expr tmp__547;
	parse_Expr tmp__548;
	parse_Expr tmp__549;
	parse_Expr tmp__550;
	parse_Expr tmp__551;
	parse_Expr tmp__552;
	parse_Expr tmp__553;
	parse_Expr tmp__554;
	parse_Expr tmp__555;
	parse_FunctionClosure tmp__556;
	parse_Expr tmp__557;
	parse_Frame tmp__558;
	parse_Frame tmp__559;
	parse_Frame tmp__560;
	parse_Frame tmp__561;
	int tmp__562;
	parse_Expr tmp__563;
	int tmp__564;
	parse_Expr tmp__565;
	int tmp__566;
	parse_Expr tmp__567;
	parse_Expr tmp__568;
	parse_Expr tmp__569;
	parse_Expr tmp__570;
	parse_SymbolClosure tmp__571;
	parse_Expr tmp__572;
	parse_SymbolClosure tmp__573;
	parse_Expr tmp__574;
	parse_Expr tmp__575;
	char tmp__576;
	char tmp__577;
	char tmp__578;
	char tmp__579;
	char tmp__580;
	parse_Expr tmp__581;
	parse_Expr tmp__582;
	parse_Expr tmp__583;
	parse_Expr tmp__584;
	parse_Expr tmp__585;
	parse_Expr tmp__586;
	parse_Expr tmp__587;
	parse_Expr tmp__588;
	parse_Frame tmp__589;
	parse_Frame tmp__590;
	parse_Frame tmp__591;
	parse_Frame tmp__592;
	parse_SymbolClosure tmp__593;
	parse_Expr tmp__594;
	parse_Frame tmp__595;
	int tmp__596;
	parse_Sequence tmp__597;
	int tmp__598;
	parse_Frame tmp__599;
	parse_Expr tmp__600;
	parse_Expr tmp__601;
	gmp_RRcell tmp__602;
	parse_Expr tmp__603;
	gmp_ZZcell tmp__604;
	parse_Expr tmp__605;
	M2_stringCell tmp__606;
	parse_Expr tmp__607;
	int tmp__608;
	parse_Expr tmp__609;
	int tmp__610;
	parse_Expr tmp__611;
	parse_Expr tmp__612;
	int tmp__613;
	parse_Expr tmp__614;
	parse_Expr tmp__615;
	int tmp__616;
	parse_Expr tmp__617;
	parse_Expr tmp__618;
	int tmp__619;
	parse_Expr tmp__620;
	int tmp__621;
	int tmp__622;
	parse_Expr tmp__623;
	parse_Expr tmp__624;
	parse_Expr tmp__625;
	parse_Expr tmp__626;
	parse_Expr tmp__627;
	parse_Expr tmp__628;
	parse_Expr tmp__629;
	tmp__543 = (load_Field((*((struct atomic_field*)TS_Get_Local(interrupts_exceptionFlag_id)))) != ((AO_t)0));
	if (tmp__543) {
		tmp__543 = (!(steppingFurther(c)));
	}
	if (tmp__543) 
	{
		if ((*((char*)TS_Get_Local(interrupts_steppingFlag_id)))) 
		{
			interrupts_clearSteppingFlag();
			tmp__544 = expr_buildErrorPacket(tokens_steppingMessage);
		}
		else
		{
			if ((*((char*)TS_Get_Local(interrupts_alarmedFlag_id)))) 
			{
				interrupts_clearAlarmedFlag();
				tmp__545 = expr_buildErrorPacket(tokens_alarmMessage);
			}
			else
			{
				if ((load_Field((*((struct atomic_field*)TS_Get_Local(interrupts_interruptedFlag_id)))) != ((AO_t)0))) 
				{
					(*((char*)TS_Get_Local(stdiop_SuppressErrors_id))) = 0;
					interrupts_clearInterruptFlag();
					tmp__546 = expr_buildErrorPacket(tokens_interruptMessage);
				}
				else
				{
					(*((char*)TS_Get_Local(stdiop_SuppressErrors_id))) = 0;
					interrupts_clearAllFlags();
					tmp__546 = expr_buildErrorPacket(str__61);
				}
				tmp__545 = tmp__546;
			}
			tmp__544 = tmp__545;
		}
		tmp__547 = tmp__544;
	}
	else
	{
		if (c == 0) invalidNullPointer(__FILE__,__LINE__,-1);
		switch (c->type_) {;
		case unaryCode_typecode:;
			u = ((parse_unaryCode)c);
			tmp__548 = u->f(u->rhs);
			break;
		case binaryCode_typecode:;
			b_6 = ((parse_binaryCode)c);
			tmp__548 = b_6->f(b_6->lhs, b_6->rhs);
			break;
		case adjacentCode_typecode:;
			b_7 = ((parse_adjacentCode)c);
			left_2 = evaluate_eval(b_7->lhs);
			if (left_2 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
			switch (left_2->type_) {;
			case FunctionClosure_typecode:;
				fc_1 = ((parse_FunctionClosure)left_2);
				tmp__549 = evaluate_applyFCC(fc_1, b_7->rhs);
				break;
			case CompiledFunction_typecode:;
				ff_10 = ((parse_CompiledFunction)left_2);
				z_5 = evaluate_eval(b_7->rhs);
				if (z_5 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
				switch (z_5->type_) {;
				case Error_typecode:;
					tmp__550 = z_5;
					break;
				default:
					checkTypeValidity(z_5->type_,__FILE__,__LINE__);
					tmp__550 = ff_10->fn(z_5);
					break;
				};
				tmp__549 = tmp__550;
				break;
			case CompiledFunctionClosure_typecode:;
				ff_11 = ((parse_CompiledFunctionClosure)left_2);
				z_6 = evaluate_eval(b_7->rhs);
				if (z_6 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
				switch (z_6->type_) {;
				case Error_typecode:;
					tmp__551 = z_6;
					break;
				default:
					checkTypeValidity(z_6->type_,__FILE__,__LINE__);
					tmp__551 = ff_11->fn(z_6, ff_11->env);
					break;
				};
				tmp__549 = tmp__551;
				break;
			case SpecialExpr_typecode:;
				s_6 = ((parse_SpecialExpr)left_2);
				tmp__552 = s_6->e;
				if (tmp__552 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
				switch (tmp__552->type_) {;
				case FunctionClosure_typecode:;
					fc_2 = ((parse_FunctionClosure)tmp__552);
					tmp__553 = evaluate_applyFCC(fc_2, b_7->rhs);
					break;
				case CompiledFunction_typecode:;
					ff_12 = ((parse_CompiledFunction)tmp__552);
					z_7 = evaluate_eval(b_7->rhs);
					if (z_7 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
					switch (z_7->type_) {;
					case Error_typecode:;
						tmp__554 = z_7;
						break;
					default:
						checkTypeValidity(z_7->type_,__FILE__,__LINE__);
						tmp__554 = ff_12->fn(z_7);
						break;
					};
					tmp__553 = tmp__554;
					break;
				case CompiledFunctionClosure_typecode:;
					ff_13 = ((parse_CompiledFunctionClosure)tmp__552);
					z_8 = evaluate_eval(b_7->rhs);
					if (z_8 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
					switch (z_8->type_) {;
					case Error_typecode:;
						tmp__555 = z_8;
						break;
					default:
						checkTypeValidity(z_8->type_,__FILE__,__LINE__);
						tmp__555 = ff_13->fn(z_8, ff_13->env);
						break;
					};
					tmp__553 = tmp__555;
					break;
				default:
					checkTypeValidity(tmp__552->type_,__FILE__,__LINE__);
					tmp__553 = evaluate_binarymethod_1(left_2, b_7->rhs, binding_AdjacentS);
					break;
				};
				tmp__549 = tmp__553;
				break;
			case Error_typecode:;
				tmp__549 = left_2;
				break;
			default:
				checkTypeValidity(left_2->type_,__FILE__,__LINE__);
				tmp__549 = evaluate_binarymethod_1(left_2, b_7->rhs, binding_AdjacentS);
				break;
			};
			tmp__548 = tmp__549;
			break;
		case functionCode_typecode:;
			m_1 = ((parse_functionCode)c);
			tmp__556 = (parse_FunctionClosure) GC_MALLOC(sizeof(struct parse_FunctionClosure_struct));
			tmp__556->type_ = 100;
			tmp__556->frame = parse_noRecycle((*((parse_Frame*)TS_Get_Local(expr_localFrame_id))));
			tmp__556->model = m_1;
			tmp__557 = ((parse_Expr)tmp__556);
			return tmp__557;
			break;
		case localMemoryReferenceCode_typecode:;
			r_8 = ((parse_localMemoryReferenceCode)c);
			f_24 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
			nd = r_8->nestingDepth;
			if ((nd == 0)) 
			{
			}
			else
			{
				if ((nd == 1)) 
				{
					tmp__558 = f_24->outerFrame;
					f_24 = tmp__558;
				}
				else
				{
					if ((nd == 2)) 
					{
						tmp__559 = f_24->outerFrame->outerFrame;
						f_24 = tmp__559;
					}
					else
					{
						tmp__560 = f_24->outerFrame->outerFrame->outerFrame;
						f_24 = tmp__560;
						nd = (nd - 3);
					L531_:;
						if ((! (nd > 0))) goto L532_;
						nd = (nd - 1);
						tmp__561 = f_24->outerFrame;
						f_24 = tmp__561;
						goto L531_;
					L532_:;
					}
				}
			}
			tmp__562 = r_8->frameindex;
			if (tmp__562 < 0 || tmp__562 >= f_24->values->len) fatalarrayindex(tmp__562,f_24->values->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1295,33);
			tmp__563 = f_24->values->array[tmp__562];
			return tmp__563;
			break;
		case globalMemoryReferenceCode_typecode:;
			r_9 = ((parse_globalMemoryReferenceCode)c);
			tmp__564 = r_9->frameindex;
			if (tmp__564 < 0 || tmp__564 >= expr_globalFrame->values->len) fatalarrayindex(tmp__564,expr_globalFrame->values->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1296,72);
			tmp__565 = expr_globalFrame->values->array[tmp__564];
			return tmp__565;
			break;
		case threadMemoryReferenceCode_typecode:;
			r_10 = ((parse_threadMemoryReferenceCode)c);
			i_8 = r_10->frameindex;
			v_5 = (*((parse_Frame*)TS_Get_Local(expr_threadFrame_id)))->values;
			if ((i_8 < v_5->len)) 
			{
				tmp__566 = i_8;
				if (tmp__566 < 0 || tmp__566 >= v_5->len) fatalarrayindex(tmp__566,v_5->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1300,39);
				tmp__567 = v_5->array[tmp__566];
			}
			else
			{
				tmp__567 = parse_nullE;
			}
			tmp__568 = tmp__567;
			return tmp__568;
			break;
		case localAssignmentCode_typecode:;
			x_35 = ((parse_localAssignmentCode)c);
			newvalue_2 = evaluate_eval(x_35->rhs);
			if (newvalue_2 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
			switch (newvalue_2->type_) {;
			case Error_typecode:;
				tmp__569 = newvalue_2;
				return tmp__569;
				break;
			default:
				checkTypeValidity(newvalue_2->type_,__FILE__,__LINE__);
				tmp__570 = localAssignment(x_35->nestingDepth, x_35->frameindex, newvalue_2);
				break;
			};
			tmp__548 = tmp__570;
			break;
		case globalAssignmentCode_typecode:;
			a = ((parse_globalAssignmentCode)c);
			tmp__548 = globalAssignmentFun(a);
			break;
		case parallelAssignmentCode_typecode:;
			p_7 = ((parse_parallelAssignmentCode)c);
			tmp__548 = parallelAssignmentFun(p_7);
			break;
		case globalSymbolClosureCode_typecode:;
			c_7 = ((parse_globalSymbolClosureCode)c);
			tmp__571 = (parse_SymbolClosure) GC_MALLOC(sizeof(struct parse_SymbolClosure_struct));
			tmp__571->type_ = 9;
			tmp__571->frame = expr_globalFrame;
			tmp__571->symbol = c_7->symbol;
			tmp__572 = ((parse_Expr)tmp__571);
			return tmp__572;
			break;
		case threadSymbolClosureCode_typecode:;
			c_8 = ((parse_threadSymbolClosureCode)c);
			tmp__573 = (parse_SymbolClosure) GC_MALLOC(sizeof(struct parse_SymbolClosure_struct));
			tmp__573->type_ = 9;
			tmp__573->frame = (*((parse_Frame*)TS_Get_Local(expr_threadFrame_id)));
			tmp__573->symbol = c_8->symbol;
			tmp__574 = ((parse_Expr)tmp__573);
			return tmp__574;
			break;
		case tryCode_typecode:;
			c_9 = ((parse_tryCode)c);
			oldSuppressErrors = (*((char*)TS_Get_Local(stdiop_SuppressErrors_id)));
			(*((char*)TS_Get_Local(stdiop_SuppressErrors_id))) = 1;
			p_8 = evaluate_eval(c_9->code);
			if ((!((*((char*)TS_Get_Local(stdiop_SuppressErrors_id)))))) 
			{
				tmp__575 = p_8;
			}
			else
			{
				(*((char*)TS_Get_Local(stdiop_SuppressErrors_id))) = oldSuppressErrors;
				if (p_8 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
				switch (p_8->type_) {;
				case Error_typecode:;
					err_28 = ((parse_Error)p_8);
					tmp__580 = (err_28->message == tokens_breakMessage);
					if ((! tmp__580)) {
						tmp__580 = (err_28->message == tokens_returnMessage);
					}
					tmp__579 = tmp__580;
					if ((! tmp__579)) {
						tmp__579 = (err_28->message == tokens_continueMessage);
					}
					tmp__578 = tmp__579;
					if ((! tmp__578)) {
						tmp__578 = (err_28->message == tokens_continueMessageWithArg);
					}
					tmp__577 = tmp__578;
					if ((! tmp__577)) {
						tmp__577 = (err_28->message == tokens_unwindMessage);
					}
					tmp__576 = tmp__577;
					if ((! tmp__576)) {
						tmp__576 = (err_28->message == tokens_throwMessage);
					}
					if (tmp__576) 
					{
						tmp__581 = p_8;
					}
					else
					{
						tmp__581 = evaluate_eval(c_9->elseClause);
					}
					tmp__582 = tmp__581;
					break;
				default:
					checkTypeValidity(p_8->type_,__FILE__,__LINE__);
					if ((c_9->thenClause == expr_NullCode)) 
					{
						tmp__583 = p_8;
					}
					else
					{
						tmp__583 = evaluate_eval(c_9->thenClause);
					}
					tmp__582 = tmp__583;
					break;
				};
				tmp__575 = tmp__582;
			}
			tmp__548 = tmp__575;
			break;
		case catchCode_typecode:;
			c_10 = ((parse_catchCode)c);
			p_9 = evaluate_eval(c_10->code);
			if (p_9 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
			switch (p_9->type_) {;
			case Error_typecode:;
				err_29 = ((parse_Error)p_9);
				if ((err_29->message == tokens_throwMessage)) 
				{
					tmp__584 = err_29->value;
				}
				else
				{
					tmp__584 = p_9;
				}
				tmp__585 = tmp__584;
				break;
			default:
				checkTypeValidity(p_9->type_,__FILE__,__LINE__);
				tmp__585 = p_9;
				break;
			};
			tmp__548 = tmp__585;
			break;
		case ifCode_typecode:;
			c_11 = ((parse_ifCode)c);
			p_10 = evaluate_eval(c_11->predicate);
			if (p_10 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
			switch (p_10->type_) {;
			case Error_typecode:;
				tmp__586 = p_10;
				break;
			default:
				checkTypeValidity(p_10->type_,__FILE__,__LINE__);
				if ((p_10 == parse_True)) 
				{
					tmp__587 = evaluate_eval(c_11->thenClause);
				}
				else
				{
					if ((p_10 == parse_False)) 
					{
						tmp__588 = evaluate_eval(c_11->elseClause);
					}
					else
					{
						tmp__588 = common_printErrorMessageE(c_11->predicate, str__62);
					}
					tmp__587 = tmp__588;
				}
				tmp__586 = tmp__587;
				break;
			};
			tmp__548 = tmp__586;
			break;
		case localSymbolClosureCode_typecode:;
			r_11 = ((parse_localSymbolClosureCode)c);
			f_25 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
			nd_1 = r_11->nestingDepth;
			if ((nd_1 == 0)) 
			{
			}
			else
			{
				if ((nd_1 == 1)) 
				{
					tmp__589 = f_25->outerFrame;
					f_25 = tmp__589;
				}
				else
				{
					if ((nd_1 == 2)) 
					{
						tmp__590 = f_25->outerFrame->outerFrame;
						f_25 = tmp__590;
					}
					else
					{
						tmp__591 = f_25->outerFrame->outerFrame->outerFrame;
						f_25 = tmp__591;
						nd_1 = (nd_1 - 3);
					L549_:;
						if ((! (nd_1 > 0))) goto L550_;
						nd_1 = (nd_1 - 1);
						tmp__592 = f_25->outerFrame;
						f_25 = tmp__592;
						goto L549_;
					L550_:;
					}
				}
			}
			parse_noRecycle(f_25);
			tmp__593 = (parse_SymbolClosure) GC_MALLOC(sizeof(struct parse_SymbolClosure_struct));
			tmp__593->type_ = 9;
			tmp__593->frame = f_25;
			tmp__593->symbol = r_11->symbol;
			tmp__594 = ((parse_Expr)tmp__593);
			return tmp__594;
			break;
		case ternaryCode_typecode:;
			b_8 = ((parse_ternaryCode)c);
			tmp__548 = b_8->f(b_8->arg1, b_8->arg2, b_8->arg3);
			break;
		case multaryCode_typecode:;
			b_9 = ((parse_multaryCode)c);
			tmp__548 = b_9->f(b_9->args);
			break;
		case newLocalFrameCode_typecode:;
			n_4 = ((parse_newLocalFrameCode)c);
			tmp__596 = 0;
			tmp__598 = n_4->framesize;
			if (0 > tmp__598) fatalarraylen(tmp__598,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1349,92);
			tmp__597 = (parse_Sequence) GC_MALLOC(sizeof(struct parse_Sequence_struct) + (tmp__598 - 1)*sizeof(parse_Expr));
			tmp__597->type_ = 6;
			tmp__597->len = tmp__598;
			if ((tmp__598 == 0)) goto L553_;
		L552_:;
			tmp__597->array[tmp__596] = parse_nullE;
			if (((++ tmp__596) < tmp__598)) goto L554_;
			goto L553_;
		L554_:;
			goto L552_;
		L553_:;
			tmp__595 = (parse_Frame) GC_MALLOC(sizeof(struct parse_Frame_struct));
			tmp__595->outerFrame = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
			tmp__595->frameID = n_4->frameID;
			tmp__595->valuesUsed = n_4->framesize;
			tmp__595->notrecyclable = 0;
			tmp__595->values = tmp__597;
			(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = tmp__595;
			x_36 = evaluate_eval(n_4->body);
			tmp__599 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)))->outerFrame;
			(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = tmp__599;
			tmp__548 = x_36;
			break;
		case forCode_typecode:;
			c_12 = ((parse_forCode)c);
			tmp__600 = evalForCode(c_12);
			return tmp__600;
			break;
		case whileListDoCode_typecode:;
			c_13 = ((parse_whileListDoCode)c);
			tmp__548 = evalWhileListDoCode(c_13);
			break;
		case whileDoCode_typecode:;
			c_14 = ((parse_whileDoCode)c);
			tmp__548 = evalWhileDoCode(c_14);
			break;
		case whileListCode_typecode:;
			c_15 = ((parse_whileListCode)c);
			tmp__548 = evalWhileListCode(c_15);
			break;
		case newCode_typecode:;
			c_16 = ((parse_newCode)c);
			tmp__548 = convertr_NewFun(c_16->newClause);
			break;
		case newOfCode_typecode:;
			c_17 = ((parse_newOfCode)c);
			tmp__548 = convertr_NewOfFun(c_17->newClause, c_17->ofClause);
			break;
		case newFromCode_typecode:;
			c_18 = ((parse_newFromCode)c);
			tmp__548 = convertr_NewFromFun(c_18->newClause, c_18->fromClause);
			break;
		case newOfFromCode_typecode:;
			c_19 = ((parse_newOfFromCode)c);
			tmp__548 = convertr_NewOfFromFun(c_19->newClause, c_19->ofClause, c_19->fromClause);
			break;
		case nullCode_typecode:;
			tmp__601 = parse_nullE;
			return tmp__601;
			break;
		case realCode_typecode:;
			v_6 = ((parse_realCode)c);
			tmp__602 = (gmp_RRcell) GC_MALLOC(sizeof(struct gmp_RRcell_struct));
			tmp__602->type_ = 82;
			tmp__602->v = v_6->x;
			tmp__603 = ((parse_Expr)tmp__602);
			return tmp__603;
			break;
		case integerCode_typecode:;
			v_7 = ((parse_integerCode)c);
			tmp__604 = (gmp_ZZcell) GC_MALLOC(sizeof(struct gmp_ZZcell_struct));
			tmp__604->type_ = 79;
			tmp__604->v = v_7->x;
			tmp__605 = ((parse_Expr)tmp__604);
			return tmp__605;
			break;
		case stringCode_typecode:;
			v_8 = ((parse_stringCode)c);
			tmp__606 = (M2_stringCell) GC_MALLOC(sizeof(struct M2_stringCell_struct));
			tmp__606->type_ = 17;
			tmp__606->v = v_8->x;
			tmp__607 = ((parse_Expr)tmp__606);
			return tmp__607;
			break;
		case Error_typecode:;
			v_9 = ((parse_Error)c);
			tmp__548 = ((parse_Expr)v_9);
			break;
		case semiCode_typecode:;
			v_10 = ((parse_semiCode)c);
			w_2 = v_10->w;
			n_5 = w_2->len;
			tmp__608 = 0;
			if (tmp__608 < 0 || tmp__608 >= w_2->len) fatalarrayindex(tmp__608,w_2->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1369,27);
			r_12 = evaluate_eval(w_2->array[tmp__608]);
			if (r_12 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
			switch (r_12->type_) {;
			case Error_typecode:;
				tmp__609 = r_12;
				break;
			default:
				checkTypeValidity(r_12->type_,__FILE__,__LINE__);
				tmp__610 = 1;
				if (tmp__610 < 0 || tmp__610 >= w_2->len) fatalarrayindex(tmp__610,w_2->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1371,31);
				r_12 = evaluate_eval(w_2->array[tmp__610]);
				if (r_12 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
				switch (r_12->type_) {;
				case Error_typecode:;
					tmp__611 = r_12;
					break;
				default:
					checkTypeValidity(r_12->type_,__FILE__,__LINE__);
					if ((n_5 == 2)) 
					{
						tmp__612 = r_12;
						return tmp__612;
					}
					tmp__613 = 2;
					if (tmp__613 < 0 || tmp__613 >= w_2->len) fatalarrayindex(tmp__613,w_2->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1374,36);
					r_12 = evaluate_eval(w_2->array[tmp__613]);
					if (r_12 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
					switch (r_12->type_) {;
					case Error_typecode:;
						tmp__614 = r_12;
						break;
					default:
						checkTypeValidity(r_12->type_,__FILE__,__LINE__);
						if ((n_5 == 3)) 
						{
							tmp__615 = r_12;
							return tmp__615;
						}
						tmp__616 = 3;
						if (tmp__616 < 0 || tmp__616 >= w_2->len) fatalarrayindex(tmp__616,w_2->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1377,41);
						r_12 = evaluate_eval(w_2->array[tmp__616]);
						if (r_12 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
						switch (r_12->type_) {;
						case Error_typecode:;
							tmp__617 = r_12;
							break;
						default:
							checkTypeValidity(r_12->type_,__FILE__,__LINE__);
							if ((n_5 == 4)) 
							{
								tmp__618 = r_12;
								return tmp__618;
							}
							tmp__619 = 4;
							if (tmp__619 < 0 || tmp__619 >= w_2->len) fatalarrayindex(tmp__619,w_2->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1380,46);
							r_12 = evaluate_eval(w_2->array[tmp__619]);
							if (r_12 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
							switch (r_12->type_) {;
							case Error_typecode:;
								tmp__620 = r_12;
								break;
							default:
								checkTypeValidity(r_12->type_,__FILE__,__LINE__);
								i_9 = 5;
							L558_:;
								if ((! (i_9 < n_5))) goto L559_;
								tmp__621 = i_9;
								if (tmp__621 < 0 || tmp__621 >= w_2->len) fatalarrayindex(tmp__621,w_2->len,"/home/gfurnish/M2/gfurnish/scons/M2/Macaulay2/d/evaluate.dd",1384,56);
								r_12 = evaluate_eval(w_2->array[tmp__621]);
								if (r_12 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
								switch (r_12->type_) {;
								case Error_typecode:;
									tmp__622 = n_5;
									break;
								default:
									checkTypeValidity(r_12->type_,__FILE__,__LINE__);
									tmp__622 = (i_9 + 1);
									break;
								};
								i_9 = tmp__622;
								goto L558_;
							L559_:;
								tmp__620 = r_12;
								break;
							};
							tmp__617 = tmp__620;
							break;
						};
						tmp__614 = tmp__617;
						break;
					};
					tmp__611 = tmp__614;
					break;
				};
				tmp__609 = tmp__611;
				break;
			};
			tmp__548 = tmp__609;
			break;
		case sequenceCode_typecode:;
			v_11 = ((parse_sequenceCode)c);
			if ((v_11->x->len == 0)) 
			{
				tmp__623 = ((parse_Expr)expr_emptySequence);
				return tmp__623;
			}
			r_13 = evaluate_evalSequence(v_11->x);
			if (evaluate_evalSequenceHadError) 
			{
				tmp__624 = evaluate_evalSequenceErrorMessage;
			}
			else
			{
				tmp__624 = ((parse_Expr)r_13);
			}
			tmp__548 = tmp__624;
			break;
		case listCode_typecode:;
			v_12 = ((parse_listCode)c);
			if ((v_12->y->len == 0)) 
			{
				tmp__625 = basic_emptyList;
				return tmp__625;
			}
			r_14 = evaluate_evalSequence(v_12->y);
			if (evaluate_evalSequenceHadError) 
			{
				tmp__626 = evaluate_evalSequenceErrorMessage;
			}
			else
			{
				tmp__626 = basic_list(r_14);
			}
			tmp__548 = tmp__626;
			break;
		case arrayCode_typecode:;
			v_13 = ((parse_arrayCode)c);
			if ((v_13->z->len == 0)) 
			{
				tmp__627 = basic_emptyArray;
				return tmp__627;
			}
			r_15 = evaluate_evalSequence(v_13->z);
			if (evaluate_evalSequenceHadError) 
			{
				tmp__628 = evaluate_evalSequenceErrorMessage;
			}
			else
			{
				tmp__628 = basic_Array(r_15);
			}
			tmp__548 = tmp__628;
			break;
		};
		tmp__547 = tmp__548;
	}
	e_9 = tmp__547;
	if (e_9 == 0) invalidNullPointer(__FILE__,__LINE__,-1);
	switch (e_9->type_) {;
	case Error_typecode:;
		tmp__629 = handleError(c, e_9);
		break;
	default:
		checkTypeValidity(e_9->type_,__FILE__,__LINE__);
		tmp__629 = e_9;
		break;
	};
	return tmp__629;
}
static M2_string str__63;
static M2_string str__64;
parse_Expr evaluate_evalexcept(parse_Code c){
	parse_Expr e_10;
	parse_Expr tmp__630;
	parse_Expr tmp__631;
	parse_Expr tmp__632;
	parse_Expr tmp__633;
	e_10 = evaluate_eval(c);
	if ((load_Field((*((struct atomic_field*)TS_Get_Local(interrupts_exceptionFlag_id)))) != ((AO_t)0))) 
	{
		if ((*((char*)TS_Get_Local(interrupts_alarmedFlag_id)))) 
		{
			interrupts_clearAlarmedFlag();
			tmp__630 = common_printErrorMessageE(c, tokens_alarmMessage);
		}
		else
		{
			if ((load_Field((*((struct atomic_field*)TS_Get_Local(interrupts_interruptedFlag_id)))) != ((AO_t)0))) 
			{
				(*((char*)TS_Get_Local(stdiop_SuppressErrors_id))) = 0;
				interrupts_clearInterruptFlag();
				tmp__631 = common_printErrorMessageE(c, tokens_interruptMessage);
			}
			else
			{
				if ((*((char*)TS_Get_Local(interrupts_steppingFlag_id)))) 
				{
					interrupts_clearSteppingFlag();
					common_printErrorMessageE(c, str__63);
					tmp__632 = e_10;
				}
				else
				{
					(*((char*)TS_Get_Local(stdiop_SuppressErrors_id))) = 0;
					interrupts_clearAllFlags();
					tmp__632 = common_printErrorMessageE(c, str__64);
				}
				tmp__631 = tmp__632;
			}
			tmp__630 = tmp__631;
		}
		tmp__633 = tmp__630;
	}
	else
	{
		tmp__633 = e_10;
	}
	return tmp__633;
}
parse_Expr evaluate_eval_1(parse_Frame f,parse_Code c){
	parse_Frame saveLocalFrame_14;
	parse_Expr ret_16;
	saveLocalFrame_14 = (*((parse_Frame*)TS_Get_Local(expr_localFrame_id)));
	(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = f;
	ret_16 = evaluate_evalexcept(c);
	(*((parse_Frame*)TS_Get_Local(expr_localFrame_id))) = saveLocalFrame_14;
	return ret_16;
}
static M2_string str__65;
static parse_Expr shieldfun(parse_Code a){
	parse_Expr ret_17;
	parse_Expr tmp__634;
	char tmp__635;
	if ((*((char*)TS_Get_Local(interrupts_interruptShield_id)))) 
	{
		tmp__634 = evaluate_eval(a);
	}
	else
	{
		(*((char*)TS_Get_Local(interrupts_interruptPending_id))) = (load_Field((*((struct atomic_field*)TS_Get_Local(interrupts_interruptedFlag_id)))) != ((AO_t)0));
		(*((char*)TS_Get_Local(interrupts_interruptShield_id))) = 1;
		ret_17 = evaluate_eval(a);
		(*((char*)TS_Get_Local(interrupts_interruptShield_id))) = 0;
		store_Field((*((struct atomic_field*)TS_Get_Local(interrupts_interruptedFlag_id))),((AO_t)(*((char*)TS_Get_Local(interrupts_interruptPending_id)))));
		interrupts_determineExceptionFlag();
		tmp__635 = (load_Field((*((struct atomic_field*)TS_Get_Local(interrupts_interruptedFlag_id)))) != ((AO_t)0));
		if (tmp__635) {
			tmp__635 = (!(stdio_stdIO->inisatty));
		}
		if (tmp__635) 
		{
			stdio_endLine(stdio_stdError);
			stdio_less_less__13(errio_less_less__1((*((errio_BasicFile*)TS_Get_Local(errio_stderr_id))), str__65), stdio_endl);
			system_exit(1);
		}
		tmp__634 = ret_17;
	}
	return tmp__634;
}
static parse_Expr returnFun(parse_Code a)
{
	if (a == expr_dummyCode)
		e = parse_nullE;
	else
		e = evaluate_eval(a);
	if(M2CPP_IsError(e))
		return e;
	parse_Error tmp__638 = M2CPP_NewObject<parse_Error,struct parse_Error_struct>(Error_typecode);
	tmp__638->position = stdiop0_dummyPosition;
	tmp__638->message = tokens_returnMessage;
	tmp__638->value = e_11;
	tmp__638->printed = 0;
	tmp__638->frame = expr_dummyFrame;
	return reinterpret_cast<parse_Expr>(tmp__638);
}
static parse_Expr throwFun(parse_Code a)
{
	parse_Expr e = evaluate_eval(a);
	if(M2CPP_IsError(e))
		return e;
	parse_Error tmp__640 = M2CPP_NewObject(Error_typecode);
	tmp__640->position = stdiop0_dummyPosition;
	tmp__640->message = tokens_throwMessage;
	tmp__640->value = e_12;
	tmp__640->printed = 0;
	tmp__640->frame = expr_dummyFrame;
	return reinterpret_cast<parse_Expr>(tmp__640);
}
static parse_Expr continueFun(parse_Code a)
{
	parse_Expr e;
	if (a == expr_dummyCode)
		e = parse_nullE;
	else
		e = evaluate_eval(a);
	if(M2CPP_IsError(e))
		return e;
	parse_Error tmp__643 = M2CPP_NewObject(Error_typecode);
	tmp__643->position = stdiop0_dummyPosition;
	if(a==expr_dummyCode)
		tmp__643->message = tokens_continueMessage;
	else
		tmp__643->message = tokens_continueMessageWithArg;
	tmp__643->value = e;
	tmp__643->printed = 0;
	tmp__643->frame = expr_dummyFrame;
	return reinterpret_cast<parse_Expr>(tmp__643);
}
static parse_Expr stepFun(parse_Code a)
{
	parse_Expr e;
	if (a == expr_dummyCode)
		e = parse_nullE;
	else
		e = evaluate_eval(a);
	if(M2CPP_IsError(e))
		return e;
	parse_Error tmp__647 = M2CPP_NewObject(Error_typecode);
	tmp__647->position = stdiop0_dummyPosition;
	if (a == expr_dummyCode)
		tmp__647->message = tokens_stepMessage;
	else
		tmp__647->message = tokens_stepMessageWithArg;
	tmp__647->value = e;
	tmp__647->printed = 0;
	tmp__647->frame = expr_dummyFrame;
	return reinterpret_cast<parse_Expr>(tmp__647);
}
static parse_Expr breakFun(parse_Code a)
{
	parse_Expr e;
	if (a == expr_dummyCode)
		e = parse_dummyExpr;
	else
	    e = evaluate_eval(a);
	if(M2CPP_IsError(e))
		return e;
	parse_Error tmp__651 = M2CPP_NewObject(Error_typecode);
	tmp__651->position = stdiop0_dummyPosition;
	tmp__651->message = tokens_breakMessage;
	tmp__651->value = e;
	tmp__651->printed = 0;
	tmp__651->frame = expr_dummyFrame;
	tmp__650 = ((parse_Expr)tmp__651);
	break;
}
static parse_Expr assigntofun(parse_Code lhs,parse_Code rhs)
	assert(lhs && rhs);
	parse_Expr left = evaluate_eval(lhs);
	if(M2CPP_isError(left))
		return left;
	if(M2CPP_IsTypeExact(left,SymbolClosure_typecode))
	{
		parse_SymbolClosure q = reinterpret_cast<parse_SymbolClosure>(left);
		if(q->symbol->Protected)
		{
			M2_string str__66 = M2CPP_NewConstString("assignment to protected variable '");
			M2_string str__67 = M2CPP_NewConstString("'");
			return expr_buildErrorPacket(strings_plus_(strings_plus_(str__66, q->symbol->word->name), str__67));
		}
		parse_Expr value = evaluate_eval(rhs);
		if(M2CPP_IsError(value))
			return value;
		expr_enlargeThreadFrame();
		assert(q->symbol->frameindex < q->frame->values->len);
		q->frame->values->array[q->symbol->frameindex] = value;
		return value;
	}
	//method for x <- y is looked up under (symbol <-, class x)
	parse_Expr method = hashtables_lookup_1(classes_Class(left), binding_LeftArrowE);
	if (method == parse_nullE)
		return expr_buildErrorPacket(M2CPP_NewConstString("'<-': no method for object on left"));
	parse_Expr value = evaluate_eval(rhs);
	if(M2CPP_IsError(value))
		return value;
	return evaluate_applyEEE(method, left, value);
}
static parse_Expr idfun(parse_Expr e)
{
	return e;
}
static parse_Expr scanpairs(parse_Expr f,parse_HashTable obj)
{
	assert(f && obj);
	for(size_t i = 0; i < obj->table->len; ++i)
	{
		parse_KeyValuePair p = obj->table->array[i];
		while(p!=p->next)
		{
			parse_Expr v = evaluate_applyEEE(f,p->key,p->value);
			if(M2CPP_IsError(v))
				return v;
			p = p->next;
		}
	}
	return parse_nullE;
}
static parse_Expr scanpairsfun(parse_Expr e)
{
	assert(e);
	if(!M2CPP_IsTypeExact(e,Sequence_typecode))
		return expr_WrongNumArgs_1(2);
	parse_Sequence a = reinterpret_cast<parse_Sequence>(e);
	if(a->len!=2)
		return expr_WrongNumArgs_1(2);
	if(!M2CPP_IsTypeExact(a->array[0],HashTable_typecode))
		return expr_WrongArg_1(1,M2CPP_NewObject("a hash table"));
	parse_HashTable o = reinterpret_cast<parse_HashTable>(o);
	if(o->Mutable)
		return expr_WrongArg_1(1,M2CPP_NewConstString("an immutable hash table"));
	return scanpairs(a_1->array[1], o);
}
static parse_Expr mpre()
{
	return expr_buildErrorPacket(M2CPP_NewConstString("applyPairs: expected function to return null, a sequence of length 2, or an option x=>y"));
}
static parse_Expr mappairs(parse_Expr f,parse_HashTable o)
{
	assert(f && o);
	parse_HashTable x = expr_newHashTable(o->Class, o->parent);
	x->beingInitialized = 1;
	for(size_t i = 0; i < o->table->len; ++i)
	{
		parse_KeyValuePair p = o->table->array[i];
		while(p!=p->next)
		{
			parse_Expr v = evaluate_applyEEE(f, p->key, p->value);
			if(M2CPP_IsError(v))
				return v;
			switch(M2CPP_Type(v)) {
			case Nothing_typecode:
				break;
			case List_typecode:
				{
					parse_List b = reinterpret_cast<parse_List>(v);
					if(b->Class != expr_optionClass)
						return mpre();
					parse_Sequence a = b->v;
					if(a->len!=2)
						return mpre();
					parse_Expr tmp__678 = hashtables_storeInHashTable_1(x, a_2->array[0], a_2->array[1]);
					if(M2CPP_IsError(tmp__676))
						return tmp__678;
					break;
				}
			case Sequence_typecode:
				{
					parse_Sequence a = reinterpret_cast<parse_Sequence>(v);
					if(a->len!=2)
						return mpre();
					parse_Expr ret = hashtables_storeInHashTable_1(x,a->array[0],a->array[1]);
					if(M2CPP_IsError(ret))
						return ret;
					break;
				}
			default:
				return mpre();
			};
			p = p->next;
		}
	}
	return reinterpret_cast<parse_Expr>(hashtables_sethash(x, o->Mutable));


}
static parse_Expr mappairsfun(parse_Expr e)
{
	assert(e);
	if(!M2CPP_IsTypeExact(e,Sequence_typecode))
		return expr_WrongNumArgs_1(2);
	parse_Sequence a = ((parse_Sequence)e);
	if(a->len!=2)
		return expr_WrongNumArgs_1(2);
	if(!M2CPP_IsTypeExact(a->array[0],HashTable_typecode))
		return expr_WrongArg_1(1,M2CPP_NewConstString("a hash table"));
	parse_HashTable o  = reinterpret_cast<parse_HashTable>(a->array[0]);
	if(o->Mutable)
		return expr_WrongArg(M2CPP_NewConstString("an immutable hash table"));
	return mappairs(a->array[1],o);
}
parse_Expr evaluate_mapkeys(parse_Expr f,parse_HashTable o)
{
	assert(f && o);
	parse_HashTable x = expr_newHashTable(o->Class, o->parent);
	x->beingInitialized = 1;
	for(size_t i = 0; i < o->table->len; ++i)
	{
		parse_KeyValuePair p = o->table->array[i];
		while(p!=p->next)
		{
			parse_Expr newkey = evaluate_applyEE(f,p->key);
			if(newkey == parse_nullE)
				//remove this error soon???
				return expr_buildErrorPacket(M2CPP_NewConstString("null key encountered"));
			if(M2CPP_IsError(newkey))
				return newkey;
		}
		parse_Expr tmp__698 = hashtables_storeInHashTableNoClobber_1(x,newkey,p->value);
		if(M2CPP_IsError(tmp__698))
			return tmp__698;
		p = p->next;
	}
	return reinterpret_cast<parse_Expr>(hashtables_sethash(x,o->Mutable));
}
static parse_Expr mapkeysfun(parse_Expr e)
{
	assert(e);
	if(!M2CPP_IsTypeExact(e,Sequence_typecode))
		return expr_WrongNumArgs_1(2);
	parse_Sequence a = reinterpret_cast<parse_Sequence>(e);
	if (!a->len == 2)
		return expr_WrongNumArgs_1(2);
	if(!M2CPP_IsTypeExact(a->array[0],HashTable_typecode))
		return expr_WrongArg_1(1, M2CPP_NewConstString("a hash table"));
	parse_HashTable o = reinterpret_cast<parse_HashTable>(a->array[0]);
	if(o->Mutable)
		return expr_WrongArg("an immutable hash table");
	return evaluate_mapkeys(a->array[1], o);
}
parse_Expr evaluate_mapvalues(parse_Expr f,parse_HashTable o)
{
	assert(f && x);
	parse_HashTable x = expr_newHashTable(o->Class, o->parent);
	x->beingInitialized = 1;
	bool hadError = false;
	parse_Expr errm = parse_nullE;
	x->numEntries = o->numEntries;
	
	struct SCC_M2_0_int_len_parse_KeyValuePair_array1 * tmp__709 = M2CPP_NewArray<struct SCC_M2_0_int_len_parse_KeyValuePair_array1,struct SCC_M2_0_int_len_parse_KeyValuePair_array1,parse_KeyValuePair>(o->table->len);
	for(size_t i  0; i < o->table->len; ++i)
	{
		parse_KeyValuePair p = o->table->array[i];
		parse_KeyValuePair q = expr_bucketEnd;
		while(p!=p->next)
		{
			parse_Expr newvalue = applyEE(f,p->value);
			if(M2CPP_IsError(newvalue))
				return errm;
			q = M2CPP_KeyValuePair(p->key,p->hash,newvalue,q);
			p = p->next;
		}
		tmp__709->array[i]=q;
	}
	x->table = tmp__709;
	return reinterpret_cast<parse_Expr>(hashtables_sethash(x, o->Mutable));
}
static parse_Expr mapvaluesfun(parse_Expr e)
{
	if(M2CPP_IsTypeExact(e,Sequence_typecode))
		return expr_WrongNumArgs_1(2);
	parse_Sequence a = reinterpret_cast<parse_Sequence>(e);
	if (a->len != 2)
		return expr_WrongNumArgs_1(2);
	if(!M2CPP_IsTypeExact(a->array[0],HashTable_typecode))
		return expr_WrongArg_1(1,M2CPP_NewConstString("a hash table"));
	parse_HashTable o = reinterpret_cast<parse_HashTable>(a->array[0]);
	if(o->Mutable)
		return expr_WrongArg_1(1,M2CPP_NewConstString("an immutable hash table"));
	else
		return evaluate_mapvalues(a->array[1],o);
}
static parse_Expr merge(parse_Expr e)
{
	assert(e);
	if(!M2CPP_IsTypeExact(e,Sequence_typecode))
		return expr_WrongNumArgs_1(3);
	parse_Sequence v = reinterpret_cast<parse_Sequence>(e);
	if(v->len != 3)
		return expr_WrongNumArgs_1(3);
	parse_Expr g = v->array[2];
	if(!M2CPP_IsTypeExact(v->array[0],HashTable_typecode))
		return expr_WrongArg(1,M2CPP_NewConstString("a hash table"));
	parse_Expr x = reinterpret_cast<parse_HashTable>(v->array[0]);
	if(x->Mutable)
		return expr_WrongArg(1,M2CPP_NewConstString("an immutable hash table"));
	if(!M2CPP_IsTypeExact(v->array[1],HashTable_typecode))
		return expr_WrongArg(2,M2CPP_NewConstString("a hash table"));
	parse_Expr y = reinterpret_cast<parse_HashTable>(v->array[1]);
	if(y->Mutable)
		return expr_WrongArg(2,M2CPP_NewConstString("an immutable hash table"));
	
	if (x->table->len < y->table->len)
	{
		parse_HashTable tmp = x;
		x = y;
		y = tmp;
	}
	parse_HashTable z = hashtables_copy(x);
	z->Mutable = 1;
	z->beingInitialized = 1;
	for(size_t i = 0; i < y->table->len; ++i)
	{
		parse_KeyValuePair q = y->table->array[i];
		while(q!=q->next)
		{
			parse_Expr val = hashtables_lookup1(z,q->key,q->hash);
			if(val != parse_notfoundE)
			{
				parse_Expr t = evaluate_applyEEE(g,val,q->value);
				if(M2CPP_IsError(t))
				{
					parse_Error err = reinterpret_cast<parse_Error>(t);
					if(err->message != tokens_continueMessage)
						return t;
					else
						hashtables_remove(z,q->key);
				}
				else
					hashtables_storeInHashTable(z,q->key,q->hash,t);
			}
			else
				hashtables_storeInHashTable(z,q->key,q->hash,2->value);
			q=q->next;
		}
	}
	bool mut = false;
	if(x->Class == y->Class && x->parent == y->parent)
	{
		z->Class = x->Class;
		z->parent = x->parent;
		mut = x->Mutable;
	}
	else
	{
		z->Class = expr_hashTableClass;
		z->parent = expr_nothingClass;
	}
	return reinterpret_cast<parse_Expr>(hashtables_sethash(z,mut));
}

static parse_Expr combine(parse_Expr f,parse_Expr g,parse_Expr h,parse_HashTable x,parse_HashTable y)
{
	parse_HashTable z = expr_newHashTable(x->Class, x->parent);
	z_11->beingInitialized = 1;
	for(size_t i = 0; i < x->table->len; ++x)
	{
		parse_KeyValuePair p = x->table->array[i];
		while(p!=p->next)
		{
			for(size_t j = 0; j < y->table->len; ++j)
			{
				parse_KeyValuePair q = y->table->array[j];
				while(q!=q->next)
				{
					parse_Expr pqkey = evaluate_applyEEE(f,p->key,q->key);
					if(M2CPP_IsError(pqkey))
						if(reinterpret_cast<parse_Error>(pqkey)->message != tokens_continueMessage)
							return pqkey;
						else
							continue;
					parse_Expr pqvalue = evaluate_applyEEE(g,p->value,q->value);
					if(M2CPP_IsError(pqvalue))
						if(reinterpret_cast<parse_Error>(pqvalue)->message !=tokens_continueMessage)
							return pqvalue;
						else
							continue;
					int pqhash = basic_hash(pqkey);
					parse_Expr previous = hashtables_lookup1(z, pqkey, pqhash);
					if (previous == parse_notfoundE)
					{
						parse_Expr r = hashtables_storeInHashTable(z, pqkey, pqhash, pqvalue);
						if(M2CPP_IsError(r))
							return r;
					}
					else
					{
						parse_Expr t = evaluate_applyEEE(h, previous, pqvalue);
						if(M2CPP_IsError(t))
							if(reinterpret_cast<parse_Error>(t)->message !=tokens_continueMessage)
								return t;
							else
							{
								hashtables_remove (z,pqkey);
								continue;
							}
						parse_Expr r = hashtables_storeInHashTable(z,pqkey,pqhash,t);
						if(M2CPP_IsError(r))
							return r;
					}
					q = q->next;
				}
			}
			p = p->next;
		}
	}
	return reinterpret_cast<parse_Expr>(hashtables_sethash(z, x->Mutable|y->Mutable));
}
static parse_Expr combine_1(parse_Expr e)
{
	assert(e);
	if(!M2CPP_IsTypeExact(e,Sequence_typecode))
		return expr_WrongNumArgs_1(5);
	parse_Sequence v = reinterpret_cast<parse_Sequence>(e);
	if (v->len != 5)
		return expr_WrongNumArgs_1(5);
	if(M2CPP_Type(v->array[0])!=HashTable_typecode)
		return expr_WrongArg_1(1, M2CPP_NewConstString("a hash table"));
	if(M2CPP_Type(v->array[1])!=HashTable_typecode)
		return expr_WrongArg_1(2, M2CPP_NewConstString("a hash table"));
	parse_HashTable x = reinterpret_cast<parse_HashTable>(v->array[0]);
	parse_HashTable y = reinterpret_cast<parse_HashTable>(v->array[1]);
	if(x->Mutable)
		return expr_WrongArg_1(1, M2CPP_NewConstString("an immutable hash table"));
	if(y->Mutable)
		return expr_WrongArg_1(2, M2CPP_NewConstString("an immutable table"));
	return combine(v->array[2], v_17->array[3], v_17->array[4], x, y);
}
parse_Expr evaluate_unarymethod_1(parse_Expr right,parse_SymbolClosure methodkey){
	parse_Expr method_5 = hashtables_lookup(classes_Class(right), reinterpret_cast<parse_Expr>(methodkey), methodkey->symbol->hash);
	if (method_5 == parse_nullE)
		return expr_MissingMethod_1(methodkey);
	else
		return evaluate_applyEE(method_5, right);
}
parse_Expr evaluate_binarymethod_2(parse_Expr left,parse_Expr right,parse_SymbolClosure methodkey)
{
	parse_Expr method_6 = hashtables_lookupBinaryMethod(classes_Class(left), classes_Class(right), reinterpret_cast<parse_Expr>(methodkey), methodkey->symbol->hash);
	if (method_6 == parse_nullE)
		return expr_MissingMethodPair_2(methodkey, left, right);
	else
		return evaluate_applyEEE(method_6, left, right);
}
parse_Expr evaluate_binarymethod_3(parse_Expr left,parse_Expr right,parse_Expr methodkey,M2_string methodkeyname)
{
	parse_Expr method_7 = hashtables_lookupBinaryMethod(classes_Class(left), classes_Class(right), methodkey, basic_hash(methodkey));
	if (method_7 == parse_nullE)
		return expr_MissingMethodPair_3(methodkeyname, left, right);
	else
		return evaluate_applyEEE(method_7, left, right);
}
parse_Expr evaluate_notFun(parse_Expr a)
{
	if (a == parse_True)
		return parse_False;
	else if (a == parse_False)
		return parse_True;
	else
		return evaluate_unarymethod_1(a, binding_notS);
}
