#include "../platform/platform.h"
#include "hashtables.hpp"
#include <M2-exports.h>
#include <interp-exports.h>
#include <hashtables-exports.h>
#include <cstring>
#include <sstream>
#include <iostream>

/***
	Enlarge the hash table.
	We do this by doing a swap so that it is atomic on threads.
	Note this relies on X86 memory ordering!
	@param o Hash table to enlarge, not null.
***/
static void enlarge(parse_HashTable o)
{
	assert(o!=NULL);
	struct SCC_M2_0_int_len_parse_KeyValuePair_array1 * oldTable = o->table;
	int newlen = (2 * oldTable->len);
	int mask = (newlen - 1);
	struct SCC_M2_0_int_len_parse_KeyValuePair_array1* newTable = (struct SCC_M2_0_int_len_parse_KeyValuePair_array1 *) GC_MALLOC(sizeof(struct SCC_M2_0_int_len_parse_KeyValuePair_array1) + (newlen - 1)*sizeof(parse_KeyValuePair));
	newTable->len = newlen;
	//initialize new table buckets to bucket end.
	for(size_t i = 0; i < newlen; ++i)
	{
		newTable->array[i] = expr_bucketEnd;
	}
	for(size_t i = 0; i < oldTable->len; ++i)
	{
		parse_KeyValuePair p = oldTable->array[i];
		while(p!=p->next)
		{
			int hmod = (int)(p->hash& mask);
			parse_KeyValuePair kvp = (parse_KeyValuePair) GC_MALLOC(sizeof(struct parse_KeyValuePair_struct));
			kvp->key = p->key;
			kvp->hash = p->hash;
			kvp->value = p->value;
			kvp->next = newTable->array[hmod];
			newTable->array[hmod] = kvp;
			p = p->next;
		}
	}
	AO_compiler_barrier();
	o->table = newTable;
}
/***
	Shrink the table.
	We do this by doing a swap so that is atomic on threads.
	Note this relies on X86 memory ordering!
	@param o Hash table to shrink, not null.
***/
static void shrink(parse_HashTable o)
{
	assert(o!=NULL);
	struct SCC_M2_0_int_len_parse_KeyValuePair_array1 * oldTable = o->table;
	int newlen = (oldTable->len / 2);
	int mask = (newlen - 1);
	struct SCC_M2_0_int_len_parse_KeyValuePair_array1 * newtable = (struct SCC_M2_0_int_len_parse_KeyValuePair_array1 *) GC_MALLOC(sizeof(struct SCC_M2_0_int_len_parse_KeyValuePair_array1) + (newlen - 1)*sizeof(parse_KeyValuePair));
	newtable->len = newlen;
	for(size_t i = 0; i < newtable->len; ++i)
	{
		newtable->array[i] = expr_bucketEnd;
	}
	for(size_t i = 0; i < oldTable->len; ++i)
	{
		parse_KeyValuePair p = oldTable->array[i];
		while(p!=p->next)
		{
			int hmod = (int)(p->hash& mask);
			parse_KeyValuePair kvp = (parse_KeyValuePair) GC_MALLOC(sizeof(struct parse_KeyValuePair_struct));
			kvp->key = p->key;
			kvp->hash = p->hash;
			kvp->value = p->value;
			kvp->next = newtable->array[hmod];
			newtable->array[hmod] = kvp;
			p = p->next;
		}			
	}
	AO_compiler_barrier();
	o->table = newtable;
}
/***
	Attempt to store the object in the hash table with a given key.
	No clobber mode raises errors on key collisions.
	Must clobber mode raises errors if there is not a collision.
	The handler provides a mechanism in no clobber mode to execute code upon a collision.
	@param x Hash table to store in, not null.
	@param key Lookup key, not null.
	@param h Hash of key.
	@param value Value to store, not null.
	@param handler collision handler, may be null.  If present, noClobber must be True.
	@param noClobber Set to True for no clobber mode.
	@param mustClobber Set to True for must clobber mode.
	@return Stored value or error, not null.
***/
static parse_Expr storeInHashTable(parse_HashTable x,parse_Expr key,int h,parse_Expr value, parse_Expr handler, bool noClobber, bool mustClobber)
{
	assert(x!=NULL);
	assert(key!=NULL);
	assert(value!=NULL);
	assert(!(noClobber && mustClobber));
	assert(!(handler && !noClobber));
	if (!x->Mutable)
	{
		return expr_buildErrorPacket(M2CPP_NewConstString("attempted to modify an immutable hash table"));
	}
	if (!x->beingInitialized)
	{
		acquireSpinLock(&(x->mutex));
	}
	int hmod = static_cast<int>(h & (x->table->len - 1));
	parse_KeyValuePair p = x->table->array[hmod];
	while(p!=p->next)
	{
		if(p->key==key || equality_equal(p->key,key) == parse_True)
		{
			if(noClobber)
			{
				if(!x->beingInitialized)
					releaseSpinLock(&x->mutex);
				if(handler)
					return hashtables_applyEEEpointer(handler, p->value, value);
				else
					return expr_buildErrorPacket(M2CPP_NewConstString("collision of keys in hash table."));
			}
			else
			{
				p->value = value;
				if(!x->beingInitialized)
					releaseSpinLock(&x->mutex);
				return value;
			}
		}
		p = p->next;
	}
	if(mustClobber)
	{
		if(!x->beingInitialized)
			releaseSpinLock(&x->mutex);
		M2_string tmp__53 = NULL;
		switch (key->type_) {
		case SymbolClosure_typecode:
			{
				parse_SymbolClosure s_1 = reinterpret_cast<parse_SymbolClosure>(key);
				tmp__53 = strings_plus_(M2CPP_NewConstString(": "), s_1->symbol->word->name);
				break;
			}
		case stringCell_typecode:
			{
				M2_stringCell s_2 = reinterpret_cast<M2_stringCell>(key);
				tmp__53 = strings_plus_(M2CPP_NewConstString(": "), s_2->v);
				break;
			}
		default:
			checkTypeValidity(key->type_,__FILE__,__LINE__);
			tmp__53 = M2CPP_NewConstString("");
			break;
		};
		return expr_buildErrorPacket(strings_plus_(M2CPP_NewConstString("encountered an unknown key or option"), tmp__53));
	}
	if (4*x->numEntries == 3*x->table->len)
	{
		enlarge(x);
		hmod = static_cast<int>(h & (x->table->len - 1));
	}
	parse_KeyValuePair tmp__38 = (parse_KeyValuePair) GC_MALLOC(sizeof(struct parse_KeyValuePair_struct));
	tmp__38->key = key;
	tmp__38->hash = h;
	tmp__38->value = value;
	tmp__38->next = x->table->array[hmod];
	x->table->array[hmod] = tmp__38;
	AO_compiler_barrier();
	x->numEntries++;
	if (!x->beingInitialized)
	{
		releaseSpinLock(&(x->mutex));
	}
	return value;
}

extern "C"
{
	int hashtables_hash(parse_HashTable x)
	{
		assert(x!=NULL);
		int h = ((x->parent->hash + (x->Class->hash * 231)) + 32455);
		struct SCC_M2_0_int_len_parse_KeyValuePair_array1 *tmp__1 = x->table;
		for(size_t i = 0; i < tmp__1->len; ++i)
		{
			parse_KeyValuePair p = tmp__1->array[i];
			while (p!=p->next)
			{
				int j = (48892373 + p->hash);
				h = (h + ((j * j) * basic_hash(p->value)));
				p = p->next;
			}
		}
		return h;
	}
	parse_HashTable hashtables_sethash(parse_HashTable o,char Mutable)
	{
		assert(o!=NULL);
		if (Mutable) 
		{
			o->Mutable = 1;
			o->hash = expr_nextHash();
		}
		else
		{
			o->Mutable = 0;
			o->hash = hashtables_hash(o);
		}
		o->beingInitialized = 0;
		return o;
	}
	parse_Expr hashtables_mutablefun(parse_Expr e)
	{
		assert(e!=NULL);
		switch (e->type_) 
		{
		case HashTable_typecode:
			{
				parse_HashTable o_1 = ((parse_HashTable)e);
				return parse_toExpr(o_1->Mutable);
			}
		case List_typecode:
			{
				parse_List x_1 = ((parse_List)e);
				return parse_toExpr(x_1->Mutable);
			}
		case SymbolClosure_typecode:
			{
				parse_SymbolClosure s = ((parse_SymbolClosure)e);
				return parse_toExpr(!(s->symbol->Protected));
			}
		case DictionaryClosure_typecode:
			{
				parse_DictionaryClosure d = ((parse_DictionaryClosure)e);
				return parse_toExpr (!d->dictionary->Protected);
			}
		case Database_typecode:
			{
				parse_Database x_2 = ((parse_Database)e);
				return parse_toExpr(x_2->Mutable);
			}
		default:
			checkTypeValidity(e->type_,__FILE__,__LINE__);
			return parse_toExpr(0);
		};
	}
	parse_Expr hashtables_remove(parse_HashTable x,parse_Expr key)
	{
		assert(x!=NULL);
		assert(key!=NULL);
		if (!x->Mutable)
		{
			return expr_buildErrorPacket(M2CPP_NewConstString("attempted to modify an immutable hash table"));
		}
		int h_1 = basic_hash(key);
		if (!x->beingInitialized)
		{
			acquireSpinLock(&(x->mutex));
		}
		int hmod = static_cast<int>(h_1 & (x->table->len - 1));
		parse_KeyValuePair p = x->table->array[hmod];
		parse_KeyValuePair prev = p;
		while(p!=p->next)
		{
			if(p->key == key || equality_equal(p->key, key) == parse_True)
			{
				x->numEntries = (x->numEntries - 1);
				//note this replacement is atomic since this is a singlely linked list.
				if(prev == p)
					//TODO: can this actually happen?
					x->table->array[hmod]=p->next;
				else
					prev->next = p->next;
				if(8*x->numEntries == 3*x->table->len && x->table->len > 4)
					shrink(x);
				if(!x->beingInitialized)
					releaseSpinLock(&x->mutex);
				return reinterpret_cast<parse_Expr>(x);
			}
			prev = p;
			p = p->next;
		}
		return reinterpret_cast<parse_Expr>(x);
	}
	parse_Expr hashtables_storeInHashTable(parse_HashTable x,parse_Expr key,int h,parse_Expr value)
	{
		return storeInHashTable(x,key,h,value,NULL,false,false);
	}
	parse_Expr hashtables_storeInHashTable_1(parse_HashTable x,parse_Expr key,parse_Expr value)
	{
		return hashtables_storeInHashTable(x, key, basic_hash(key), value);
	}
	parse_Expr hashtables_storeInHashTableNoClobber(parse_HashTable x,parse_Expr key,int h,parse_Expr value)
	{
		return storeInHashTable(x,key,h,value,NULL,true,false);
	}
	parse_Expr hashtables_storeInHashTableNoClobber_1(parse_HashTable x,parse_Expr key,parse_Expr value)
	{
		return hashtables_storeInHashTableNoClobber(x, key, basic_hash(key), value);
	}
	parse_Expr hashtables_storeInHashTableMustClobber(parse_HashTable x,parse_Expr key,int h,parse_Expr value)
	{
		return storeInHashTable(x,key,h,value,NULL,false,true);
	}
	parse_Expr hashtables_storeInHashTableMustClobber_1(parse_HashTable x,parse_Expr key,parse_Expr value)
	{
		return hashtables_storeInHashTableMustClobber(x, key, basic_hash(key), value);
	}
	parse_Expr hashtables_storeInHashTableWithCollisionHandler(parse_HashTable x,parse_Expr key,parse_Expr value,parse_Expr handler)
	{
		storeInHashTable(x,key,basic_hash(key),value,handler,true,false);
	}
}
