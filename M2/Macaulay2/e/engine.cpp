// Copyright 1994-2002 by Michael E. Stillman

#include "style.hpp"
#include "mem.hpp"
#include "intarray.hpp"
#include "hash.hpp"
#include "engine.h"

#include "random.hpp"

#include "array.hpp"
#include "queue.hpp"
#include "Z.hpp"
#include "QQ.hpp"

unsigned long mutable_object::next_hash_sequence_number = 100000;

template class array< char * >;
template class queue< int >;

Matrix_int_pair global_Matrix_int_pair;

int heap_size[GEOHEAP_SIZE] = {4, 16, 64, 256, 1024, 4096, 
			       16384, 65536, 262144, 1048576, 4194304,
			       16777216, 67108864, 268435456,
			       1073741824};

void IM2_initialize()
{
  doubles                  = new doubling_stash;

  ZZ = Z::create(Monoid::get_trivial_monoid());
  globalQQ = QQ::create(Monoid::get_trivial_monoid());
  Random::i_random();
}

M2_string IM2_last_error_message()
{
  M2_string result = tostring(error_message());
  clear_error();
  return result;
}
