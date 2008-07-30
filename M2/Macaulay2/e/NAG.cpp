// Copyright 2008 Anton Leykin and Mike Stillman

#include "NAG.hpp"

StraightLineProgram::StraightLineProgram()
{
}

StraightLineProgram_OrNull *StraightLineProgram::make(Matrix *m_consts, M2_arrayint program)
{
  // todo: move some of these lines to rawSLP
  StraightLineProgram* res;
  if (program->len < 3) {
    ERROR("invalid SLP");
    res = NULL;
  } else {
    res = new StraightLineProgram;
    res->num_consts = program->array[0];
    if (m_consts->n_cols() != res->num_consts) 
      ERROR("different number of constants expected");
    res->num_inputs = program->array[1];
    res->num_outputs = program->array[2];
    res->program = program;
    res->nodes = newarray(M2_CCC, program->len);
    for (int i=0; i<res->num_consts; i++) 
      res->nodes[i] = BIGCC_VAL(m_consts->elem(0,i));
    res->output = newarray(M2_CCC, res->num_outputs);
    res->evaluated = false;
  }
  return res;
}

Matrix *StraightLineProgram::evaluate(const Matrix *values)
{
  int cur_node = num_consts;
  int i;
  if (values->n_cols() != num_inputs) 
    ERROR("different number of constants expected");
  for(i=0; i<num_inputs; i++, cur_node++)
    nodes[i] = BIGCC_VAL(values->elem(0,i));
  //precision = mpfr_get_prec(nodes[0]->re); // set precision gracefully?
  i=3+num_outputs;
  for(; i<program->len; cur_node++) {
    switch (program->array[i]) {
    case slpCOPY: 
      nodes[cur_node] = nodes[program->array[(++i)++]];
      break;
    case slpMULTIsum: 
      {	
	int n_summands = program->array[i+1];
	nodes[cur_node] = nodes[program->array[i+3]]; // create a temp var? 
	//for(int j=1; j<n_summands; j++)
	//  mpfc_add(nodes[cur_node], nodes[cur_node], nodes[program->array[i+j+2]]);
	i += n_summands+2; 
      }
      break;
    case slpPRODUCT:
      nodes[cur_node] = (M2_CCC) getmem(sizeof(M2_CCC_struct));
      mpfc_init_set(nodes[cur_node], nodes[0]);
      //mpfc_mul(nodes[cur_node], nodes[program->array[i+1]],nodes[program->array[i+2]]);
      i+=3;
      break;
    default:
      ERROR("unknown SLP operation");
    }
  }
  for(i=0; i<num_outputs; i++)
    output[i] = nodes[program->array[i+3]];
  //evaluated = true;
  return values->transpose(); //hmm... how to make a matrix?
}

void StraightLineProgram::text_out(buffer& o) const
{
  o<<"CONSTANT (count = "<< num_consts;
  o<<") nodes:\n";
  int cur_node = 0;
  int i;
  for(i=0; i<num_consts; i++, cur_node++)
    o << gmp_tostringCC(nodes[cur_node]) << " ";
  o<<newline;   
  o<<"INPUT (count = " << num_inputs <<") nodes:\n";
  for(i=0; i<num_inputs; i++, cur_node++)
    o << cur_node << " ";
  o<<newline;   
  o<<"OUTPUT (count = " << num_outputs<<") nodes:\n";
  for(i=3; i<3+num_outputs; i++)
    o << program->array[i] << " ";
  o<<newline;   
  for(; i<program->len; cur_node++) {
    o<<cur_node<<" => ";
    switch (program->array[i]) {
    case slpCOPY: 
      o<<"copy "<< program->array[(++i)++];
      break;
    case slpMULTIsum:
      {	o<<"sum";
	int n_summands = program->array[i+1];
	for(int j=0; j<n_summands; j++)
	  o<<" "<<program->array[i+j+2];
	i += n_summands+2; }
      break;
    case slpPRODUCT:
      o<<"product "<<program->array[i+1]<<" "<<program->array[i+2];
      i+=3;
      break;
    default:
      o<<"BLA i="<<i++;
    }
    o<<newline;
  }
  if (evaluated) {
    o<<"OUTPUT\n";
    for(i=0; i<num_outputs; i++){
      o<<gmp_tostringCC(output[i])<<newline;
    }
  }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
