// Copyright 2008 Anton Leykin and Mike Stillman

#include "NAG.hpp"
#include "matrix-con.hpp"

//                                        CONSTRUCTOR
complex::complex(double r=0.0f,double im=0.0f)
{
  real=r;
  imag=im;
}
 
//                                 COPY CONSTRUCTOR
complex::complex(const complex &c)
{
  this->real=c.real;
  this->imag=c.imag;
}
 
complex::complex(M2_CCC mpfrCC)
{
  real = mpfr_get_d(mpfrCC->re,GMP_RNDN);
  imag = mpfr_get_d(mpfrCC->im,GMP_RNDN);
}
 
void complex::operator =(complex c)
{
  real=c.real;
  imag=c.imag;
}
 
 
complex complex::operator +(complex c)
{
  complex tmp;
  tmp.real=this->real+c.real;
  tmp.imag=this->imag+c.imag;
  return tmp;
}
 
complex complex::operator -(complex c)
{
  complex tmp;
  tmp.real=this->real - c.real;
  tmp.imag=this->imag - c.imag;
  return tmp;
}
 
complex complex::operator *(complex c)
{
  complex tmp;
  tmp.real=(real*c.real)-(imag*c.imag);
  tmp.imag=(real*c.imag)+(imag*c.real);
  return tmp;
}
 
complex complex::operator /(complex c)
{
  double div=(c.real*c.real) + (c.imag*c.imag);
  complex tmp;
  tmp.real=(real*c.real)+(imag*c.imag);
  tmp.real/=div;
  tmp.imag=(imag*c.real)-(real*c.imag);
  tmp.imag/=div;
  return tmp;
}
 
complex complex::getconjugate()
{
  complex tmp;
  tmp.real=this->real;
  tmp.imag=this->imag * -1;
  return tmp;
}
 
complex complex::getreciprocal()
{
  complex t;
  t.real=real;
  t.imag=imag * -1;
  double div;
  div=(real*real)+(imag*imag);
  t.real/=div;
  t.imag/=div;
  return t;
}
 
double complex::getmodulus()
{
  double z;
  z=(real*real)+(imag*imag);
  z=sqrt(z);
  return z;
}
 
double complex::getreal()
{
  return real;
}
 
double complex::getimaginary()
{
  return imag;
}
 
bool complex::operator ==(complex c)
{
  return (real==c.real)&&(imag==c.imag) ? 1 : 0;
}

void complex::sprint(char* s)
{
  sprintf(s, "(%lf) + i*(%lf)", real, imag);
}
 
// Straiight Line Program class

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
    res->rows_out = program->array[2];
    res->cols_out = program->array[3];
    res->program = program;
    res->nodes = newarray(complex, program->len+res->num_consts+res->num_inputs);
    for (int i=0; i<res->num_consts; i++) { 
      res->nodes[i] = complex(BIGCC_VAL(m_consts->elem(0,i)));
    }
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
    nodes[cur_node] = complex(BIGCC_VAL(values->elem(0,i)));
  i=4+rows_out*cols_out;
  for(; i<program->len; cur_node++) {
    switch (program->array[i]) {
    case slpCOPY: 
      nodes[cur_node] = nodes[program->array[(++i)++]];
      break;
    case slpMULTIsum: 
      {	
	int n_summands = program->array[i+1];
	nodes[cur_node] = (n_summands>0) ?  // create a temp var?  
	  nodes[program->array[i+2]] : complex(); // zero if empty sum
	for(int j=1; j<n_summands; j++)
	  nodes[cur_node] = nodes[cur_node]+nodes[program->array[i+j+2]];
	i += n_summands+2; 
      }
      break;
    case slpPRODUCT:
      nodes[cur_node] = nodes[program->array[i+1]] * nodes[program->array[i+2]];
      i+=3;
      break;
    default:
      ERROR("unknown SLP operation");
    }
  }
  evaluated = true;
  const CCC* R = values->get_ring()->cast_to_CCC(); 
  //CCC* R = CCC::create(53); //values->get_ring();
  FreeModule* S = R->make_FreeModule(cols_out); 
  FreeModule* T = R->make_FreeModule(rows_out);
  MatrixConstructor mat(T,S);
  mpfr_t re, im;
  mpfr_init(re); mpfr_init(im);
  for(i=0; i<rows_out; i++)
  for(int j=0; j<cols_out; j++) {
    complex c = nodes[program->array[i*cols_out+j+4]]; 
    mpfr_set_d(re, c.getreal(), GMP_RNDN);
    mpfr_set_d(im, c.getimaginary(), GMP_RNDN);
    ring_elem e = R->from_BigReals(re,im);
    mat.set_entry(i,j,e);
  }
  mpfr_clear(re); mpfr_clear(im);
  return mat.to_matrix(); //hmm... how to make a matrix from scratch?
}

void StraightLineProgram::text_out(buffer& o) const
{
  o<<"CONSTANT (count = "<< num_consts;
  o<<") nodes:\n";
  int cur_node = 0;
  int i;
  for(i=0; i<num_consts; i++, cur_node++) {
    char s[100];
    nodes[cur_node].sprint(s);
    o << s << ", ";
  }
  o<<newline;   
  o<<"INPUT (count = " << num_inputs <<") nodes:\n";
  for(i=0; i<num_inputs; i++, cur_node++)
    o << cur_node << " ";
  o<<newline;   
  o<<"OUTPUT ("<< rows_out << "x" << cols_out << ") nodes:\n";
  for(i=4; i<4+cols_out*rows_out; i++)
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
    o<<"NODES\n";
    for(i=0; i<program->len; i++){
      char s[100];
      nodes[i].sprint(s);
      o<<"node["<< i <<"] = " << s << newline;
    }
  }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
