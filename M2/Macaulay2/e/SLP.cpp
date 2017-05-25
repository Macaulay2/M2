// Copyright 2015 Anton Leykin and Mike Stillman

#include "SLP.hpp"

// SLProgram
SLProgram::SLProgram() { 
  // std::cout << "in SLProgram::SLProgram" << std::endl;
  inputCounter = 0; 
}
SLProgram::~SLProgram() {}
SLProgram::GATE_POSITION SLProgram::addMSum(const M2_arrayint a) 
{
  mNodes.push_back(MSum);
  mNumInputs.push_back(a->len);
  for(int i=0; i<a->len; i++)
    mInputPositions.push_back(a->array[i]-static_cast<GATE_POSITION>(mNodes.size())+1);
  return static_cast<GATE_POSITION>(mNodes.size())-1;
}
SLProgram::GATE_POSITION SLProgram::addMProduct(const M2_arrayint a) 
{
  mNodes.push_back(MProduct);
  mNumInputs.push_back(a->len);
  for(int i=0; i<a->len; i++)
    mInputPositions.push_back(a->array[i]-static_cast<GATE_POSITION>(mNodes.size())+1);
  return static_cast<GATE_POSITION>(mNodes.size())-1;
}
SLProgram::GATE_POSITION SLProgram::addDet(const M2_arrayint a) 
{
  mNodes.push_back(Det);
  mNumInputs.push_back(a->len);
  for(int i=0; i<a->len; i++)
    mInputPositions.push_back(a->array[i]-static_cast<GATE_POSITION>(mNodes.size())+1);
  return static_cast<GATE_POSITION>(mNodes.size())-1;
}
SLProgram::GATE_POSITION SLProgram::addDivide(const M2_arrayint a) 
{
  mNodes.push_back(Divide);
  if (a->len!=2) 
    ERROR("Divide expected two arguments");
  for(int i=0; i<2; i++)
    mInputPositions.push_back(a->array[i]-static_cast<GATE_POSITION>(mNodes.size())+1);
  return static_cast<GATE_POSITION>(mNodes.size())-1;
}
void SLProgram::setOutputPositions(const M2_arrayint a) 
{
  for(int i=0; i<a->len; i++) {
    int p = a->array[i];
    if (p<0 && -p>inputCounter)
      ERROR("input or constant position out of range");
    else if (p>=0 && p>=mNodes.size()) 
      ERROR("node position out of range");
    else mOutputPositions.push_back(p);
  }
}

void SLProgram::text_out(buffer& o) const { 
  o << "SLProgram (" << newline;
  o << "  inputs: " << inputCounter << newline; 
  o << "  #mNodes: " << mNodes.size() << newline;
  o << "  outputs: " << mOutputPositions.size() << newline;
  o << "  )" << newline;  
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
