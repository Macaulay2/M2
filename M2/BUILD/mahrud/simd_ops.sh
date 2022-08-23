# Script for checking which SIMD operations are used in an executable file

list="X86 X64 SSE2 FMA AVX"

objdump -M intel -d $1 > source
for x in $list; do echo -e "`cat source | opcode.sh -s $x -c`\t$x"; done | sort -n
# 
