#include <stdio.h>

int main (int argc, char **argv, char **envp) {
  while (*envp) puts(*envp++);
  return 0;
}

    
