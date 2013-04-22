main()
{
  unsigned int bit=0x40000000, sum=0;
  char *x;
  
  while (bit > 4096) 
  {
    x = malloc(bit);
    if (x)
    sum += bit;
    bit >>= 1;
  }
  printf("%08x bytes (%.1fMb)\n", sum, sum/1024.0/1024.0);
  return 0;
}

