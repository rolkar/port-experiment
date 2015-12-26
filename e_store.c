#include "e_store.h"

int e_set(int v)
{
  int old_v = e_value;
  e_value = v;

  return old_v;
}

int e_get()
{
  return e_value;
}

int e_inc(int v)
{
  int old_v = e_value;
  e_value += v;

  return old_v;
}

int e_crash(char *NullPtr)
{
  *NullPtr = 0;
  return 1;
}
