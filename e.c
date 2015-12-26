/* e.c */

/* An experimental code for testing port behaviours, in particular
   detecting that a port goes down or are taken down.  Uses the Erlang
   module files e.erl or gen_e.erl. The standalone version. */

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "e_store.h"

typedef unsigned char byte;

int e_value = 0;

static int read_exact(byte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);

  return(len);
}

static int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}

static int read_cmd(byte *buf)
{
  int len;

  if (read_exact(buf, 1) != 1)
    return(-1);
  len = buf[0];
  return read_exact(buf, len);
}

static int write_cmd(byte *buf, int len)
{
  byte li;

  li = len;
  write_exact(&li, 1);

  return write_exact(buf, len);
}


int main()
{
  byte buf[100];

  fprintf(stderr, "Starting external C driver\n");

  while (read_cmd(buf) > 0) {
    int res;

    int fn = buf[0];
    
    if (fn == SET) {
      int arg = buf[1];
      res = e_set(arg);
    } else if (fn == GET) {
      res = e_get();
    } else if (fn == INC) {
      int arg = buf[1];
      res = e_inc(arg);
    } else if (fn == CRASH) {
      res = e_crash(NULL);
    } else if (fn == EXIT) {
      int arg = buf[1];
      res = 0; /* dummy */
      exit(arg);
    } else {
      res = 42;
    }

    buf[0] = res;
    write_cmd(buf, 1);

  }

  fprintf(stderr, "Stopping external C driver\n");

  return 0;
}
