/* e.c */

/* An experimental code for testing port behaviours, in particular
   detecting that a port goes down or are taken down.  Uses the Erlang
   module files e.erl or gen_e.erl. The standalone version. */

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>


#define SET 1
#define GET 2
#define INC 3
#define CRASH 4
#define EXIT 5

typedef unsigned char byte;

static int value = 0;

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

static int set(int v)
{
  int old_v = value;
  value = v;

  return old_v;
}

static int get()
{
  return value;
}

static int inc(int v)
{
  int old_v = value;
  value += v;

  return old_v;
}

static int crash(char *NullPtr)
{
  *NullPtr = 0;
  return 1;
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
      res = set(arg);
    } else if (fn == GET) {
      res = get();
    } else if (fn == INC) {
      int arg = buf[1];
      res = inc(arg);
    } else if (fn == CRASH) {
      res = crash(NULL);
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
