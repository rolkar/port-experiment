/* e_li.c */

/* An experimental code for testing port behaviours, in particular
   detecting that a port goes down or are taken down.  Uses the Erlang
   module files e.erl or gen_e.erl. The linked in version. */

#include "erl_driver.h"

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "e_store.h"

int e_value = 100;

typedef struct {
  ErlDrvPort port;
} e_data;

static ErlDrvData e_li_start(ErlDrvPort port, char *buff)
{
  e_data *d = (e_data *)driver_alloc(sizeof(e_data));
  fprintf(stderr, "Starting linked in C driver\n");
  d->port = port;
  return (ErlDrvData)d;
}


static void e_li_stop(ErlDrvData handle)
{
  fprintf(stderr, "Stopping linked in C driver\n");
  driver_free((char*)handle);
}

static void e_li_output(ErlDrvData handle,
                         char *buf, 
                         ErlDrvSizeT bufflen)
{
  e_data *d = ((e_data *)handle);
  char res;
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

  driver_output(d->port, &res, 1);
}

static ErlDrvEntry e_li_entry = {
  NULL,			/* F_PTR init, called when driver is loaded */
  e_li_start,		/* L_PTR start, called when port is opened */
  e_li_stop,		/* F_PTR stop, called when port is closed */
  e_li_output,		/* F_PTR output, called when erlang has sent */
  NULL,			/* F_PTR ready_input, called when input descriptor ready */
  NULL,			/* F_PTR ready_output, called when output descriptor ready */
  "e_li",		/* char *driver_name, the argument to open_port */
  NULL,			/* F_PTR finish, called when unloaded */
  NULL,			/* void *handle, Reserved by VM */
  NULL,			/* F_PTR control, port_command callback */
  NULL,			/* F_PTR timeout, reserved */
  NULL,			/* F_PTR outputv, reserved */
  NULL,			/* F_PTR ready_async, only for async drivers */
  NULL,			/* F_PTR flush, called when port is about 
                           to be closed, but there is data in driver 
                           queue */
  NULL,                 /* F_PTR call, much like control, sync call
                           to driver */
  NULL,                 /* F_PTR event, called when an event selected 
                           by driver_event() occurs. */
  ERL_DRV_EXTENDED_MARKER,        /* int extended marker, Should always be 
                                     set to indicate driver versioning */
  ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be 
                                     set to this value */
  ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be 
                                     set to this value */
  0,                          /* int driver_flags, see documentation */
  NULL,                       /* void *handle2, reserved for VM use */
  NULL,                       /* F_PTR process_exit, called when a 
                                 monitored process dies */
  NULL                        /* F_PTR stop_select, called to close an 
                                 event object */
};

DRIVER_INIT(e_li)
{
  fprintf(stderr, "Init linked in C driver\n");
  return &e_li_entry;
}
