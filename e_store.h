#ifndef E_STORE_H
#define E_STORE_H 1

extern int e_set(int v);
extern int e_get(void);
extern int e_inc(int v);
extern int e_crash(char *NullPtr);

extern int e_value;

#define SET 1
#define GET 2
#define INC 3
#define CRASH 4
#define EXIT 5

#endif
