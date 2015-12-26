ifeq ($(OS),Windows_NT)
  OS_detected := Windows
else
  OS_detected := $(shell uname -s)
endif

ifeq ($(OS_detected),Windows)
  CompSw = TODO
  LinkSW = TODO
else ifeq ($(OS_detected),Linux)
  CompSW = -fno-common -fPIC
  LinkSW = -shared
else ifeq ($(OS_detected),Darwin)
  CompSW = -fno-common -arch $(shell uname -m) -fPIC
  LinkSW = -bundle -flat_namespace -undefined suppress
else
  CompSW = TODO
  LinkSW = TODO
endif


DST = e.out e_li.so gen_e.beam

.PHONY: test clean

all : $(DST)

e.out: e.c e_store.o
	gcc -Wall -o e.out e.c e_store.o

e_store.o: e_store.c
	gcc -Wall -c -o e_store.o e_store.c

e_store_li.o: e_store.c
	gcc -Wall $(CompSW) -c -o e_store_li.o e_store.c

e_li.o: e_li.c
	gcc -Wall $(CompSW) -o e_li.o -c e_li.c \
        -I /usr/local/lib/erlang/erts*/include

e_li.so: e_li.o e_store_li.o
	gcc $(LinkSW) -o e_li.so e_li.o e_store_li.o

gen_e.beam: gen_e.erl
	erlc -o gen_e.beam gen_e.erl

test: all
	@echo External driver
	@echo
	@echo "{ok,_}=gen_e:start([{params, [exit_status]}]), gen_e:get(), gen_e:crash(), init:stop()." | erl
	@echo
	@echo Linked in driver
	@echo
	@echo "{ok,_}=gen_e:start([{type, linkedin}]), gen_e:get(), gen_e:stop(), init:stop()." | erl

clean:
	@-rm $(DST)
	@-rm *.o
