DST = e.out e_li.so gen_e.beam

.PHONY: test clean

all : $(DST)

e.out: e.c
	gcc -Wall -o e.out e.c

e_li.o: e_li.c
	gcc -Wall -arch `uname -m` -fPIC -o e_li.o -c -fpic e_li.c -I /usr/local/lib/erlang/erts*/include

e_li.so: e_li.o
	gcc -bundle -flat_namespace -undefined suppress -o e_li.so e_li.o \
	|| gcc -shared -o e_li.so e_li.o

gen_e.beam: gen_e.erl
	erlc -o gen_e.beam gen_e.erl

test: all
	@echo External driver
	@echo
	@echo "gen_e:start(), gen_e:get(), gen_e:stop(), init:stop()." | erl
	@echo
	@echo Linked in driver
	@echo
	@echo "gen_e:start([{type, linkedin}]), gen_e:get(), gen_e:stop(), init:stop()." | erl

clean:
	@-rm $(DST)
	@-rm *.o
