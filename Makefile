DST = e.out e.beam gen_e.beam

all : $(DST)

e.out: e.c
	gcc -Wall -o e.out e.c

e.beam: e.erl
	erlc e.erl

gen_e.beam: gen_e.erl
	erlc gen_e.erl

test:
	echo "gen_e:start(), gen_e:stop(), init:stop()." | erl

clean:
	rm $(DST)
