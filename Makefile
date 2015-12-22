DST = e.out gen_e.beam

all : $(DST)

e.out: e.c
	gcc -Wall -o e.out e.c

gen_e.beam: gen_e.erl
	erlc gen_e.erl

test:
	echo "gen_e:start(), gen_e:stop(), init:stop()." | erl

clean:
	rm $(DST)
