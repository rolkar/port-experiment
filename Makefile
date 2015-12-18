DST = e.out e.beam gen_e.beam

all : $(DST)

e.out:
	gcc -Wall -o e.out e.c

e.beam:
	erlc e.erl

gen_e.beam:
	erlc gen_e.erl

test:
	echo "gen_e:start(), gen_e:stop(), init:stop()." | erl

clean:
	rm $(DST)
