DST = e.out e_li.out gen_e.beam

all : $(DST)

e.out: e.c
	gcc -Wall -o e.out e.c

e_li.out: e_li.c
	gcc -Wall -o e_li.out e_li.c

gen_e.beam: gen_e.erl
	erlc -o gen_e.beam gen_e.erl

test:
	echo "gen_e:start(), gen_e:stop(), init:stop()." | erl
	echo "gen_e:start([{file, \"./e_li.out\"}]), gen_e:stop(), init:stop()." | erl

clean:
	rm $(DST)
