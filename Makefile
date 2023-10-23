z8kzork: z8kzork.c host.c host.h
	gcc -DLINUX -g -c z8kzork.c -o z8kzork.o -Wno-pointer-to-int-cast -Wno-format
	gcc -DLINUX -g -c host.c -o host.o
	gcc -g -o z8kzork z8kzork.o host.o

mojozork: mojozork.c
	gcc -c mojozork.c -o mojozork.o
	gcc -o mojozork mojozork.o

clean:
	rm z8kzork *.o

up:
	rm -rf holding
	mkdir holding
	cp z8kzork.c holding/
	cp host.c holding/
	cp host.h holding/
	python ~/projects/pi/z8000/cpm8kdisks/addeof.py holding/*.h holding/*.c
	cpmrm -f cpm8k ~/projects/pi/z8000/super/sup.img  z8kzork.c host.h host.c catseye.z3 dejavu.z3 moonglow.z3 zork1.dat leather.dat || true
	cpmcp -f cpm8k ~/projects/pi/z8000/super/sup.img holding/* 0:
	cpmcp -f cpm8k ~/projects/pi/z8000/super/sup.img games/catseye.z3 games/dejavu.z3 games/moonglow.z3 zork1.dat games/leather.dat 0:

.PHONY: down
down:
	mkdir -p down
	rm -f down/zork.z8k
	cpmcp -f cpm8k ~/projects/pi/z8000/super/sup.img 0:ZORK.Z8K down/

compare:
	./z8kzork -d < zorktest.txt > good2.out || true
	tr A-Z a-z < good2.out > good2.lower
	tr A-Z a-z < bad2.out > bad2.lower
	./trimsame bad2.lower good2.lower good2h.lower


listimg:
	cpmls -f cpm8k -D ~/projects/pi/z8000/super/sup.img

# optimized version -- it's about twice as fast as the nonoptimized version.
olizork.cmd:
	z8k-pcos-gcc z8kzork.c olihost.c -o olizork.cmd -DOLIVETTI -Wl,-multi,-map,olizork.map -O2

# non-optimized version
olizorkn.cmd:
	z8k-pcos-gcc z8kzork.c olihost.c -o olizorkn.cmd -DOLIVETTI -Wl,-multi,-map,olizorkn.map
