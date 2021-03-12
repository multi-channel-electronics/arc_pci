TARGETS = lod2s scan

all: $(TARGETS)

lod2s: lod2s.c srec.o
	gcc lod2s.c srec.o -o lod2s

scan: scan.c srec.o
	gcc scan.c srec.o -o scan

clean: tidy
	rm $(TARGETS)

tidy:
	rm *~ *.o
