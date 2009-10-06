
FC=ifort

FCFLAGS=-fpp -extend-source 132 -warn all  -O3 -fast # -stand f03 

OBJS=cub2cur.o cub2cubV2.o  cubAccess.o

PROG1=cub2cur
PROG2=cub2cubV2
all	: $(PROG1) $(PROG2)	$(OBJS)

.PHONY=clean

cub2cur.o   :     cubAccess.o

cub2cubV2.o :     cubAccess.o

$(PROG1)	:	$(OBJS)
	$(FC) -o $@ $(OBJS) 

$(PROG2)	:	$(OBJS)
	$(FC) -o $@ $(OBJS) 

clean :
	rm -f  $(PROG1) $(PROG2) $(OBJS) 


%.o : %.for
	$(FC) -c $(FCFLAGS) $< -o $@


