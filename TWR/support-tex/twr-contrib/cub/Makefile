
FC=ifort

FCFLAGS=-fpp -extend-source 132 -warn all  -O3 -fast # -stand f03 

OBJS=cub2cur.o cub2cubV2.o  cubAccess.o mergeCubs.o

PROG1=cub2cur
PROG2=cub2cubV2
PROG3=cub2smt
PROG4=mergeCubs

ALLPROGS= $(PROG1) $(PROG2) $(PROG3) $(PROG4)	

all	: $(ALLPROGS) $(OBJS)

.PHONY=clean

cub2cur.o   :     cubAccess.o

cub2cubV2.o :     cubAccess.o

mergeCubs.o :     cubAccess.o

$(PROG1)	:	$(OBJS)
	$(FC) -o $@ $(OBJS) 

$(PROG2)	:	$(OBJS)
	$(FC) -o $@ $(OBJS) 

$(PROG3)	:	cub2smt.o cubAccess.o
	$(FC) -o $@ $^ 

$(PROG4)	: cubAccess.o mergeCubs.o
	$(FC) -o $@ $^
clean :
	rm -f  $(ALLPROGS) $(OBJS) 
cub2smt.o	: cub2smt.f90 cubAccess.o
	$(FC) -c $(FCFLAGS) $< -o $@

%.o : %.for
	$(FC) -c $(FCFLAGS) $< -o $@

%.o : %.f90
	$(FC) -c $(FCFLAGS) $< -o $@

