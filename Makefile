FC = ftn

OFLAGS =-O3 -ffixed-line-length-132 -g
#LIB = -lmpich

OBJECTS = memory.o bound.o relax.o swap.o io.o source.o \
          custompi.o station.o structure.o operator.o \
          viscoop.o pml.o cerjan.o pmcl3d.o set_names.o \
          sgsndyna.o sgsnswap.o

pmcl3d: $(OBJECTS)
	$(FC) $(OFLAGS) $(OBJECTS) -o pmcl3d $(LIB) 

pmcl3d.o: pmcl3d.f
	$(FC) $(OFLAGS) -c -o pmcl3d.o	pmcl3d.f

sgsndyna.o: sgsndyna.f
	$(FC) $(OFLAGS) -c -o sgsndyna.o    sgsndyna.f

sgsnswap.o: sgsnswap.f
	$(FC) $(OFLAGS) -c -o sgsnswap.o    sgsnswap.f

memory.o: memory.f
	$(FC) $(OFLAGS) -c -o memory.o	memory.f

relax.o: relax.f
	$(FC) $(OFLAGS) -c -o relax.o 	relax.f

bound.o: bound.f
	$(FC) $(OFLAGS) -c -o bound.o 	bound.f

swap.o: swap.f
	$(FC) $(OFLAGS) -c -o swap.o        swap.f

io.o: io.f
	$(FC) $(OFLAGS) -c -o io.o		io.f

structure.o: structure.f
	$(FC) $(OFLAGS) -c -o structure.o	structure.f

operator.o: operator.f
	$(FC) $(OFLAGS) -c -o operator.o    operator.f

viscoop.o: viscoop.f
	$(FC) $(OFLAGS) -c -o viscoop.o 	viscoop.f

pml.o: pml.f
	$(FC) $(OFLAGS) -c -o pml.o         pml.f

cerjan.o: cerjan.f
	$(FC) $(OFLAGS) -c -o cerjan.o	cerjan.f

source.o: source.f
	$(FC) $(OFLAGS) -c -o source.o      source.f

station.o: station.f
	$(FC) $(OFLAGS) -c -o station.o     station.f

custompi.o: custompi.f	
	$(FC) $(OFLAGS) -c -o custompi.o	custompi.f

set_names.o: set_names.f
	$(FC) $(OFLAGS) -c -o set_names.o   set_names.f

clean:
	rm -f pmcl3d $(BIN)/pmcl3d *.o *.oo *.mod

cleanmore:
	rm fort.* core.* CHK* SS* V* SRCT*


