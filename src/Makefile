EXE = swf.exe

FC = gfortran

LDFLAGS=
CPPFLAGS= -cpp
FFLAGS= -c -O3 $(CPPFLAGS) $(LDFLAGS)

OBJS = main.o system_state.o kinds.o gridinfo.o \
model_core.o io_module.o physics.o

install: $(EXE)

main.o: main.f90 model_core.o
io_module.o: io_module.f90 kinds.o
model_core.o: model_core.f90 system_state.o kinds.o \
              gridinfo.o io_module.o physics.o
system_state.o: system_state.f90 kinds.o gridinfo.o
kinds.o: kinds.f90 
physics.o: physics.f90 kinds.o system_state.o
gridinfo.o: gridinfo.f90 kinds.o

%.o: %.f90
	$(FC) $(FFLAGS)  $<

$(EXE): $(OBJS)
	$(FC) $(OBJS) -o $(EXE) $(CPPFLAGS) $(LDFLAGS)

clean:
	rm -rf *.exe *.o *.mod