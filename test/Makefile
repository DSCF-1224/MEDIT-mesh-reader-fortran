# compiler selection
FC = gfortran

# compiler option: Common Options
FFLAGS_COMMON = -ffree-line-length-none -fimplicit-none -pedantic -std=f2008 -Wall -Werror -Wextra

# compiler option: Release mode
FFLAGS_RELEASE = ${FFLAGS_COMMON} -O3

# compiler option: Debug mode
FFLAGS_DEBUG = ${FFLAGS_COMMON} -O0 -s -fbacktrace -fbounds-check -g

# target of compilation
TARGET = ./test.exe

# object codes
OBJS = \
	./inria_medit_reader_interface.o \
	./main.o


# suffix rule
.SUFFIXES:
	.o .f08

all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) -o $@ $(OBJS)

%.o: src/%.f08
	$(FC) $(FFLAGS) -c $<

%.o: ../src/%.f08
	$(FC) $(FFLAGS) -c $<

clean:
	rm ./*.exe ./*.mod ./*.o ./*.smod ./*.txt

debug_mode:
	make clean; \
	make FFLAGS="$(FFLAGS_DEBUG)"; \
	make run_exe

release_mode:
	make clean; \
	make FFLAGS="$(FFLAGS_RELEASE)"; \
	make run_exe

run_exe:
	time $(TARGET) >./output_unit.txt 2>./error_unit.txt
