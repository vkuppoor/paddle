CC = clang

# Define the source files and their corresponding object files
SRC_FILES := main.s print.s
OBJ_FILES := $(SRC_FILES:.s=.o)
objs = main.o print.o

# Define the default target
.DEFAULT_GOAL := all

# rule that links all of the runtime object files listed in objs into one file
runtime.o: $(objs)
	ld -r $(objs) -o runtime.o

# rule to build executable from %.run file
%.run: %.o runtime.o
	$(CC) runtime.o $< -o $@

# suffix rule for compiling .c files to .o files
.c.o:
	$(CC) -fPIC -c -g -o $@ $<

# suffix rule for compiling .s files to .o files
.s.o:
	$(CC) -c $< -o $@

# rule for compiling .ll files to .o files
%.s: %.ll
	llc $< -o $@

# Rule to build a LLVM IR (.ll) from a source (.rkt) file
%.ll: %.rkt
	dune exec bin/main.exe $<

# Clean up generated files
clean:
	rm -f *.ll *.o *.s *.run

.PHONY: all clean
