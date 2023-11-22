CC = clang

# Define the object files of the C runtime system
objs = main.o print.o

# Define the default target
.DEFAULT_GOAL := all

# Rule that links all of the runtime object files listed in objs into one file
runtime.o: $(objs)
	ld -r $(objs) -o runtime.o

# Rule to build executable from %.run file
%.run: %.o runtime.o
	$(CC) runtime.o $< -o $@

# Suffix rule for compiling .c files to .o files
.c.o:
	$(CC) -fPIC -c -g -o $@ $<

# Suffix rule for compiling .s files to .o files
.s.o:
	$(CC) -c $< -o $@

# Rule for compiling .ll files to .o files
%.s: %.ll
	llc $< -o $@

# Rule to build a LLVM IR (.ll) from a source (.rkt) file
%.ll: %.rkt
	dune exec bin/compile_filein.exe $<

# Clean up generated files
clean:
	rm -f *.ll *.o *.s *.run

.PHONY: all clean
