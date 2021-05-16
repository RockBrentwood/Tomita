## The compiler and compiler flags to use.
CFLAGS = -std=gnu99
CC = gcc
#CFLAGS =
#CC = qcl

## The file extensions. 
## Object File Suffix
O = .o
#O = .obj
 
## Executable File Suffix
X =
#X = .exe
 
### End of user definitions
 
all:	tom$X
tom$X:	tom$O
	$(CC) $(CFLAGS) -o tom tom$O
clean:
	rm -f *$O
clobber: clean
	rm -f tom$X
