#The compiler to use
CC = gcc
#CC = qcl
 
# OBJECT FILE SUFFIX
O = o
#O = obj
 
# EXECUTABLE FILE SUFFIX
X =
#X = .exe
 
### End of user definitions
 
all:	tom$X
tom$X:	tom.$O
	$(CC) -o tom tom.$O
