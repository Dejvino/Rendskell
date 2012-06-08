#
# RENDSKELL makefile
#
#

# build options
PROGRAM_NAME=rendskell
CC=ghc
BUILDOP=-fvia-C -threaded --make
RUNOPT=



rendskell: Main

all:	clean rendskell

clean:
	rm -f *.hi *.hc *.o $(PROGRAM_NAME)

Main:	Main.hs
	$(CC) $(BUILDOP) Main.hs -o $(PROGRAM_NAME)
