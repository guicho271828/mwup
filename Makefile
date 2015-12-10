

all: mwup downward

downward: downward-fixed.tar.gz
	tar xf $<

mwup: $(shell find -name "*.lisp")
	ros dump executable ./mwup.ros

