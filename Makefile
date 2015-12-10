

all: mwup

mwup: $(shell find -name "*.lisp")
	ros dump executable ./mwup.ros

