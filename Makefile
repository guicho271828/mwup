
.PHONY: submodules

all: mwup

submodules:
	git submodule update --init --recursive --remote

mwup: $(shell find -name "*.lisp") submodules
	ros dump executable ./mwup.ros

