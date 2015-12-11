
.PHONY: submodules

all: mwup

submodules:
	git submodule update --init --recursive --remote

mwup: $(shell find -name "*.lisp") submodules
	ros -e "(setf ql:*local-project-directories* '(#p\"$(CURDIR)/\"))(ql:register-local-projects)" dump executable ./mwup.ros

