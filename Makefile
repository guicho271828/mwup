
.PHONY: submodules

all: mwup

submodules:
	git submodule update --init --recursive --remote
	+downward/build.py

mwup: $(shell find -name "*.lisp") submodules
	ros dynamic-space-size=16000 -- -e "(setf ql:*local-project-directories* '(#p\"$(CURDIR)/\"))(ql:register-local-projects)" dump executable ./mwup.ros

clean:
	rm mwup
