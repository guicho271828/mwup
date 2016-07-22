
.PHONY: submodules

all: mwup

submodules:
	git submodule update --init --recursive --remote
	+downward/build.py translate preprocess downward

mwup: $(shell find -name "*.lisp") submodules
	ros dynamic-space-size=4000 -- -e "(setf ql:*local-project-directories* '(#p\"$(CURDIR)/\"))(ql:register-local-projects)" dump executable ./mwup.ros

clean:
	rm mwup

test:
	./testscr.ros
