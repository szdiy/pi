
.PHONY: test

test:
	@for t in ./test-suite/tests/*.test; do guile -L ./ $$t; done


