.DEFAULT_GOAL = help

.PHONY: test bench

help:
	@echo ""
	@echo "AVAILABLE TASKS"
	@echo ""
	@echo "  test ................... Runs the tests for the project."
	@echo "  benc ................... Runs the bench"
	@echo ""

bench:
	pulp build -I bench --main Bench.Main -O --to output/benchmarks.js
	node output/benchmarks.js

test:
	pulp test
