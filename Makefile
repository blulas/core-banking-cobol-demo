# Core Banking System - local build targets (GnuCOBOL)
.PHONY: build test data eod clean

build:            ## compile modules, batch, utilities and unit tests
	@scripts/build.sh

test:             ## build and run all unit-test suites
	@scripts/run-tests.sh

data: build       ## rebuild master files from data/seed and data/input
	@scripts/init-data.sh

eod: build        ## run the full end-of-day cycle against current masters
	@scripts/run-eod.sh

clean:
	rm -rf build data/master data/output
