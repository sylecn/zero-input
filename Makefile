VERSION := $(shell grep 'setq zero-version' zero-framework.el | cut -d'"' -f2)

compile:
	emacs -Q --batch -l zero-reload-all.el -f zero-rebuild -l zero-table.el -f ert-run-tests-batch
zip:
	git archive -o zero-el-$(VERSION).zip --prefix=zero/ HEAD
pkg:
	echo "Creating tar for package.el distribution..."
	git archive -o zero-$(VERSION).tar --prefix=zero/$(VERSION) HEAD
	echo "Done"
.PHONY: compile zip pkg
