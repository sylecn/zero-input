VERSION := $(shell grep 'setq zero-version' zero.el | cut -d'"' -f2)

default: compile
pkg-el:
	sed "s/PKG_VERSION/$(VERSION)/g" zero-pkg.el.tpl > zero-pkg.el
compile:
	emacs -Q --batch -l zero-reload-all.el -f zero-rebuild -l zero-table.el -l zero-table-test.el -f ert-run-tests-batch
zip: pkg-el
	git archive -o zero-el-$(VERSION).zip --prefix=zero/ HEAD
pkg: pkg-el
# Note: install from tar is not working. install from dir does work.
	@echo "Creating tar for use with M-x package-install-file"
	git archive -o zero-$(VERSION).tar --prefix=zero-$(VERSION)/ HEAD
	@echo "Done"
install-git-hooks:
	rsync -air git-hooks/ .git/hooks/
version:
	@echo $(VERSION)
.PHONY: default pkg-el compile zip pkg install-git-hooks version
