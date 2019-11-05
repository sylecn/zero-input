VERSION := $(shell grep 'setq zero-input-version' zero-input.el | cut -d'"' -f2)
TAR_BUILD_DIR := build

default: check
#===============
# multiple file
#===============
check:
	emacs -Q --batch -l zero-input-reload-all.el -f zero-input-rebuild -l zero-input-table.el -l zero-input-table-test.el -f ert-run-tests-batch
	sed "s/PKG_VERSION/$(VERSION)/g" zero-input-pkg.el.tpl > zero-input-pkg.tmp
	if ! diff -q zero-input-pkg.tmp zero-input-pkg.el; then mv zero-input-pkg.tmp zero-input-pkg.el; fi
tar: check
	git archive -o zero-input-$(VERSION).tar --prefix=zero-input-$(VERSION)/ HEAD
	rm -rf $(TAR_BUILD_DIR)/ && mkdir -p $(TAR_BUILD_DIR)/
	tar xf zero-input-$(VERSION).tar -C $(TAR_BUILD_DIR)/
	sed "s/PKG_VERSION/$(VERSION)/g" zero-input-pkg.el.tpl > $(TAR_BUILD_DIR)/zero-input-$(VERSION)/zero-input-pkg.el
	rm zero-input-$(VERSION).tar
	tar cf zero-input-$(VERSION).tar -C $(TAR_BUILD_DIR)/ zero-input-$(VERSION)
	tar -tf zero-input-$(VERSION).tar
zip:
	git archive -o zero-input-el-$(VERSION).zip --prefix=zero-input/ HEAD
#====================
# other make targets
#====================
install-git-hooks:
	rsync -air git-hooks/ .git/hooks/
version:
	@echo $(VERSION)
.PHONY: default check tar zip dist build dist-check install-git-hooks version
