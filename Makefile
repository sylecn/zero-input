VERSION := $(shell grep 'setq zero-input-version' zero-input-framework.el | cut -d'"' -f2)
EMACS := emacs

default: dist
#===============
# multiple file
#===============
check:
	$(EMACS) -Q --batch -l zero-input-reload-all.el -f zero-input-rebuild -l zero-input-table.el -l zero-input-table-test.el -f ert-run-tests-batch
zip:
	git archive -o zero-input-el-$(VERSION).zip --prefix=zero-input/ HEAD
#==========================
# single file distribution
#==========================
dist: dist-check
build:
	if ! python3 -m pytest --version; then python3 -m pip install --user pytest; fi
	python3 -m pytest build.py
	./build.py
	sed -i "s/PKG_VERSION/$(VERSION)/g" zero-input.el
dist-check: build
	@echo "testing byte-compile is clean..."
	$(EMACS) -Q --batch -l ~/.emacs.d/elpa/s-1.11.0/s.el --eval='(byte-compile-file "zero-input.el")'
	@echo "running unit tests..."
	$(EMACS) -Q --batch -l ~/.emacs.d/elpa/s-1.11.0/s.el -l zero-input.el -l zero-input-panel-test.el -l zero-input-pinyin-service-test.el -l zero-input-framework-test.el -l zero-input-pinyin-test.el -l zero-input-table.el -l zero-input-table-test.el -f ert-run-tests-batch-and-exit
#====================
# other make targets
#====================
install-git-hooks:
	rsync -air git-hooks/ .git/hooks/
version:
	@echo $(VERSION)
.PHONY: default check zip dist build dist-check install-git-hooks version
