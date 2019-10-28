VERSION := $(shell grep 'setq zero-input-version' zero-input-framework.el | cut -d'"' -f2)

default: dist
#===============
# multiple file
#===============
check:
	emacs -Q --batch -l zero-input-reload-all.el -f zero-input-rebuild -l zero-input-table.el -l zero-input-table-test.el -f ert-run-tests-batch
zip:
	git archive -o zero-input-el-$(VERSION).zip --prefix=zero-input/ HEAD
#==========================
# single file distribution
#==========================
dist: dist-check
build:
	if [ ! -x ~/.local/bin/pytest ]; then python3 -m pip install --user pytest; fi
	~/.local/bin/pytest build.py
	./build.py
	sed -i "s/PKG_VERSION/$(VERSION)/g" zero-input.el
dist-check: build
	@echo "testing byte-compile is clean..."
	emacs -Q --batch -l ~/.emacs.d/elpa/s-1.11.0/s.el --eval='(byte-compile-file "zero-input.el")'
	@echo "running unit tests..."
	emacs -Q --batch -l ~/.emacs.d/elpa/s-1.11.0/s.el -l zero-input.el -l zero-input-panel-test.el -l zero-input-pinyin-service-test.el -l zero-input-framework-test.el -l zero-input-pinyin-test.el -l zero-input-table.el -l zero-input-table-test.el -f ert-run-tests-batch
#====================
# other make targets
#====================
install-git-hooks:
	rsync -air git-hooks/ .git/hooks/
version:
	@echo $(VERSION)
.PHONY: default check zip dist build dist-check install-git-hooks version
