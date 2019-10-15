VERSION := $(shell grep 'setq zero-version' zero-framework.el | cut -d'"' -f2)

default: dist
#===============
# multiple file
#===============
check:
	emacs -Q --batch -l zero-reload-all.el -f zero-rebuild -l zero-table.el -l zero-table-test.el -f ert-run-tests-batch
zip:
	git archive -o zero-el-$(VERSION).zip --prefix=zero/ HEAD
#==========================
# single file distribution
#==========================
dist: dist-check
build:
	if [ ! -x ~/.local/bin/pytest ]; then python3 -m pip install --user pytest; fi
	~/.local/bin/pytest build.py
	./build.py
	sed -i "s/PKG_VERSION/$(VERSION)/g" zero.el
dist-check: build
	emacs -Q --batch -l ~/.emacs.d/elpa/s-1.11.0/s.el -l zero.el -l zero-panel-test.el -l zero-pinyin-service-test.el -l zero-framework-test.el -l zero-pinyin-test.el -l zero-table.el -l zero-table-test.el -f ert-run-tests-batch
#====================
# other make targets
#====================
install-git-hooks:
	rsync -air git-hooks/ .git/hooks/
version:
	@echo $(VERSION)
.PHONY: default check zip dist build dist-check install-git-hooks version
