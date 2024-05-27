VERSION := $(shell grep 'setq zero-input-version' zero-input-framework.el | cut -d'"' -f2)
EMACS := emacs
S_EL := ~/.emacs.d/elpa/s-20220902.1511/s.el
POSFRAME_EL := ~/.emacs.d/elpa/posframe-1.4.3/posframe.el

default: dist
#===============
# multiple file
#===============
check:
	$(EMACS) -Q --batch \
		-l zero-input-reload-all.el -f zero-input-rebuild \
		-l zero-input-table.el \
		-l zero-input-table-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -Q --batch \
		-l $(S_EL) \
		-l $(POSFRAME_EL) \
		-l ./byte-compile-flags.el \
		-l ./zero-input.el \
		--eval='(byte-compile-file "zero-input-panel-posframe.el")'
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
dist-check: build
	@echo "testing byte-compile is clean..."
	$(EMACS) -Q --batch -l $(S_EL) -l ./byte-compile-flags.el --eval='(byte-compile-file "zero-input.el")'
	$(EMACS) -Q --batch -l $(S_EL) -l $(POSFRAME_EL) -l ./byte-compile-flags.el -l ./zero-input.el --eval='(byte-compile-file "zero-input-panel-posframe.el")'
	@echo "running unit tests..."
	$(EMACS) -Q --batch -l $(S_EL) -l $(POSFRAME_EL) \
		-l zero-input.el \
		-l zero-input-panel-posframe.el \
		-l zero-input-panel-test.el \
		-l zero-input-pinyin-service-test.el \
		-l zero-input-framework-test.el \
		-l zero-input-pinyin-test.el \
		-l zero-input-table.el \
		-l zero-input-table-test.el \
		-l zero-input-panel-minibuffer-test.el \
		-f ert-run-tests-batch-and-exit
#====================
# other make targets
#====================
install-git-hooks:
	rsync -air git-hooks/ .git/hooks/
version:
	@echo $(VERSION)
.PHONY: default check zip dist build dist-check install-git-hooks version
