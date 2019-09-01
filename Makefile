compile:
	emacs -Q --batch -l zero-reload-all.el -f zero-rebuild -l zero-table.el -f ert-run-tests-batch
zip:
	git archive -o zero-el.zip --prefix=zero/ HEAD
.PHONY: zip compile
