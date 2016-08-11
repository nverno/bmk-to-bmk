emacs ?= emacs
wget ?= wget

auto ?= bmk-to-bmk-autoloads.el
el = $(filter-out $(auto),$(wildcard *.el))
elc = $(el:.el=.elc)

elpa_dir=.emacs.d/elpa
auto_flags ?= \
	--eval "(let ((generated-autoload-file \
                      (expand-file-name (unmsys--file-name \"$@\"))) \
                      (wd (expand-file-name default-directory)) \
                      (backup-inhibited t) \
                      (default-directory (expand-file-name \"$(elpa_dir)\" \"~\"))) \
                   (normal-top-level-add-subdirs-to-load-path) \
                   (update-directory-autoloads wd))"

.PHONY: $(auto) clean distclean
all: compile $(auto)

compile: $(elc)
%.elc : %.el
	$(emacs) -batch -f batch-byte-compile $<

$(auto):
	$(emacs) -batch $(auto_flags)

README.md: el2markdown.el $(el)
	$(emacs) -batch -l $< $(el) -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~ *.elc *loaddefs.el *autoloads.el
