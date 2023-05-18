#  cl-test - An emacs interface to cl-test
#  Copyright (C) 2022  M E Leypold
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

# * Standard targets -----------------------------------------------------------

all::
clean::
cleaner::

.PHONY: @always

-include Config.mk

# * Shell ----------------------------------------------------------------------

export PS4 = =>

.ONESHELL:

ifdef MAKE_RESTARTS
$(info $(MAKE_RESTARTS:%=Restarting: %))
endif

clean::
	rm -f *~
	rm -rf .build

# * Parameters -----------------------------------------------------------------

SHORT-NAME      ?= $(shell echo "$(notdir $(CURDIR))" | sed 's|_.*||')
SHORT-NAME      := $(strip $(SHORT-NAME))
DISTRIBUTABLES  ?= *.el \
		   README.md README NOTES.md NOTES \
                   $(MORE-DISTRIBUTABLES)
DONT-DISTRIBUTE ?= HEADER% TODO% test-%.el install.el

$(info SHORT-NAME      = $(SHORT-NAME))

DATE           := $(shell date -Is | sed 's|[+].*||;s|[:-]||g;s|T||')

# * Version --------------------------------------------------------------------


ifndef RELEASE
  CURRENT-VERSION := $(shell ./git-version.sh)
  -include .VERSION.mk

  ifeq ($(CURRENT-VERSION),$(VERSION))
    .VERSION:
	    echo $(CURRENT-VERSION) >$@
  else
    .VERSION: @always
	    echo $(CURRENT-VERSION) >$@
  endif

  .VERSION.mk: .VERSION
	  set -eux
	  echo "VERSION = $$(cat "$<")" >$@
else
  VERSION = $(RELEASE)
  CURRENT-VERSION = $(RELEASE)
endif


EMACS-PACKAGE-VERSION = $(subst -,snapshot,$(subst +,snapshot,$(VERSION)))

$(info CURRENT-VERSION       = $(CURRENT-VERSION))
$(info VERSION               = $(VERSION))
$(info EMACS-PACKAGE-VERSION = $(EMACS-PACKAGE-VERSION))

clean::
	set -eux
	rm -f .VERSION.mk .VERSION

cleaner:: clean
	rm -f VERSION

# * Package file ---------------------------------------------------------------

$(SHORT-NAME)-pkg.$(VERSION): $(SHORT-NAME)-pkg.el.t
	sed <$< 's|__VERSION__|$(EMACS-PACKAGE-VERSION)|' >"$@"

$(SHORT-NAME)-pkg.el: $(SHORT-NAME)-pkg.$(VERSION)
	cp $< $@

all:: $(SHORT-NAME)-pkg.el

cleaner:: clean
	rm -f $(SHORT-NAME)-pkg.el $(SHORT-NAME)-pkg.[0-9]*

# * Package --------------------------------------------------------------------


PACKAGE-NAME   = $(SHORT-NAME)-$(subst +,+DIRTY-,$(VERSION))
PACKAGE-PATH   = .build/$(PACKAGE-NAME).tar
PACKAGE-TOPDIR = $(SHORT-NAME)-$(EMACS-PACKAGE-VERSION)
PACKAGED-FILES = $(filter-out $(DONT-DISTRIBUTE),$(wildcard $(DISTRIBUTABLES))) \
                 $(SHORT-NAME)-pkg.el

$(info PACKAGE-NAME    = $(PACKAGE-NAME))
$(info PACKAGE-TOPDIR  = $(PACKAGE-TOPDIR))
$(info PACKAGED-FILES  = $(PACKAGED-FILES))

$(PACKAGE-PATH): $(PACKAGED-FILES)
	set -eux
	rm -rf .build/$(PACKAGE-TOPDIR)
	mkdir  -p .build/$(PACKAGE-TOPDIR)
	cp $(PACKAGED-FILES) .build/$(PACKAGE-TOPDIR)
	mkdir -p "$(@D)"
	tar -C .build -cvf "$@" $(PACKAGE-TOPDIR)
	: $$(ls -1 .build/*.tar | tail -1)

package: $(PACKAGE-PATH)
	ln -sf $(PACKAGE-NAME).tar .build/PACKAGE.tar

# * Installation ---------------------------------------------------------------

install-directly:
	emacs --batch -u $$USER \
              --eval '(push "$(CURDIR)" load-path)' \
              -l $(SHORT-NAME)-dev.el -f $(SHORT-NAME)-install

install-from-package: package
	set -eux
	emacs --batch -l "./install.el"

ifeq ($(sort $(filter-out %.el,$(PACKAGED-FILES))),)
  install:: install-directly
else
  install:: install-from-package
endif



# * Release --------------------------------------------------------------------

ifdef RELEASE

VERSION:
	echo $(CURRENT-VERSION) >$@


release: $(SHORT-NAME)-pkg.el VERSION
	set -eux
	git add $^
	git add -u
	git commit -m 'Releasing $(RELEASE)'
	git tag $(RELEASE)
	rm -f $^
	git add -u
	git commit -m 'Starting development after $(RELEASE)'
endif
