## Makefile

# Copyright (C) 2019  Naoya Yamashita

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

all:

REPO_USER    := conao3
PACKAGE_NAME := dired-git
REPO_NAME    := dired-git.el

EMACS        ?= emacs
ELS          := $(shell cask files)

GIT_HOOKS    := pre-commit

##################################################

.PHONY: all git-hook help build test clean

all: git-hook help

git-hook: $(GIT_HOOKS:%=.git/hooks/%)

.git/hooks/%: git-hooks/%
	cp -a $< $@

help:
	$(info )
	$(info Commands)
	$(info ========)
	$(info   - make          # Install git-hook to your local .git folder)
	$(info   - make build    # Compile Elisp files)
	$(info   - make test     # Compile Elisp files and test $(PACKAGE_NAME))
	$(info )
	$(info Cleaning)
	$(info ========)
	$(info   - make clean    # Clean compiled files)
	$(info )
	$(info This Makefile required `cask`)
	$(info See https://github.com/$(REPO_USER)/$(REPO_NAME)#contribution)
	$(info )

##############################

%.elc: %.el .cask
	cask exec $(EMACS) -Q --batch -f batch-byte-compile $<

.cask: Cask
	cask install
	touch $@

##############################

build: $(ELS:%.el=%.elc)

test: build
	cask exec buttercup -L .

clean:
	rm -rf $(ELS:%.el=%.elc) .cask
