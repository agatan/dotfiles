DOTPATH := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CANDIDATES := $(wildcard .??*) bin
EXCLUSIONS := .DS_Store .git $(wildcard .??*.swp)
DOTFILES := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

all:

list:
	@$(foreach val, $(DOTFILES), /bin/ls -dF $(val);)

deploy:
	@echo '==> Start to deploy dotfiles to home directory.'
	@echo ''
	@$(foreach val, $(DOTFILES), ln -sfnv $(abspath $(val)) $(HOME)/$(val);)

clean:
	@echo 'Remove dotfiles from home directory...'
	@-$(foreach val, $(DOTFILES), rm -vr $(HOME)/$(val);)

help:
	@echo "HOE"
