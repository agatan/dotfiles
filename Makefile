DOTPATH := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CANDIDATES := $(wildcard .??*) bin
EXCLUSIONS := .DS_Store .git $(wildcard .??*.swp)
DOTFILES := $(filter-out $(EXCLUSIONS), $(CANDIDATES))
CONFIG_FILES := $(wildcard config/??*)
TARGETS := $(DOTFILES) $(CONFIG_FILES)

list:
	@$(foreach val, $(TARGETS), echo $(val);)

deploy:
	@echo '==> Start to deploy dotfiles to home directory.'
	@echo ''
	@$(foreach val, $(DOTFILES), ln -sfnv $(abspath $(val)) $(HOME)/$(val);)
	@echo '==> Start to deploy .config files to home directory.'
	@echo ''
	@$(foreach val, $(CONFIG_FILES), ln -sfnv $(abspath $(val)) $(HOME)/$(subst config,.config,$(val));)


clean:
	@echo 'Remove dotfiles from home directory...'
	@-$(foreach val, $(TARGETS), rm -vr $(HOME)/$(val);)

