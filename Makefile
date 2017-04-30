
define newline


endef

shell-quote = '$(subst $(newline),'\$(newline)',$(subst ','\'',$(1)))'


ifdef nixpkgs
  override nixpkgs := $(call shell-quote,$(nixpkgs))
else
  nixpkgs = 'import <stockholm>'
endif

.PHONY: _default
_default: ghci

.PHONY: clean
clean:
	@find * -type f \( -name \*.o -o -name \*.hi \) -exec rm -v \{\} \;

.PHONY: ghci
ghci: shell.nix
	nix-shell --arg nixpkgs $(nixpkgs) --command 'exec ghci -Wall -fobject-code'

.PHONY: install
install:
	$(error to install run "make result && nix-env -i ./result")

.PHONY: result
result: shell.nix
	nix-build --arg nixpkgs $(nixpkgs) ./shell.nix

shell.nix: $(wildcard *.cabal)
	cabal2nix --shell . > $@
