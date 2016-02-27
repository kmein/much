.PHONY: ghci
ghci: shell.nix
	nix-shell --arg nixpkgs 'import <stockholm>' --command 'exec ghci -Wall'

shell.nix: $(wildcard *.cabal)
	cabal2nix --shell . > $@

.PHONY: clean
clean:
	@find * -type f \( -name \*.o -o -name \*.hi \) -exec rm -v \{\} \;

.PHONY: install
install:
	nix-env --arg target "$$(cabal2nix .)" -f install.nix -i

.PHONY: install-env
install-env:
	nix-env -f env.nix -i
