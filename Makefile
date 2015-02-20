
.PHONY: all
all:

.PHONY: clean
clean:
	@find -maxdepth 1 -type f \( -name \*.o -o -name \*.hi \) -exec rm -v \{\} \;

.PHONY: install
install:
	nix-env --arg target "$$(cabal2nix .)" -f install.nix -i

.PHONY: install-env
install-env:
	nix-env -f env.nix -i
