build:
	@rm result ||:
	nix-build release0.nix

make doc:
	cabal new-haddock --enable-documentation

make cbuild:
	cabal new-configure && cabal new-build && hie-executable-build-fix

# run: build
# 	result/bin/<your_executable>

repl:
	nix-shell -p haskellPackages.cabal-install --run \
		"cabal new-repl lib:genetic-algorithm"

clean:
	rm -rf dist*

shell:
	nix-shell -p haskellPackages.cabal-install haskellPackages.hoogle haskellPackages.hlint ghc

shell-pure:
	nix-shell --pure shell.nix 

external-shell:
	nix-shell external.nix

generate-nix:
	cabal2nix . > default.nix
	cabal2nix --shell . > shell.nix

.PHONY: build run repl shell shell-pure external-shell doc cbuild generate-nix
