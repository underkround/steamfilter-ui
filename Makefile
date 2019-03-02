SOURCES_ELM=$(shell find src -name '*.elm')

NIX_BRANCH=nixos-18.09

.PHONY: shell
shell:
	nix-shell

.PHONY: format
format:
	elm-format --yes src

.PHONY: watch
watch:
	./node_modules/.bin/webpack-serve

build: $(SOURCES_ELM) Makefile webpack.config.js node_modules
	mkdir -p build
	./node_modules/.bin/webpack --progress --colors --bail
	touch build

.PHONY: clean
clean: clean
	rm -fr build
	rm -fr build-watch

deploy: build
	aws --profile=steamfilter s3 sync --delete build s3://steamfilter.net

# Plumbing

.PHONY: nix-%
nix-%:
	@echo "run inside nix-shell: $*"
	nix-shell --pure --run "$(MAKE) $*"

node_modules: package.json package-lock.json
	npm ci
	touch node_modules

# Upgrade to the latest commit of the selected nixpkgs branch
.PHONY: upgrade
upgrade: NIX_FILE=shell.nix
upgrade:
	@echo "Updating nixpkgs from branch: $(NIX_BRANCH)"; \
	set -e pipefail; \
	rev=$$(curl https://api.github.com/repos/NixOS/nixpkgs-channels/branches/$(NIX_BRANCH) | jq -er .commit.sha); \
	echo "Updating nixpkgs to hash: $$rev"; \
	sha=$$(nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs-channels/archive/$$rev.tar.gz); \
	sed -i \
		-e "2s|.*|    # $(NIX_BRANCH)|" \
		-e "3s|.*|    url = \"https://github.com/NixOS/nixpkgs-channels/archive/$$rev.tar.gz\";|" \
		-e "4s|.*|    sha256 = \"$$sha\";|" \
		$(NIX_FILE)
