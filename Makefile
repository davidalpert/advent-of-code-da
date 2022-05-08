PROJECTNAME=$(shell basename "$(PWD)")

SLN=advent-of-code.sln

# ---------------------- targets -------------------------------------

.PHONY: default
default: help

.PHONY: clean
clean: ## clean build output
	echo "tell me what to clean"

restore: ## restore packages
	dotnet restore $(SLN)

build: restore ## builds something
	dotnet build $(SLN)

test: restore ## run tests
	dotnet test $(SLN)

test-watch: restore ## run tests and watch
	dotnet watch test --project 2020/2020.fsproj

.PHONY: help
help: Makefile
	@echo
	@echo " $(PROJECTNAME) - available targets:"
	@echo
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
	@echo
