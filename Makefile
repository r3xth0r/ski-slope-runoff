SCRIPTS := $(shell find dev/plots/ -name '*.R' | sort -n)
PLOTS_DIR := plt

all: plot

plot:
	@for script in $(SCRIPTS); do \
		echo "$$(date '+%Y-%m-%d %H:%M:%S') » Running $$script"; \
		Rscript $$script; \
		echo ""; \
	done

clean:
	rm -f $(PLOTS_DIR)/*
	
help:
	@echo "Available targets:"
	@echo "  all      - Default, runs *plot* target"
	@echo "  plot     - Run all .R scripts in dev/plots/"
	@echo "  clean    - Remove all files in the plt/ directory"
