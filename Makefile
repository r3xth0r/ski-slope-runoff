SCRIPTS := $(shell find dev/plots/ -name '*.R' | sort -n)
PLOTS_DIR := plt
OUTPUT_FORMAT := png
CONFIG_FILE := dev/helper/config.R

all: plot

config:
	@sed -i.bak '1s|.*|file_format <- "$(OUTPUT_FORMAT)"|' $(CONFIG_FILE) && rm -f $(CONFIG_FILE).bak; \
	echo "INFO: Using file_format = $(OUTPUT_FORMAT)"

plot: config
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
	@echo "  OUTPUT_FORMAT=<format> make all - Specify the output format (e.g., png or pdf)"
