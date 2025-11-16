.PHONY: build clean

all: build

build:
	@echo "Building via org-publish..."
	@mkdir -p public
	@emacs --batch \
		--eval "(setq debug-on-error t)" \
		--load publish.el \
		2>&1 | tee build-publish.log

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@rm -f *.pdf *.tex *.aux *.log *.nav *.out *.snm *.toc *.vrb
	@rm -f build.log build-publish.log debug.log
	@rm -rf public/
	@echo "✓ Cleaned"
