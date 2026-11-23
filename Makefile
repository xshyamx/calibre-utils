.phony: clean optimize kill_optmize help
CALIBRE_LIBRARY = "$(HOME)/Calibre Library"
OUT_DIR = ./out

help:
	@echo Available targets
	@echo '================='
	@echo 'clean         - Removes all generated files'
	@echo 'metadata.db   - Copies the Calibre metadata database'
	@echo 'generate.sh   - Generates shell script to copy by series'
	@echo 'booklist.html - Generates html book list'


clean:
	rm -rf work metadata.db generate.sh generate.sh~ out booklist.html

metadata.db:
	cp $(CALIBRE_LIBRARY)/metadata.db .

generate.sh: metadata.db
	emacs -q --script book.el $(CALIBRE_LIBRARY) $(OUT_DIR)
	chmod +x generate.sh

booklist.html: metadata.db
	emacs -q --script book-html.el metadata.db

optimize: $(OUT_DIR)
	./reduce-book-size.sh

kill_optimize:
	ps -ef | awk '/reduce-book-size.sh/ {print $$2}' | head -1 | xargs kill

include Makefile.local
