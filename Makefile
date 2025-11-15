.phony: copy clean

CALIBRE_LIBRARY = "$(HOME)/Calibre Library"
OUT_DIR = ./out

clean:
	rm -rf raw  metadata.db generate.sh generate.sh~ out booklist.html
raw:
	mkdir -p raw
	find $(CALIBRE_LIBRARY) -name '*.epub' -print0 | xargs -0 -i cp {} raw/

metadata.db:
	cp $(CALIBRE_LIBRARY)/metadata.db .

generate.sh: metadata.db
	emacs -q --script book.el $(CALIBRE_LIBRARY) $(OUT_DIR)
	chmod +x generate.sh

booklist.html: metadata.db
	emacs -q --script book-html.el
