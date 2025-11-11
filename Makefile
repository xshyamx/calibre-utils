.phony: copy clean

CALIBRE_LIBRARY = "$(HOME)/Calibre Library"

clean:
	rm -rf raw  metadata.db generate.sh generate.sh~ out booklist.html
raw:
	mkdir -p raw

metadata.db:
	cp $(CALIBRE_LIBRARY)/metadata.db .

copy: raw
	find $(CALIBRE_LIBRARY) -name '*.epub' -print0 | xargs -0 -i cp {} raw/

generate.sh: metadata.db
	emacs -q --script book.el
	chmod +x generate.sh

booklist.html: metadata.db
	emacs -q --script book-html.el
