OCB_FLAGS = -use-ocamlfind -I src # -I lib
OCB       = ocamlbuild $(OCB_FLAGS)
PKGS      = toml,opium.unix,omd,str,batteries,tyxml,lens,ptime,ptime.clock.os,re.str,lens.ppx_deriving,mustache

all: web

web:
	$(OCB) web.native -pkgs $(PKGS)
	mv web.native web

doc_html:
	$(OCB) doc/logarion.docdir/index.html -pkgs $(PKGS)

clean:
	$(OCB) -clean
	rm -f src/*.{cmx,cmi,o} *.{cmx,cmi,o}

.PHONY: web doc
