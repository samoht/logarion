OCB_FLAGS = -use-ocamlfind -I src # -I lib
OCB       = ocamlbuild $(OCB_FLAGS)
PKGS      = toml,uuidm,omd,str,batteries,lens,ptime,ptime.clock.os,re.str,lens.ppx_deriving
WEB_PKGS  = $(PKGS),opium.unix,tyxml,mustache
CMD_PKGS  = $(PKGS),cmdliner

all: cmd web

web:
	$(OCB) web.native -pkgs $(WEB_PKGS)
	mv web.native logarion-web

cmd:
	$(OCB) command.native -pkg $(CMD_PKGS)
	mv command.native logarion

style:
	sassc share/sass/main.sass > share/static/main.css

doc_html:
	$(OCB) doc/logarion.docdir/index.html -pkgs $(PKGS)

clean:
	$(OCB) -clean
	rm -f src/*.{cmx,cmi,o} *.{cmx,cmi,o}

.PHONY: web doc
