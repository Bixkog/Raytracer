
.PHONY: all clean

OCB_FLAGS = -I common -I object -I scene -I reader -I renderer -tag bin_annot 
OCB = ocamlbuild $(OCB_FLAGS) -use-ocamlfind -pkg yojson


all:
	$(OCB) main.native

debug:
	$(OCB) main.native -cflag -g

clean:
	$(OCB) -clean

%.native :
	$(OCB) $@
