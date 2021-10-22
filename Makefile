# replace "-tag debug" with "-tag profile" to use gprof
FLAGS=\
	-use-ocamlfind \
	-tag debug -tag safe_string \
	-tag warn\(A-4-9+27-33-40-42-44-45-60\) \
	-tag warn_error\(A\) \
	-tag dtypes -tag annot \
	-use-menhir -menhir 'menhir --explain --table'

all: sparse

sparse:
	@echo "        [OCAMLBUILD] $@"
	@ocamlbuild $(INCLUDES) $(FLAGS) sparse.native
	mv -f sparse.native sparse

clean:
	rm -f sparse
	rm -rf _build

.PHONY: all clean sparse
