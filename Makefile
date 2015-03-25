PROGS=imp.out imp_split.out imp_hash.out imp_mutable.out sum-c
all: $(PROGS)
.PHONY: all clean distclean
clean :
	rm -f *.cmx *.cmi *.o
distclean : clean
	rm -f $(PROGS)
%.out : %.ml
	ocamlopt $^ -o $@
sum-c : sum.c
	gcc -O sum.c -o sum-c
