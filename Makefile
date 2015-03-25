PROGS=imp.out imp_split.out imp_hash.out imp_mutable.out imp-mlton sum-c
all: $(PROGS)
.PHONY: all clean distclean
clean :
	rm -f *.cmx *.cmi *.o
distclean : clean
	rm -f $(PROGS)
%.out : %.ml
	ocamlopt $^ -o $@
imp-mlton : imp.sml imp.mlb
	mlton -default-type int64 -output imp-mlton imp.mlb
sum-c : sum.c
	gcc -O sum.c -o sum-c
