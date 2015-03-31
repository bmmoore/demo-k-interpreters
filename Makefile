PROGS=imp.out imp_split.out imp_hash.out imp_mutable.out imp-mlton Sum Imp sum-c
all: $(PROGS)
.PHONY: all clean distclean
clean :
	rm -f *.cmx *.cmi *.o *.hi
distclean : clean
	rm -f $(PROGS)
%.out : %.ml
	ocamlopt $^ -o $@
imp-mlton : imp.sml imp.mlb
	mlton -default-type int64 -output imp-mlton imp.mlb
sum-c : sum.c
	gcc -O sum.c -o sum-c
Imp : Imp.hs
	ghc --make -O2 Imp
# N.B. -fllvm cheats on Sum.hs
Sum : Sum.hs
	ghc --make -O2 Sum
