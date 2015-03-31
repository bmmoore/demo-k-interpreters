PROGS=sum.out imp.out imp_split.out imp_hash.out imp_mutable.out imp-mlton imp2-mlton imp3-mlton Sum Imp sum-c
all: $(PROGS)
.PHONY: all clean distclean
clean :
	rm -f *.cmx *.cmi *.o *.hi
distclean : clean
	rm -f $(PROGS)
%.out : %.ml
	ocamlopt $^ -o $@
imp-mlton : maps.sml imp_eval.sml
imp2-mlton : maps.sml
imp3-mlton : maps.sml
%-mlton : %.mlb %.sml
	mlton -default-type int64 -output $@ $<
sum-c : sum.c
	gcc -O sum.c -o sum-c
Imp : Imp.hs
	ghc --make -O2 Imp
# N.B. -fllvm cheats on Sum.hs
Sum : Sum.hs
	ghc --make -O2 Sum
