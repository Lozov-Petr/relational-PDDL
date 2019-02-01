pddl:
	ocamlbuild -use-ocamlfind -I src run.native
	mv run.native dppl.native

clear:
	$(RM) -r _build *.native
