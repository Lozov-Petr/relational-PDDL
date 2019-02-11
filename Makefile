pddl:
	ocamlbuild -use-ocamlfind -I src run.native
	mv run.native pddl.native

clear:
	$(RM) -r _build *.native
