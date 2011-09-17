%:- module(test,[tst/1]).

:- use_module(graffle).

test :- 
	graffle_to_file('../test/test.graffle').

