:- module( util, [append_atoms/2,all_but_last/3,out/2,out/3,list_out/2,get_assocs/3]).

get_assocs( [], Value, Value ).
get_assocs( [Key|Keys], Map, Value ) :- get_assoc( Key, Map, V ), get_assocs( Keys, V, Value ).

% append a number of atoms to make a new one
append_atoms( Atoms, Atom ) :-
    maplist( atom_chars, Atoms, AtomChars ),
    append( AtomChars, Chars ),
    atom_chars( Atom, Chars ).

% Write a period terminated term and newline
out( Out, Term ) :-
    write_term( Out, Term, [quoted(true),spacing(next_argument)] ),
    write( Out, '.' ),
    nl( Out ).
out( Out, Func, Args ) :-
    TList = [Func|Args],
    Term =.. TList,
    out( Out, Term ).
    
list_out( Out, [] ).
list_out( Out, [Term|Rest] ) :- 
    ( is_list(Term) -> 
        list_out( Out, Term ) ;
        out( Out, Term ) ), 
    list_out( Out, Rest ).
    
% Split list into last element and the list preceding it
all_but_last( [], [], _ ). 
all_but_last( A, Front, Last ) :- all_but_last( A, [], Front, Last ).

all_but_last( [A], B, Front, A ) :- reverse(B, Front), !.
all_but_last( [A|B], C, Front, Last ) :- all_but_last( B, [A|C], Front, Last ).

