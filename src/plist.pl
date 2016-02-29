% (c) 2016, David N. Main - see LICENSE file

:- module(plist, [read_plist/2]).

% Read a PLIST file and return a dictionary term
read_plist(FileName, Plist) :- 
    load_xml(FileName, XML, []),
    only_elements(XML, [Doc]),
    elem(Doc, Plist).

% Filter a list of nodes to leave only elements (we do want whitespace in text)
only_elements(Nodes, Elements) :- include(is_element, Nodes, Elements).
is_element(element(_,_,_)). 

% Process PLIST elements
elem(element(plist, _, Body), E) :- only_elements(Body, [Root]), elem(Root, E), !.
elem(element(integer, _, [Value]), E) :- atom_number(Value, E), !.
elem(element(real, _, [Value]), E) :- atom_number(Value, E), !.
elem(element(string, _, [E]), E) :- !.
elem(element(string, _, []), empty(string)) :- !.
elem(element(true, _, _), bool(true)) :- !.
elem(element(false, _, _), bool(false)) :- !.

% arrays -> lists
elem(element(array, _, Kids), E) :-
    only_elements(Kids, Elems),
    maplist(elem, Elems, E), !.

% dicts -> new style dicts
elem(element(dict, _, Kids), E) :- 
    only_elements(Kids, Elems),
    dict_entries( Elems, Pairs ),
    dict_pairs(E, dict, Pairs), !.

% unrecognized plist types
elem(element(A, _, _), unimplemented(A)).

% read key-value pairs into a list
dict_entries([K, V | R], Pairs ) :-
    element(key, _, [Key]) = K,
    elem(V, Value),
    dict_entries(R, Rest),
    Pairs = [(Key - Value) | Rest ], !.
dict_entries( _, [] ).