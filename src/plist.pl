:- module( plist, [read_plist/2]).

%% Read a PLIST file and return <assoc>
read_plist( File, Plist ) :- 
	parse_plist(File,XML),
	[element(plist,_,[Dict])] = XML,
	elem(Dict,Plist).

%% Get the Apple PLIST DTD
apple_plist_dtd(DTD) :- 
	catch( dtd(plist,DTD), _, 
           ( new_dtd(plist,DTD),
             load_dtd(DTD,'plist.dtd',[dialect(xml)]) )).

%% Parse PLIST XML file
parse_plist( File, XML ) :- 
	apple_plist_dtd(Plist),
    load_structure(File,XML,[dialect(xml),dtd(Plist)]).

%% Process PLIST elements
elem(element(Tag,_,Kids),E) :- Elem =.. [Tag,Kids], elem(Elem,E). % unwrap elements - don't care about attrs
elem(array(Kids),E) :- maplist( elem, Kids, E ).
elem(integer([Value]),E) :- atom_number(Value,E).
elem(real([Value]),E) :- atom_number(Value,E).
elem(string([E]),E).
elem(string([]),empty(string)).
elem(true(_),bool(true)).
elem(false(_),bool(false)).

elem(dict(Kids),E) :-
	dict_entries( Kids, Pairs ),
	list_to_assoc( Pairs, E ).

elem(A,unimplemented(A)).

%% read key-value pairs into a list
dict_entries([K,V |R], Pairs ) :-
	element(key,_,[Key]) = K,
	elem(V,Value),
	dict_entries(R,Rest),
	Pairs = [(Key - Value) | Rest ].
dict_entries( _, [] ).