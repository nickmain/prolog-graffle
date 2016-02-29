:- module( util, [
    sub_dict/3, each_sub_dict/3, dict_get/4, dict_get/5,
    set/4, get/4
]).

% get a value from the context dict - fail if missing - and use the value as the new context dict 
sub_dict(Key, In, Out) :-
    Dict = In.get(dict),
    Value = Dict.get(Key),
    Out = In.put(dict, Value).

% use each array value as the context dict
each_sub_dict(Array, In, Out) :-
    member(M, Array),
    Out = In.put(dict, M).

% get a value from the context dict (key or list of sub-keys) - fail if missing
dict_get(Key, Value, Ctxt, Ctxt) :-
    Dict = Ctxt.get(dict),
    dict_chase(Dict, Key, Value).

dict_chase(Dict, [Key], Value) :- !, Value = Dict.get(Key).
dict_chase(Dict, [Key|Keys], Value) :- !, Dict2 = Dict.get(Key), dict_chase(Dict2, Keys, Value).
dict_chase(Dict, Key, Value) :- Value = Dict.get(Key).
    
% get a value from the context dict - use Default if missing
dict_get(Key, Value, Default, Ctxt, Ctxt) :-
    Dict = Ctxt.get(dict),
    dict_chase(Dict, Key, Value),
    ! ; Value = Default.
    
% set a context value
set(Key, Value, In, Out) :-
    Out = In.put(Key, Value).
    
% get a context value
get(Key, Value, Ctxt, Ctxt) :-
    Value = Ctxt.get(Key).
