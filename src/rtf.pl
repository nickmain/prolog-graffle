:- module( rtf, [rtf_to_plain/2]).

%%--Extract plain text out of RTF
rtf_to_plain( Rtf, Plain ) :- 
	atom_codes( Rtf, RtfString ),  
	rtf( R, RtfString, _ ), 
	extract_text( R, T ), 
	trim( T, T2 ),
	atom_codes( Plain, T2 ).  

rtf([]) --> skip_whitespace, eof, !.
rtf([]) --> skip_whitespace, "}", !.
rtf([Item|Items]) --> skip_whitespace, rtf_item(Item), rtf(Items), !.

rtf_item(ctrl(Item)) --> control_code(Item), !.
rtf_item(braces(Item)) --> brace_block(Item), !.
rtf_item(text(Item)) --> text(Item).

brace_block(Block) --> open_brace, skip_whitespace, rtf(Block).
control_code(Code) --> "\\", until_space_or_brace(Code).
text(Text)         --> until_backslash_or_brace(Text).

white_char(32).
white_char(10).
closing_brace(125).
backslash(92).

whitespace(W,[C|Rest],Result) :- white_char(C), whitespace(W2,Rest,Result), W = [C|W2], !.
whitespace("",Result,Result).

skip_whitespace --> (whitespace(_), !) ; {true, !}.

until_backslash_or_brace("",[],[]).
until_backslash_or_brace(T,[92, 10  |Rest],Result) :- until_backslash_or_brace( T2, Rest, Result ), T = [32|T2], !. % \n -> space
until_backslash_or_brace(T,[92, 92  |Rest],Result) :- until_backslash_or_brace( T2, Rest, Result ), T = [92|T2], !. %skip over "\\"
until_backslash_or_brace(T,[92, 125 |Rest],Result) :- until_backslash_or_brace( T2, Rest, Result ), T = [125|T2], !. %skip over "\\"
until_backslash_or_brace("",[C|Rest],[C|Rest]) :- ( closing_brace(C); backslash(C) ), !.
until_backslash_or_brace(T,[A|Rest],Result) :- until_backslash_or_brace( T2, Rest, Result ), T = [A|T2].

until_space_or_brace("",[],[]).
until_space_or_brace(T,[92, 125 |Rest],Result) :- until_space_or_brace( T2, Rest, Result ), T = [125|T2], !. %skip over "\\"
until_space_or_brace("",[C|Rest],[C|Rest]) :- ( white_char(C) ; closing_brace(C); backslash(C) ), !.
until_space_or_brace(T,[A|Rest],Result) :- until_space_or_brace( T2, Rest, Result ), T = [A|T2].

open_brace  --> "{".
close_brace --> "}".

eof([],[]).

trim( T, R ) :- trim_front( T, T2 ), trim_end( T2, R ).
trim_front( [C|R], Text ) :- white_char(C), trim_front( R, Text ), !.
trim_front( T, T ).
trim_end( [], [] ).
trim_end( [C], [] ) :- white_char(C), !.
trim_end( [A|R], [A|T] ) :- trim_end( R, T ).  

extract_text( text(Text), Text ).
extract_text( ctrl([123|Rest]), R ) :- append(["{",Rest," "],R), !. % handle escaped open brace
extract_text( ctrl(_), " " ).
extract_text( braces([ctrl("fonttbl")|_]), "" ) :- !.
extract_text( braces(Items), R ) :- extract_text( Items, R ).
extract_text( [A|R], B ) :- extract_text(A,A2), extract_text(R,C), append([A2,C],B).
extract_text( [], "" ).