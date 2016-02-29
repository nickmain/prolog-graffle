% (c) 2016, David N. Main - see LICENSE file

:- module( rtf, [rtf_to_plain/2]).

% Extract plain text out of RTF
rtf_to_plain(RTF, Plain) :- 
    atom_codes(RTF, Codes),
    ( plain_text(Txt, [], Codes, _), ! ; Txt = [] ),
    atom_codes(Plain, Txt).

plain_text(A, B) --> skip_noise, text_chars(A, [32|A2]), plain_text(A2, B), ! ;
                     skip_noise, text_chars(A, B).

gather_char([A|T], T, [A|B], B).
text_char(A, B) --> "\\\\", {A = ["\\"|B]}, ! ; \+whitespace, \+"\\", gather_char(A, B).
text_chars(A, B) --> text_char(A, A2), text_chars(A2, B), ! ; text_char(A, B).

skip_noise --> skip_whitespace, skip_font_table, skip_control_codes, skip_whitespace.

font_table --> "\\fonttbl", skip_control_codes, skip_whitespace, text_chars(_, _), "}".
skip_font_table --> font_table, skip_whitespace, ! ; {true}.

control_code -->  "\\", control_code_chars.
control_code_char --> \+whitespace, eat_char.
control_code_chars --> control_code_char, control_code_chars, ! ; control_code_char.
control_codes --> control_code, skip_whitespace, skip_font_table, control_codes, ! ; control_code.
skip_control_codes --> control_codes, ! ; {true}.

whitespace --> " " ; "\t" ; "\n" ; "\r" ; "\\\n" ; "{" ; "}".
whitespaces --> whitespace, whitespaces, ! ; whitespace.
skip_whitespace --> whitespaces, ! ; {true}.

eat_char([A|B], B).