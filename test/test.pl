% (c) 2016, David N. Main - see LICENSE file

:- use_module('src/plist').
:- use_module('src/graffle').

test :- 
    read_plist('test/test.graffle', Plist),
    plist_to_graffle(Plist, GrafModule),
	GrafModule:listing.

