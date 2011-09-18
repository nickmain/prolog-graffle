:- module( graffle, [graffle_to_file/1] ).

:- use_module(plist).
:- use_module(util).
:- use_module(rtf).

% Read the graffle file and write a Prolog file with '.pl' appended
graffle_to_file( File ) :-
    append_atoms( [File, '.pl'], PLFile ),
    open( PLFile, write, Out ),
    ( graffle_to_file( File, Out ) ; writeln('graffle_to_file/2 failed') ),
    close( Out ), !.
    
% Process an open OmniGraffle file and write corresponding terms to the stream
graffle_to_file( File, Out ) :-
    read_plist(File,Plist), !,
    graffle_to_list(File,Plist,TermList),
    list_out(Out,TermList).

% Read a PList and generate a list of terms representing the contents
graffle_to_list( Filename, Plist, TermList ) :-
    gensym(og,DocId),
    doc_info( DocId, Plist, DocInfo ),
    canvases( DocId, Plist, Canvases ),
    TermList = [ graf_document(Filename,DocId) , DocInfo, Canvases ].
    
% get a list of the canvases
canvases( DocId, Plist, TermList ) :-
    ( get_assoc( 'Sheets', Plist, Sheets ) ; Sheets = [Plist] ), !,
    canvases( DocId, Plist, [], Sheets, TermList ).
    
canvases( _, _, Results, [], Results ).
canvases( DocId, Plist, TermList, [Canvas|Rest], Results ) :- 
    get_assoc( 'UniqueID', Canvas, Id ),
    get_assoc( 'SheetTitle', Canvas, Title ),
    
	(( get_assocs( ['BackgroundGraphic','Notes'], Canvas, NotesRTF ),
	   rtf_to_plain( NotesRTF, Notes )) ; Notes = empty(notes) ), !,

    get_assoc( 'GraphicsList', Canvas, Gs ), 

    CanvasId = DocId:Id,
    CanvasTerm = graf_canvas(CanvasId,Title,Notes),
    graphics( DocId, canvas(CanvasId), Gs, [], Graphics ),
    Terms = [ CanvasTerm, Graphics | TermList ],
    canvases( DocId, Plist, Terms, Rest, Results ).

% convert graphics to terms
graphics( _, _, [], Results, Results ).
graphics( DocId, ParentId, [Graphic|Rest], TermList, Results ) :-
    get_assoc( 'ID', Graphic, Id ),
    (( get_assoc( 'Notes', Graphic, NotesRTF ),
       rtf_to_plain( NotesRTF, Notes )) ; Notes = empty(notes) ), !,
    get_assoc( 'Class', Graphic, Class ),
    
    GraphicId = DocId:Id,
    GraphicTerm = graf_graphic(ParentId,GraphicId,Class,Notes),

    details( DocId, GraphicId, Class, TermList, Graphic, Details ),

    %subgraph
    %graphics
    %rows
    
    Terms = [ GraphicTerm | Details ],
    graphics( DocId, ParentId, Rest, Terms, Results ).
    
% type-specific graphic details
details( DocId, Id, 'ShapedGraphic', TermList, G, Results ) :-
    ((  get_assocs( ['Text','Text'], G, T ), 
		rtf_to_plain( T, Text ) ) ; 
      Text = empty(text) ),
    !,
    bounds( G, Bounds ),
    get_assoc( 'Shape', G, Shape ),
    dashed( G, Dashed ),
    
    % line labels
    (( get_assocs( ['Line','ID'], G, LineId ),
       get_assocs( ['Line','Position'], G, Posn ), 
       Line = label_for(DocId:LineId,position(Posn))) ;
     Line = not_label ), !,
    
    % connector shapes
	( (get_assoc('isConnectedShape', G, _),
	  (( get_assocs( ['Head','ID'], G, Hid ), Head = DocId:Hid ) ; Head = none ),
	  (( get_assocs( ['Tail','ID'], G, Tid ), Tail = DocId:Tid ) ; Tail = none ),
	  TList = [graf_connection(Id,Head,Tail) | TermList ]) ; 
	 TList = TermList ), !,
    
    Results = [ graf_shape(Id,Shape,Dashed,Line),
                graf_bounds(Id,Bounds),
                graf_text(Id,Text) |
                TList ].
    
details( DocId, Id, 'LineGraphic', TermList, G, Results ) :-
    ( get_assocs( ['Style','stroke','HeadArrow'], G, HeadArrow ) ; HeadArrow = none ), !,
    ( get_assocs( ['Style','stroke','TailArrow'], G, TailArrow ) ; TailArrow = none ), !,
    (( get_assocs( ['Head','ID'], G, Hid ), Head = DocId:Hid ) ; Head = none ), !,
    (( get_assocs( ['Tail','ID'], G, Tid ), Tail = DocId:Tid ) ; Tail = none ), !,
    
    dashed( G, Dashed ),    

    Results = [ graf_line(Id,HeadArrow,TailArrow,Dashed),
                graf_connection(Id,Head,Tail) |
                TermList ].

details( DocId, Id, 'Group', TermList, G, Results ) :-
    get_assoc( 'Graphics', G, Gs ),

    (( get_assoc( isSubgraph, G, _ ), 
       subgraph_backshape( Gs, BackG ),
       get_assoc( 'ID', BackG, BackId ),
       SubG = subgraph(DocId:BackId) ) ; 
    
    SubG = not_subgraph ), !,

	graphics( DocId, Id, Gs, TermList, TList ),

    Results = [ graf_group(Id,SubG) | TList ].

subgraph_backshape( [Shape], Shape ) :- !.
subgraph_backshape( [_|Rest], Shape ) :- subgraph_backshape( Rest, Shape ).

%non sub-graph
group_details( bool(false), Out, Id, G, Gs ) :-  
    graphics( Out, Id, Gs ).

g_details( 'TableGroup', G, Details ) :- Details = table(todo).
    
details( _, _, _, _ ).    
    
% whether graphic is dashed or solid
dashed( G, DS ) :-
    ( ( get_assocs( ['Style','stroke','Pattern'], G, _ ),
        DS = dashed ) ;
      DS = solid ),
    !.

% parse bounds
bounds( G, Bounds ) :-
    get_assoc( 'Bounds', G, Bs ),
    atom_to_term( Bs, {{X,Y},{W,H}}, _ ),
    Bounds = bounds(X,Y,W,H).

% Write document info
doc_info( DocId, Plist, TermList ) :-
    get_assoc( 'UserInfo', Plist, U ),
    assoc_to_list( U, KVs ),
    info_item( DocId, KVs, [], TermList ),
    !.
doc_info( _, _, [] ).

info_item( DocId, [], TermList, TermList ).
info_item( DocId, [K-V|Rest], TermList, Result ) :-
    info_key( K, Predicate ),
    info_item_value( DocId, Predicate, V, TermList, Terms ),
    info_item( DocId, Rest, Terms, Result ).
    
info_item_value(_, _, [], TermList, TermList ).
info_item_value( DocId, Predicate, [V|Vs], TermList, Result ) :- 
    Term =.. [Predicate, DocId, V],
    Terms = [Term|TermList],
    info_item_value( DocId, Predicate, Vs, Terms, Result ).
info_item_value( DocId, Predicate, Value, TermList, Result ) :- 
    Term =.. [Predicate, DocId, Value],
    Result = [Term|TermList].  
    
info_key( kMDItemDescription, graf_description ).
info_key( kMDItemComments, graf_comments ).
info_key( kMDItemAuthors, graf_author ).
info_key( kMDItemCopyright, graf_copyright ).
info_key( kMDItemProjects, graf_project ).
info_key( kMDItemLanguages, graf_language ).
info_key( kMDItemKeywords, graf_keyword ).
info_key( kMDItemOrganizations, graf_organization ).
info_key( kMDItemSubject, graf_subject ).
info_key( kMDItemVersion, graf_version ).
